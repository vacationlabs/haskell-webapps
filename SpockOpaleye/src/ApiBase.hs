{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module ApiBase where

import qualified Data.Profunctor.Product.Default      as D
import           Data.Time                            (UTCTime, getCurrentTime)
import           Control.Lens
import           DataTypes
import           Database.PostgreSQL.Simple
import Opaleye
import OpaleyeDef
import Prelude hiding (id)
import OpaleyeTypes

create_row ::(
    HasCreatedat columnsW (Maybe (Column PGTimestamptz)),
    HasUpdatedat columnsW (Column PGTimestamptz),
    D.Default Constant incoming columnsW, D.Default QueryRunner returned row) 
    => Connection -> Table columnsW returned -> incoming -> IO row
create_row conn table item = do
  current_time <- fmap pgUTCTime getCurrentTime
  let itemPg = (constant item) & createdat .~ (Just current_time) & updatedat .~ (current_time)
  fmap head $ runInsertManyReturning conn table [itemPg] (\x -> x)

update_row :: (
    HasUpdatedat haskells UTCTime
    , D.Default Constant haskells columnsW
    , D.Default Constant item_id (Column PGInt4)
    , HasId haskells item_id
    , HasId columnsR (Column PGInt4)
    )
    => Connection -> Table columnsW columnsR -> item_id -> haskells -> IO haskells
update_row conn table it_id item = do
  current_time <- getCurrentTime
  let updated_item = (put_updated_timestamp current_time) item
  runUpdate conn table (\_ -> constant updated_item) match_func
  return updated_item
  where
    put_updated_timestamp :: (HasUpdatedat item (UTCTime)) => UTCTime -> item -> item
    put_updated_timestamp timestamp  = updatedat .~ timestamp
    match_func :: (HasId cmR (Column PGInt4)) => (cmR -> Column PGBool)
    match_func item = (item ^. id) .== (constant it_id)
