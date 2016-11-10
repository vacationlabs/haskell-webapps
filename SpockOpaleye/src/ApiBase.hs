{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module ApiBase where

import           Control.Lens
import qualified Data.Profunctor.Product.Default as D
import           Data.Time                       (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple
import           DataTypes
import           Opaleye
import           OpaleyeDef
import           Prelude                         hiding (id)

createRow ::(
    HasCreatedat columnsW (Maybe (Column PGTimestamptz)),
    HasUpdatedat columnsW (Column PGTimestamptz),
    D.Default Constant incoming columnsW, D.Default QueryRunner returned row)
    => Connection -> Table columnsW returned -> incoming -> IO row
createRow conn table item = do
  currentTime <- fmap pgUTCTime getCurrentTime
  let itemPg = (constant item) & createdat .~ (Just currentTime) & updatedat .~ (currentTime)
  fmap head $ runInsertManyReturning conn table [itemPg] (\x -> x)

updateRow :: (
    HasUpdatedat haskells UTCTime
    , D.Default Constant haskells columnsW
    , D.Default Constant itemId (Column PGInt4)
    , HasId haskells itemId
    , HasId columnsR (Column PGInt4)
    )
    => Connection -> Table columnsW columnsR -> haskells -> IO haskells
updateRow conn table item = do
  currentTime <- getCurrentTime
  let itId = item ^. id
  let updatedItem = (putUpdatedTimestamp currentTime) item
  runUpdate conn table (\_ -> constant updatedItem) (matchFunc itId)
  return updatedItem
  where
    putUpdatedTimestamp :: (HasUpdatedat item (UTCTime)) => UTCTime -> item -> item
    putUpdatedTimestamp timestamp  = updatedat .~ timestamp
    matchFunc :: (HasId cmR (Column PGInt4), D.Default Constant itemId (Column PGInt4)) => (itemId -> cmR -> Column PGBool)
    matchFunc itId item = (item ^. id) .== (constant itId)
