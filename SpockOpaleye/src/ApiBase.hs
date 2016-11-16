{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module ApiBase where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Profunctor.Product.Default as D
import           Data.Time                       (UTCTime, getCurrentTime)
import           Database.PostgreSQL.Simple
import           DataTypes
import           Opaleye
import           GHC.Int
import           Prelude                         hiding (id)


auditLog :: String -> AppM ()
auditLog = tell 

createDbRows :: (Show columnsW, D.Default QueryRunner columnsR haskells) 
    =>  Connection -> Table columnsW columnsR -> [columnsW] -> AppM [haskells]
createDbRows conn table pgrows = do
  auditLog $ "Create : " ++ (show pgrows)
  liftIO $ runInsertManyReturning conn table pgrows (\x -> x)

updateDbRow :: (Show columnsW, HasId columnsR (Column PGInt4)) => Connection -> Table columnsW columnsR -> Column PGInt4 -> columnsW -> AppM columnsW
updateDbRow conn table row_id item = do
  auditLog $ "Update :" ++ (show item)
  _ <- liftIO $ runUpdate conn table (\_ -> item) (matchFunc row_id) 
  return item
  where
    matchFunc :: (HasId cmR (Column PGInt4)) => (Column PGInt4 -> cmR -> Column PGBool)
    matchFunc itId item' = (item' ^. id) .== itId
  
createRow ::(
    Show incoming,
    Show columnsW,
    HasCreatedat columnsW (Maybe (Column PGTimestamptz)),
    HasUpdatedat columnsW (Column PGTimestamptz),
    D.Default Constant incoming columnsW, D.Default QueryRunner returned row)
    => Connection -> Table columnsW returned -> incoming -> AppM row
createRow conn table item = do
  auditLog $ "Create : " ++ (show item)
  currentTime <- liftIO $ fmap pgUTCTime getCurrentTime
  let itemPg = (constant item) & createdat .~ (Just currentTime) & updatedat .~ (currentTime)
  fmap head $ createDbRows conn table [itemPg] 

updateRow :: (
    Show columnsW
    , Show haskells
    , HasUpdatedat haskells UTCTime
    , D.Default Constant haskells columnsW
    , D.Default Constant itemId (Column PGInt4)
    , HasId haskells itemId
    , HasId columnsR (Column PGInt4)
    )
    => Connection -> Table columnsW columnsR -> haskells -> AppM haskells
updateRow conn table item = do
  auditLog $ "Update : " ++ (show item)
  let itId = item ^. id
  currentTime <- liftIO getCurrentTime
  let updatedItem = (putUpdatedTimestamp currentTime) item
  _ <- updateDbRow conn table (constant itId) (constant updatedItem) 
  return updatedItem
  where
    putUpdatedTimestamp :: (HasUpdatedat item (UTCTime)) => UTCTime -> item -> item
    putUpdatedTimestamp timestamp  = updatedat .~ timestamp

removeRow :: (
      Show haskells
    , D.Default Constant itemId (Column PGInt4)
    , HasId haskells itemId
    , HasId columnsR (Column PGInt4)
    ) => Connection -> Table columnsW columnsR -> haskells -> AppM GHC.Int.Int64
removeRow conn table item = do
  auditLog $ "Remove : " ++ (show item)
  liftIO $ do
    runDelete conn table $ matchFunc $ item ^. id
  where
    matchFunc :: (
        HasId columnsR (Column PGInt4),
        D.Default Constant itemId (Column PGInt4)
        ) => (itemId -> columnsR -> Column PGBool)
    matchFunc itId item' = (item' ^. id) .== (constant itId)
