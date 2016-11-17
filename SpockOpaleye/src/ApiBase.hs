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
import           DataTypes
import           Opaleye
import           GHC.Int
import           Prelude                         hiding (id)


auditLog :: String -> AppM ()
auditLog = tell 

removeRawDbRows :: Table columnsW columnsR -> (columnsR -> Column PGBool) -> AppM GHC.Int.Int64
removeRawDbRows table matchFunc = do
  conn <- getConnection
  liftIO $ runDelete conn table matchFunc 

createDbRows :: (Show columnsW, D.Default QueryRunner columnsR haskells) 
    =>  Table columnsW columnsR -> [columnsW] -> AppM [haskells]
createDbRows table pgrows = do
  auditLog $ "Create : " ++ (show pgrows)
  conn <- getConnection
  liftIO $ runInsertManyReturning conn table pgrows (\x -> x)

updateDbRow :: (Show columnsW, HasId columnsR (Column PGInt4)) => Table columnsW columnsR -> Column PGInt4 -> columnsW -> AppM columnsW
updateDbRow table row_id item = do
  auditLog $ "Update :" ++ (show item)
  conn <- getConnection
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
    => Table columnsW returned -> incoming -> AppM row
createRow table item = do
  auditLog $ "Create : " ++ (show item)
  currentTime <- liftIO $ fmap pgUTCTime getCurrentTime
  let itemPg = (constant item) & createdat .~ (Just currentTime) & updatedat .~ (currentTime)
  fmap (head) $ createDbRows table [itemPg] 

updateRow :: (
    Show columnsW
    , Show haskells
    , HasUpdatedat haskells UTCTime
    , D.Default Constant haskells columnsW
    , D.Default Constant itemId (Column PGInt4)
    , HasId haskells itemId
    , HasId columnsR (Column PGInt4)
    )
    => Table columnsW columnsR -> haskells -> AppM haskells
updateRow table item = do
  auditLog $ "Update : " ++ (show item)
  let itId = item ^. id
  currentTime <- liftIO getCurrentTime
  let updatedItem = (putUpdatedTimestamp currentTime) item
  _ <- updateDbRow table (constant itId) (constant updatedItem) 
  return updatedItem
  where
    putUpdatedTimestamp :: (HasUpdatedat item (UTCTime)) => UTCTime -> item -> item
    putUpdatedTimestamp timestamp  = updatedat .~ timestamp

removeRow :: (
      Show haskells
    , D.Default Constant itemId (Column PGInt4)
    , HasId haskells itemId
    , HasId columnsR (Column PGInt4)
    ) => Table columnsW columnsR -> haskells -> AppM GHC.Int.Int64
removeRow table item = do
  auditLog $ "Remove : " ++ (show item)
  conn <- getConnection
  liftIO $ do
    runDelete conn table $ matchFunc $ item ^. id
  where
    matchFunc :: (
        HasId columnsR (Column PGInt4),
        D.Default Constant itemId (Column PGInt4)
        ) => (itemId -> columnsR -> Column PGBool)
    matchFunc itId item' = (item' ^. id) .== (constant itId)

readRow :: (D.Default QueryRunner columnsR haskells) => 
  Opaleye.Query columnsR -> AppM [haskells]
readRow query' = do
  conn <- getConnection
  liftIO $ runQuery conn query'

