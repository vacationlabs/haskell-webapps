{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module ApiBase where

import           Classes
import           UserDefs
import           TenantDefs
import           OpaleyeDef
import           Lenses
import           Auditable
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Control.Monad.Reader as R
import           Control.Monad.Writer
import qualified Data.Profunctor.Product.Default as D
import           Data.Time                       (UTCTime, getCurrentTime)
import           Opaleye
import qualified Data.Text as T
import           GHC.Int
import           Prelude                         hiding (id)
import           Data.Aeson (Value(..))
import           Data.ByteString (ByteString)
import           Database.PostgreSQL.Simple
import           InternalUtils

removeRawDbRows :: (DbConnection m, MonadIO m) 
    => Table columnsW columnsR 
    -> (columnsR -> Column PGBool)
    -> m GHC.Int.Int64
removeRawDbRows table matchFunc = do
  conn <- getConnection
  liftIO $ runDelete conn table matchFunc 

createDbRows :: (DbConnection m, MonadIO m, Show columnsW, D.Default QueryRunner columnsR haskells) 
    =>  Table columnsW columnsR
    -> [columnsW] 
    -> m [haskells]
createDbRows table pgrows = do
  --auditLog $ "Create : " ++ (show pgrows)
  conn <- getConnection
  liftIO $ runInsertManyReturning conn table pgrows (\x -> x)

updateDbRow :: (DbConnection m, MonadIO m, Show columnsW, HasId columnsR (Column PGInt4)) 
  => Table columnsW columnsR 
  -> Column PGInt4
  -> columnsW
  -> m columnsW
updateDbRow table row_id item = do
  --auditLog $ "Update :" ++ (show item)
  conn <- getConnection
  _ <- liftIO $ runUpdate conn table (\_ -> item) (matchFunc row_id) 
  return item
  where
    matchFunc :: (HasId cmR (Column PGInt4)) => (Column PGInt4 -> cmR -> Column PGBool)
    matchFunc itId item' = (item' ^. id) .== itId
  
createRow ::(
    DbConnection m,
    MonadIO m, 
    Show incoming,
    Show columnsW,
    HasCreatedat columnsW (Maybe (Column PGTimestamptz)),
    HasUpdatedat columnsW (Column PGTimestamptz),
    D.Default Constant incoming columnsW, D.Default QueryRunner returned row)
    => Table columnsW returned -> incoming -> m (Auditable row)
createRow table item = do
  --auditLog $ "Create : " ++ (show item)
  currentTime <- liftIO $ fmap pgUTCTime getCurrentTime
  let itemPg = (constant item) & createdat .~ (Just currentTime) & updatedat .~ (currentTime)
  fmap (auditable.head) $ createDbRows table [itemPg] 

updateRow :: (
    DbConnection m,
    MonadIO m, 
    Show columnsW
    , Show haskells
    , HasUpdatedat haskells UTCTime
    , D.Default Constant haskells columnsW
    , D.Default Constant itemId (Column PGInt4)
    , HasId haskells itemId
    , HasId columnsR (Column PGInt4)
    )
    => Table columnsW columnsR -> haskells -> m haskells
updateRow table item = do
  --auditLog $ "Update : " ++ (show item)
  let itId = item ^. id
  currentTime <- liftIO getCurrentTime
  let updatedItem = (putUpdatedTimestamp currentTime) item
  _ <- updateDbRow table (constant itId) (constant updatedItem) 
  return updatedItem
  where
    putUpdatedTimestamp :: (HasUpdatedat item (UTCTime)) => UTCTime -> item -> item
    putUpdatedTimestamp timestamp  = updatedat .~ timestamp

updateAuditableRow :: (
    DbConnection m
    , CurrentUser m
    , CurrentTenant m
    , MonadIO m
    , Show columnsW
    , HasUpdatedat (Auditable haskells) UTCTime
    , D.Default Constant haskells columnsW
    , D.Default Constant itemId (Column PGInt4)
    , HasId (Auditable haskells) itemId
    , HasId columnsR (Column PGInt4)
    )
    => Table columnsW columnsR -> Auditable haskells -> m (Auditable haskells)
updateAuditableRow table audti = do
  --auditLog $ "Update : " ++ (show item)
  let itId = audti ^. id
  currentTime <- liftIO getCurrentTime
  let updatedItem = (putUpdatedTimestamp currentTime) audti
  let Auditable { _data = item, _log = _log} = updatedItem
  _ <- updateDbRow table (constant itId) (constant item) 
  insertIntoLog table itId "" _log
  return audti
  where
    putUpdatedTimestamp :: (HasUpdatedat item (UTCTime)) => UTCTime -> item -> item
    putUpdatedTimestamp timestamp  = updatedat .~ timestamp

insertIntoLog :: (
    MonadIO m
    , CurrentTenant m
    , CurrentUser m
    , DbConnection m
    ,D.Default Constant item_id (Column PGInt4)
    ) => Table a b -> item_id -> T.Text -> Value -> m ()
insertIntoLog table auditable_id summary changes = do
  case table of
    Table table_name _ -> do
      Just tenant <- getCurrentTenant
      Just user <- getCurrentUser
      conn <- getConnection
      let tenant_id = tenant ^. id
      let user_id = user ^. id
      _ <- liftIO $ runInsertMany conn auditTable [(
        (),
        constant tenant_id,
        Just $ constant user_id,
        Just $ pgBool False,
        constant auditable_id,
        pgStrictText $ T.pack table_name,
        pgStrictText $ summary,
        constant changes,
        Nothing)]
      return ()
    _ -> error "Unsupported Table constructor"

removeAuditableRow :: (
    MonadIO m
    , DbConnection m
    , Show haskells
    , D.Default Constant itemId (Column PGInt4)
    , HasId haskells itemId
    , HasId columnsR (Column PGInt4)
    ) => Table columnsW columnsR -> Auditable haskells -> m GHC.Int.Int64
removeAuditableRow table item_r = do
  --auditLog $ "Remove : " ++ (show item)
  conn <- getConnection
  let Auditable { _data = item, _log = _log} = item_r
  liftIO $ do
    runDelete conn table $ matchFunc $ item ^. id
  where
    matchFunc :: (
        HasId columnsR (Column PGInt4),
        D.Default Constant itemId (Column PGInt4)
        ) => (itemId -> columnsR -> Column PGBool)
    matchFunc itId item' = (item' ^. id) .== (constant itId)

removeRow :: (
    MonadIO m
    , DbConnection m
    , Show haskells
    , D.Default Constant itemId (Column PGInt4)
    , HasId haskells itemId
    , HasId columnsR (Column PGInt4)
    ) => Table columnsW columnsR -> haskells -> m GHC.Int.Int64
removeRow table item = do
  --auditLog $ "Remove : " ++ (show item)
  conn <- getConnection
  liftIO $ do
    runDelete conn table $ matchFunc $ item ^. id
  where
    matchFunc :: (
        HasId columnsR (Column PGInt4),
        D.Default Constant itemId (Column PGInt4)
        ) => (itemId -> columnsR -> Column PGBool)
    matchFunc itId item' = (item' ^. id) .== (constant itId)

readRow :: (
  MonadIO m
  , DbConnection m
  , D.Default QueryRunner columnsR haskells) => 
  Opaleye.Query columnsR -> m [Auditable haskells]
readRow query' = do
  conn <- getConnection
  liftIO $ wrapAuditable $ runQuery conn query'
