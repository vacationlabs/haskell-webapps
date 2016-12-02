{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ScopedTypeVariables               #-}
module PersistenceBm
    ( 
     persistence_benchmark,
     Users
    ) where


import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import Control.Monad.Logger
import qualified Database.PostgreSQL.Simple as PSimple

import Criterion.Main
import Criterion.Types (Config(..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase| 
Users
    name String 
    email String
    deriving Show
|]


persistence_benchmark :: IO ()
--persistence_benchmark = return ()
persistence_benchmark = do
  withPostgresqlConn "host=localhost port=5432 user=postgres dbname=benchmark password=test" (\backend -> do
    defaultMainWith defaultConfig {resamples = 10000} [
       bench "getRows" $ whnfIO $ getRows backend 
      ,bench "insertRows" $ whnfIO $ insertRow backend 
      ,bench "updateRows" $ whnfIO $ updateRow backend 
      ]
    )

instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()

getRows :: SqlBackend -> IO [Entity Users]
getRows backend = do
  flip runSqlPersistM backend $ do
    selectList [] [LimitTo 1]  :: ReaderT SqlBackend (Control.Monad.Logger.NoLoggingT (Control.Monad.Trans.Resource.ResourceT IO)) [Entity Users]

insertRow :: SqlBackend -> IO (Key Users)
insertRow backend = do
  flip runSqlPersistM backend $ do
    michaelId <- insert $ Users "Michael" "michael@mail.com"
    return michaelId

updateRow :: SqlBackend -> IO ()
updateRow backend = do
  flip runSqlPersistM backend $ do
    update (toSqlKey 1) [ UsersName =. "John" ]
