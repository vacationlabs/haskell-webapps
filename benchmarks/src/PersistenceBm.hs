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
     persistence_benchmark
    ) where


import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import Control.Monad.Logger

import Criterion.Main
import Criterion.Types (Config(..))

import Control.Monad
import Control.Monad.IO.Class

import qualified Database.Esqueleto as E

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase| 
Users
    name String 
    email String
    deriving Show

Tenants
    name String
    email String
    ownerId (Key Users)
    deriving Show
|]


persistence_benchmark :: IO ()
--persistence_benchmark = return ()
persistence_benchmark = do
  withPostgresqlConn "host=localhost port=5432 user=postgres dbname=benchmark password=test" (\backend -> do
    defaultMainWith defaultConfig {resamples = 1000} [
       bench "Persistence: getRows" $ whnfIO $ replicateM_ 1000 $ getRows backend 
      ,bench "Persistence: insertRows" $ whnfIO $ replicateM_ 1000 $ insertRow backend 
      ,bench "Persistence: insertRowsReturning" $ whnfIO $ replicateM_ 1000 $ insertRowReturning backend 
      ,bench "Persistence: updateRows" $ whnfIO $ replicateM_ 1000 $ updateRow backend 
      ,bench "Persistence: updateRowsReturning" $ whnfIO $ replicateM_ 1000 $ updateRowReturning backend 
      ,bench "Persistence: twowayJoin" $ whnfIO $ replicateM_ 1000 $ twowayJoin backend 
      ]
    )

instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()

getRows :: SqlBackend -> IO [Entity Users]
getRows backend = do
  flip runSqlPersistM backend $ do
    selectList [] [LimitTo 1]  :: SqlPersistM [Entity Users]

insertRow :: SqlBackend -> IO (Key Users)
insertRow backend = do
  flip runSqlPersistM backend $ do
    michaelId <- insert $ Users "Michael" "michael@mail.com"
    return michaelId

insertRowReturning :: SqlBackend -> IO (Entity Users)
insertRowReturning backend = do
  flip runSqlPersistM backend $ do
    insertEntity $ Users "Michael" "michael@mail.com"

updateRow :: SqlBackend -> IO ()
updateRow backend = do
  flip runSqlPersistM backend $ do
    update (toSqlKey 1) [ UsersName =. "John" ]

updateRowReturning :: SqlBackend -> IO Users
updateRowReturning backend = do
  flip runSqlPersistM backend $ do
    updateGet (toSqlKey 1) [ UsersName =. "John" ]

twowayJoin :: SqlBackend -> IO [Entity Users]
twowayJoin backend = do
  flip runSqlPersistM backend twowayJoin'
  where
    twowayJoin' :: SqlPersistM [Entity Users]
    twowayJoin' = 
      do 
        users <- E.select $
                 E.from $ \(users `E.InnerJoin` tenants) -> do
                 let k = (tenants E.^. TenantsOwnerId)
                 E.on ((users E.^. UsersId) E.==. k)
                 return users
        return users
