{-# LANGUAGE Arrows                #-}
module OpaleyeBm
    ( 
     opaleye_benchmark
     ,clearTables
    ) where


import Opaleye
import Data.Profunctor.Product (p3, p4)

import Database.PostgreSQL.Simple

import GHC.Int

import Criterion.Main
import Criterion.Types (Config(..))
import Control.Arrow

userTable :: Table 
  (Maybe (Column PGInt4), Column PGText, Column PGText) 
  (Column PGInt4, Column PGText, Column PGText)
userTable = Table "users" (p3 (
  optional "id",
  required "name",
  required "email"
  ))

tenantTable :: Table 
  (Maybe (Column PGInt4), Column PGText, Column PGText, Column PGInt4) 
  (Column PGInt4, Column PGText, Column PGText, Column PGInt4)
tenantTable = Table "tenants" (p4 (
  optional "id",
  required "name",
  required "email",
  required "owner_id"
  ))

getRows :: Connection -> IO [(Int, String, String)]
getRows conn = do
  runQuery conn $ queryTable userTable

insertRow :: Connection -> (Maybe Int, String, String) -> IO Int64
insertRow conn u = do
  runInsertMany conn userTable [(constant u)] 

insertRowReturning :: Connection -> (Maybe Int, String, String) -> IO [(Int, String, String)]
insertRowReturning conn u = do
  runInsertManyReturning conn userTable [(constant u)] (\x -> x)

updateRowReturning :: Connection -> (Maybe Int, String, String) -> IO [(Int, String, String)]
updateRowReturning conn row = do
  runUpdateReturning conn userTable (\_ -> constant row) (\(id, _, _) -> id .== (pgInt4 1)) (\x -> x)

twowayJoin :: Connection -> IO [(String, String, String, String)]
twowayJoin conn = do
  runQuery conn $ proc () ->
    do
      (u_id, u_name, u_email) <- queryTable userTable -< ()
      (t_id, t_name, t_email, t_uid) <- queryTable tenantTable -< ()
      restrict -< (t_uid .== u_id)
      returnA -< (u_name, u_email, t_name, t_email)

clearTables :: Connection -> IO ()
clearTables conn = do
  runDelete conn userTable (\(id', _, _) -> (id' .> constant (1::Int)))
  return ()

opaleye_benchmark :: Connection -> IO ()
opaleye_benchmark conn = do
  conn <- connect defaultConnectInfo { connectDatabase = "benchmark"}
  defaultMainWith defaultConfig {resamples = 10000} [
      bench "getRows" $ nfIO $ getRows conn
    , bench "insertRow" $ nfIO $ insertRow conn (Nothing, "Max", "max@mail.com")
    , bench "insertRowReturning" $ nfIO $ insertRowReturning conn (Nothing, "Max", "max@mail.com")
    , bench "updateRowReturning" $ nfIO $ updateRowReturning conn (Nothing, "Bob", "max@mail.com")
    , bench "twowayJoin" $ nfIO $ twowayJoin conn 
    ]
  clearTables conn



