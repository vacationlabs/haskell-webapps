module Main where

import Opaleye
import Data.Profunctor.Product (p3)

import Database.PostgreSQL.Simple

import GHC.Int

import Criterion.Main
import Criterion.Types (Config(..))

userTable :: Table 
  (Maybe (Column PGInt4), Column PGText, Column PGText) 
  (Column PGInt4, Column PGText, Column PGText)
userTable = Table "users" (p3 (
  optional "id",
  required "name",
  required "email"
  ))

getRows :: Connection -> IO [(Int, String, String)]
getRows conn = do
  runQuery conn $ queryTable userTable

insertRow :: Connection -> (Maybe Int, String, String) -> IO Int64
insertRow conn u = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  runInsertMany conn userTable [(constant u)] 

insertRowReturning :: Connection -> (Maybe Int, String, String) -> IO [(Int, String, String)]
insertRowReturning conn u = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  runInsertManyReturning conn userTable [(constant u)] (\x -> x)

updateRowReturning :: Connection -> (Maybe Int, String, String) -> IO [(Int, String, String)]
updateRowReturning conn row = do
  runUpdateReturning conn userTable (\_ -> constant row) (\(id, _, _) -> id .== (pgInt4 1)) (\x -> x)

-- export LC_ALL=C.UTF-8
-- You may have to run this command in your terminal if you get a char encoding/display error
main = IO ()
main = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  defaultMainWith defaultConfig {resamples = 10000} [
      bench "getRows" $ nfIO $ getRows conn
    , bench "insertRow" $ nfIO $ insertRow conn (Nothing, "Max", "max@mail.com")
    , bench "insertRowReturning" $ nfIO $ insertRowReturning conn (Nothing, "Max", "max@mail.com")
    ,bench "updateRowReturning" $ nfIO $ updateRowReturning conn (Nothing, "Bob", "max@mail.com")
    ]
