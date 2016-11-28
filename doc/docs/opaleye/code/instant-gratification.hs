{-# LANGUAGE Arrows                #-}
module Main where

import Opaleye
import Database.PostgreSQL.Simple
import Data.Profunctor.Product (p3)
import Control.Arrow

userTable :: Table
  (Column PGInt4, Column PGText, Column PGText)  -- read type
  (Column PGInt4, Column PGText, Column PGText) -- write type
userTable = Table "users" (p3 (required "id",
                               required "name",
                               required "email"))


selectAllRows :: Connection -> IO [(Int, String, String)]
selectAllRows conn = runQuery conn $ queryTable userTable

insertRow :: Connection -> (Int, String, String) -> IO ()
insertRow conn row = do
  runInsertMany conn userTable [(constant row)]
  return ()

selectByEmail :: Connection -> String -> IO [(Int, String, String)]
selectByEmail conn email = runQuery conn $ proc () ->
    do
      row@(_, _, em) <- queryTable userTable -< ()
      restrict -< (em .== constant email)
      returnA -< row

updateRowByEmail :: Connection -> (Int, String, String) -> IO ()
updateRowByEmail conn row@(id, name, email) = do
  runUpdate conn userTable (\_ -> constant row) (\(_,_,em) -> em .== constant email)
  return ()


main :: IO ()
main = do
  conn <- connect ConnectInfo{connectHost="localhost"
                             ,connectPort=5432
                             ,connectDatabase="opaleye_tutorial"
                             ,connectPassword="opalaye_tutorial"
                             ,connectUser="opaleye_tutorial"
                             }

  allRows <- selectAllRows conn
  print allRows

  insertRow conn (4, "Saurabh", "saurabhnanda@gmail.com")

  row <- selectByEmail conn "saurabhnanda@gmail.com"
  print row

  updateRowByEmail conn (4, "Don", "corleone@puzo.com")

  allRows <- selectAllRows conn
  print allRows

  return ()
