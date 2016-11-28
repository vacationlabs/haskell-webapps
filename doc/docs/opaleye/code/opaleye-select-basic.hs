module Main where

import Opaleye
import Data.Profunctor.Product (p3)

import Database.PostgreSQL.Simple

userTable :: Table 
    (Column PGInt4, Column PGText, Column PGText) 
    (Column PGInt4, Column PGText, Column PGText)
userTable = Table "users" (p3 (
    required "id",
    required "name",
    required "email"
    ))

getUserRows :: IO [(Int, String, String)]
getUserRows = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  runQuery conn $ queryTable userTable

main :: IO ()
main = do
  rows <- getUserRows
  putStrLn $ show rows

-- Output
-- >main
-- [(1,"John","john@mail.com"),(2,"Bob","bob@mail.com"),(3,"Alice","alic
-- e@mail.com")]
