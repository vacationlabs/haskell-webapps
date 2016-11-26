module Main where

import Opaleye
import Data.Profunctor.Product (p3)

import Database.PostgreSQL.Simple

userTable :: Table 
    (Column PGInt4, Column PGText, Column PGText) 
    (Column PGInt4, Column PGText, Column PGText)
userTable = Table "scratch" (p3 (
    required "id",
    required "email",
    required "name"
    ))

getUserRows :: IO [(Int, String, String)]
getUserRows = do
  conn <- connect defaultConnectInfo
  runQuery conn $ queryTable userTable

main :: IO ()
main = do
  rows <- getUserRows
  putStrLn $ show rows
