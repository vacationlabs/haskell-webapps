{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Opaleye
import Data.Profunctor.Product (p4, p3)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField

userTable :: Table 
    (Column PGInt4, Column PGText, Column PGText) 
    (Column PGInt4, Column PGText, Column PGText)
userTable = Table "scratch" (p3 (
    required "id",
    required "email",
    required "name"
    ))

newtype UserId = UserId Int deriving (Show)

instance FromField UserId where
  fromField field bs = UserId <$> fromField field bs

instance QueryRunnerColumnDefault PGInt4 UserId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

getUserRows :: IO [(UserId, String, String)]
getUserRows = do
  conn <- connect defaultConnectInfo
  runQuery conn $ queryTable userTable

main :: IO ()
main = do
  rows <- getUserRows
  putStrLn $ show rows
