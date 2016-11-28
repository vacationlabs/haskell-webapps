{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows                #-}
{-# LANGUAGE OverloadedStrings      #-}

module Main where

import Opaleye
import Data.Profunctor.Product (p3)
import Data.Profunctor.Product.Default

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField(..))

import Control.Arrow

userTable :: Table 
    (Column PGInt4, Column PGText, Column PGText) 
    (Column PGInt4, Column PGText, Column PGText)
userTable = Table "users" (p3 (
    required "id",
    required "name",
    required "email"
    ))

newtype UserId = UserId Int deriving (Show)

instance FromField UserId where
  fromField field bs = UserId <$> fromField field bs

instance QueryRunnerColumnDefault PGInt4 UserId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

data User = User { id :: UserId, name :: String, email :: String } deriving (Show)

makeUserFromTuple :: (Int, String, String) -> User
makeUserFromTuple (id_, name_, e_mail) = User (UserId id_) name_ e_mail

instance Default QueryRunner (Column PGInt4, Column PGText, Column PGText) User where
  def = makeUserFromTuple <$> def

getUserRows :: IO [User]
getUserRows = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  runQuery conn $ proc () ->
    do
      user@(_, pgName, _) <- queryTable userTable -< ()
      restrict -< (pgName .== (pgStrictText "John"))
      returnA -< user

main :: IO ()
main = do
  rows <- getUserRows
  putStrLn $ show rows

-- Output
-- >main
-- [User {id = UserId 1, name = "John", email = "john@mail.com"}]
