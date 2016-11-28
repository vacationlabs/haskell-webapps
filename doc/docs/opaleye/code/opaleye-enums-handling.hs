{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE Arrows                #-}

module Main where

import Opaleye
import Data.Profunctor.Product
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH           (makeAdaptorAndInstance)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField(..), returnError, ResultError(..), Conversion)

import Prelude hiding (id)
import Control.Arrow

data UserType = SuperAdmin | Admin | Registered deriving (Show)

newtype UserId = UserId Int deriving (Show)

data UserPoly id name email utype = User { id :: id, name :: name, email :: email, utype :: utype } deriving (Show)

type User = UserPoly UserId String String UserType
type UserPGW = UserPoly (Column PGInt4) (Column PGText) (Column PGText) (Column PGText)
type UserPGR = UserPoly (Column PGInt4) (Column PGText) (Column PGText) (Column PGText)

$(makeAdaptorAndInstance "pUser" ''UserPoly)

userTable :: Table UserPGW UserPGR
userTable = Table "typed_users" (pUser User {
    id = required "id",
    name = required "name",
    email = required "email",
    utype = required "user_type"
    }
  )

instance FromField UserId where
  fromField field bs = UserId <$> fromField field bs

instance QueryRunnerColumnDefault PGInt4 UserId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromField UserType where
  fromField field bs = utConversion $ fromField field bs
    where
      utConversion :: Conversion String -> Conversion UserType
      utConversion cString = do
        typeString <- cString
        case mkUserType typeString of
          Nothing -> returnError ConversionFailed field "Unrecognized user type"
          Just ut -> return ut
      mkUserType :: String -> Maybe UserType
      mkUserType "superadmin" = Just SuperAdmin
      mkUserType "admin" = Just Admin
      mkUserType "registered" = Just Registered
      mkUserType _ = Nothing

instance QueryRunnerColumnDefault PGText UserType where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

getUserRows :: IO [User]
getUserRows = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  runQuery conn $ proc () ->
    do
      user <- queryTable userTable -< ()
      returnA -< user

main :: IO ()
main = do
  rows <- getUserRows
  putStrLn $ show rows
