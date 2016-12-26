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
import Data.Text

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField(..))

import Prelude hiding (id)
import Control.Arrow

newtype UserId = UserId Int deriving (Show)

data Name
data Email

newtype DbText a = DbText Text

data UserPoly id name email = User { userId :: id, userName :: name, userEmail :: email } deriving (Show)

type User = UserPoly UserId String String
type UserPGW = UserPoly (Column UserId) (Column (DbText Name)) (Column (DbText Email))
type UserPGR = UserPoly (Column UserId) (Column (DbText Name)) (Column (DbText Email))

newtype PostId = PostId Int deriving (Show)

data PostPoly id name user_id = Post { postId :: id, postName :: name, postUserId :: user_id } deriving (Show)

type Post = PostPoly PostId String UserId
type PostPGW = PostPoly (Column PostId) (Column (DbText Name)) (Column UserId)
type PostPGR = PostPoly (Column PostId) (Column (DbText Name)) (Column UserId)

$(makeAdaptorAndInstance "pUser" ''UserPoly)
$(makeAdaptorAndInstance "pPost" ''PostPoly)

userTable :: Table UserPGW UserPGR
userTable = Table "users" (pUser User {
    userId = required "id",
    userName = required "name",
    userEmail = required "email"
    }
  )

postTable :: Table PostPGW PostPGR
postTable = Table "posts" (pPost Post {
    postId = required "id",
    postName = required "name",
    postUserId = required "user_id"
    }
  )

instance FromField UserId where
  fromField field bs = UserId <$> fromField field bs
  
instance FromField PostId where
  fromField field bs = PostId <$> fromField field bs

instance QueryRunnerColumnDefault UserId UserId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault PostId PostId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromField (DbText a) where
  fromField field bs = DbText <$> fromField field bs

instance QueryRunnerColumnDefault (DbText a) String where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant Text (Column (DbText a)) where
  def = Constant (\x -> unsafeCoerceColumn $ pgStrictText x)

instance Default Constant String (Column (DbText a)) where
  def = Constant (\x -> unsafeCoerceColumn $ pgString x)

instance Default Constant UserId (Column UserId) where
  def = Constant (\(UserId x) -> unsafeCoerceColumn $ pgInt4 x)

instance Default Constant PostId (Column PostId) where
  def = Constant (\(PostId x) -> unsafeCoerceColumn $ pgInt4 x)

getUserRows :: IO [(User, Post)]
getUserRows = do
  conn <- connect defaultConnectInfo { connectDatabase = "scratch"}
  runQuery conn $ proc () ->
    do
      post@Post {postId = puid} <- queryTable postTable -< ()
      user@User {userId = uid} <- queryTable userTable -< ()
      --restrict -< (puid .== uid)
      returnA -< (user, post)

insertUser :: Connection -> User -> IO ()
insertUser conn user = do
  runInsertMany conn userTable [(constant user)]
  return ()

insertPost :: Connection -> Post -> IO ()
insertPost conn post = do
  runInsertMany conn postTable [(constant post)]
  return ()

main :: IO ()
main = do
  rows <- getUserRows
  putStrLn $ show rows
