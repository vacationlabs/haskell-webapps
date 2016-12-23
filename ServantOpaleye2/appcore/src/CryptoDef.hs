{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module CryptoDef
  ( BcryptPassword
  , bcryptPassword
  , verifyPassword
  ) where

import           Crypto.BCrypt
import           Data.Aeson                           (ToJSON (..), Value (..))
import           Data.ByteString
import qualified Data.Profunctor.Product.Default      as D
import           Data.Text
import           Data.Text.Encoding
import           Database.PostgreSQL.Simple.FromField
import           Opaleye

newtype BcryptPassword =
  BcryptPassword ByteString
  deriving (Show)

bcryptPassword :: Text -> IO (Maybe BcryptPassword)
bcryptPassword password = do
  hash <-
    hashPasswordUsingPolicy slowerBcryptHashingPolicy $ encode password
  return $ BcryptPassword <$> hash

verifyPassword :: Text -> BcryptPassword -> Bool
verifyPassword password (BcryptPassword hash) = validatePassword hash (encode password)

encode :: Text -> ByteString
encode password = encodeUtf8 password

instance D.Default Constant (BcryptPassword) (Column PGBytea) where
  def = Constant def'
    where
      def' :: BcryptPassword -> (Column PGBytea)
      def' (BcryptPassword hash) = pgStrictByteString $ hash

instance FromField BcryptPassword where
  fromField field mdata = do
    x <- fromField field mdata
    return $ BcryptPassword x

instance QueryRunnerColumnDefault PGBytea BcryptPassword where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance ToJSON BcryptPassword where
  toJSON _ = String ""
