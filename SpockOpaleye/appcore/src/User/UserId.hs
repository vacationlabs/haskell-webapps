{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE FlexibleInstances          #-}

module UserId where

import           Database.PostgreSQL.Simple.FromField
import Opaleye
import qualified Data.Profunctor.Product.Default      as D
import           GHC.Generics

import           Data.Aeson
import           Data.Aeson.Types

newtype UserId = UserId Int
  deriving (Show, Generic)

instance D.Default Constant (UserId) (Column PGInt4) where
  def = Constant def'
    where
      def' :: UserId -> (Column PGInt4)
      def' (UserId id') = pgInt4 id'

instance D.Default Constant UserId () where
  def = Constant (\_ -> ())

instance D.Default Constant (UserId) (Column (Nullable PGInt4)) where
  def = Constant def'
    where
      def' :: UserId -> (Column (Nullable PGInt4))
      def' (UserId id') = (toNullable.pgInt4) id'

instance FromField UserId where
  fromField f mdata = do
    x <- fromField f mdata
    return $ UserId x

instance QueryRunnerColumnDefault PGInt4 UserId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromJSON UserId where
  parseJSON j@(Number _) = UserId <$> (parseJSON j)
  parseJSON invalid      = typeMismatch "UserId" invalid

instance ToJSON UserId where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions
