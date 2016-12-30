{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Ids where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Profunctor.Product.Default      as D
import           Database.PostgreSQL.Simple.FromField
import           GHC.Generics
import           Opaleye

-- TenantId

newtype TenantId = TenantId Int
  deriving (Show, Generic, Eq)

instance D.Default Constant TenantId (Column PGInt4) where
  def = Constant def'
    where
      def' :: TenantId -> (Column PGInt4)
      def' (TenantId id') = pgInt4 id'

instance D.Default Constant TenantId () where
  def = Constant (\_ -> ())

instance FromField TenantId where
  fromField f mdata = do
    x <- fromField f mdata
    return $ TenantId x

instance QueryRunnerColumnDefault PGInt4 TenantId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromJSON TenantId where
  parseJSON j@(Number _) = TenantId <$> (parseJSON j)
  parseJSON invalid      = typeMismatch "TenantId" invalid

instance ToJSON TenantId where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

-- UserId

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
