{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE MultiParamTypeClasses          #-}
{-# LANGUAGE FlexibleInstances          #-}
module TenantId where

import qualified Data.Profunctor.Product.Default      as D
import Opaleye
import           Database.PostgreSQL.Simple.FromField
import           Data.Aeson
import           Data.Aeson.Types
import           GHC.Generics

newtype TenantId = TenantId Int
  deriving (Show, Generic)

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
