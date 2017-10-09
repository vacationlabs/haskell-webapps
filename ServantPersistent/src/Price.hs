{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE DeriveGeneric  #-}
module Price where

import Database.Persist.Sql
import GHC.Generics
import Data.Aeson

newtype Price = Price { intPrice :: Int }
  deriving (PersistField, PersistFieldSql, Generic, Eq, Ord)

instance ToJSON Price
