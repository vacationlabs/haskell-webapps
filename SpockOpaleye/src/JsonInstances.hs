{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JsonInstances where

import           AppCore
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Text

import qualified Data.HashMap.Strict as HM

instance (FromJSON a) => FromJSON (Auditable a) where
  parseJSON j = auditable <$> (parseJSON j)
