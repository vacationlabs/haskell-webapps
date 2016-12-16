{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module JsonInstances where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Text
import           DataTypes

import Auditable
import qualified Data.HashMap.Strict as HM

auditable :: a -> Auditable a
auditable a = Auditable {_data = a, _log = Object HM.empty}

wrapAuditable :: (Functor a, Functor b) => a (b c) -> a (b (Auditable c))
wrapAuditable a = (fmap auditable) <$> a

instance (FromJSON a) => FromJSON (Auditable a) where
  parseJSON j = auditable <$> (parseJSON j)
