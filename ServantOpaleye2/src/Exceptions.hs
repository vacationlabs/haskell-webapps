{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Exceptions where

import           Control.Exception
import           Data.Typeable

data NotFoundException = NotFoundException String
    deriving (Show, Typeable)

instance Exception NotFoundException
