{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
module Exceptions where

import Control.Exception
import Data.Typeable

data NotFoundException = NotFoundException String
    deriving (Show, Typeable)

instance Exception NotFoundException
