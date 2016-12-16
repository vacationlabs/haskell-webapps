module Auditable where

import           Data.Aeson.Types

data Auditable a = Auditable { _data:: a, _log:: Value }  deriving (Show)
