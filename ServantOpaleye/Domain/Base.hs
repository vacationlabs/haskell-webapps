-- TODO: This can act as my custom prelude
module Domain.Base(
  module Opaleye
  ,module DB
  ,module Domain.BaseTypes
  ) where

import qualified Data.Text as T
import Opaleye (Query)
import Domain.BaseTypes
import DB
