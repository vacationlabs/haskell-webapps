module InternalUtils where

import Auditable
import Data.Aeson.Types
import qualified Data.HashMap.Strict as HM

auditable :: a -> Auditable a
auditable a = Auditable {_data = a, _log = Object HM.empty}

wrapAuditable :: (Functor a, Functor b) => a (b c) -> a (b (Auditable c))
wrapAuditable a = (fmap auditable) <$> a
