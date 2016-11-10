{-# LANGUAGE DataKinds, FlexibleContexts, FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies                 #-}

module MockAPI.Shaped where
import Data.Text

data Shape = Info | Error | Validation

type family Field i a where
  Field Info a       = a
  Field Error a      = Maybe Text
  Field Validation a = a -> Maybe Text

class Shaped a b | a -> b, b -> a where
  toShape   :: a -> b
  fromShape :: b -> a
