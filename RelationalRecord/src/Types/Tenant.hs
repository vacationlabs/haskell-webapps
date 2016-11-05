{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

{-# LANGUAGE FlexibleContexts #-}

module  Types.Tenant where

import  Types.DefineTable


import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineTable "tenants")

deriving instance Generic Tenants
instance ToJSON Tenants



-- experiment how to best make an enum field

data SomeEnum = Item1 | Item2 | Item3 deriving Enum

data SomeType = SomeType
    { theId     :: Int
    , theEnum   :: SomeEnum
    }

-- $(makeRecordPersistableDefault ''SomeType)
