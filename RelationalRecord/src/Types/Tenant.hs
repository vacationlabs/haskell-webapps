{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

{-# LANGUAGE FlexibleContexts #-}

module  Types.Tenant where

import  Types.DefineTable


import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)



import Database.HDBC.Query.TH
import Database.Record



$(defineTable "tenant")

deriving instance Generic Tenant
instance ToJSON Tenant



-- experiment how to best make an enum field

data SomeEnum = Item1 | Item2 | Item3 deriving Enum

data SomeType = SomeType
    { theId     :: Int
    , theEnum   :: SomeEnum
    }

-- $(makeRecordPersistableDefault ''SomeType)
