{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module  Types.Role where

import  Types.DefineTable


import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineTable "roles")

deriving instance Generic Roles
instance ToJSON Roles
