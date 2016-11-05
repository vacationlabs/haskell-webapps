{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}

module  Types.User where

import  Types.DefineTable


import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineTable "users")

deriving instance Generic Users
instance ToJSON Users
