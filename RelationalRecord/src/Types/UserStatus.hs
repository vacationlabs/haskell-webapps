{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}


module  Types.UserStatus where

import  Helpers.DefineEnum

import  GHC.Generics            (Generic)
import  Data.Aeson              (ToJSON)


$(defineEnum "user_status")

deriving instance Generic UserStatus
instance ToJSON UserStatus
