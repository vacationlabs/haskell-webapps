{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Types.Enum where

import  Helpers.DefineEnum

$(defineEnum "test_enum")
