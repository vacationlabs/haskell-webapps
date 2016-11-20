{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Types.EnumDummy where

import  DefineTable
import  Prelude hiding (id)

$(defineTable "enum_dummy")
