{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Types.AuditLog where

import  Types.DefineTable

$(defineTable "audit_logs")
