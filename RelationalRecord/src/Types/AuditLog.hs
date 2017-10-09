{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module  Types.AuditLog where

import  DefineTable

$(defineTable "audit_logs")
