{-# LANGUAGE OverloadedStrings #-}

module Validations where

import DataTypes

validateIncomingTenant :: TenantIncoming -> ValidationResult
validateIncomingTenant ti = Valid
