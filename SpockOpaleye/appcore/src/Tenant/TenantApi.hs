{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module TenantApi where

import           Control.Monad.IO.Class
import           Control.Lens
import           TenantDefs
import           ApiBase
import           AppM
import           Utils
import           Lenses
import           CryptoDef

createTenant :: TenantIncoming -> AppM Tenant
createTenant tenant = do
  auditable <$> createRow tenantTable tenant
