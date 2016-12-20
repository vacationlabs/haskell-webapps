{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Lenses where

import           AuditableTH
import           Control.Lens
import           Language.Haskell.TH
import           Prelude             hiding (id)
import           RoleDefs
import           TenantDefs
import           UserDefs

$(makeLensesWith abbreviatedFields ''TenantPoly)
$(makeLensesWith abbreviatedFields ''UserPoly)
$(makeLensesWith abbreviatedFields ''RolePoly)
$(makeAuditableLenses ''InternalTenant)
$(makeAuditableLenses ''InternalUser)
$(makeAuditableLenses ''InternalRole)
