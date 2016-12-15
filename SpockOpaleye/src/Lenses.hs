{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
module Lenses where

import Control.Lens
import Language.Haskell.TH
import UserDefs
import TenantDefs
import RoleDefs
import TH
import Prelude hiding (id)

$(makeLensesWith abbreviatedFields ''TenantPoly)
$(makeLensesWith abbreviatedFields ''UserPoly)
$(makeLensesWith abbreviatedFields ''RolePoly)
$(makeAuditableLenses ''InternalTenant)
$(makeAuditableLenses ''InternalUser)
$(makeAuditableLenses ''InternalRole)
