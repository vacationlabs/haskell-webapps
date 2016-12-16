module RoleApi where

import           Control.Monad.IO.Class
import           Control.Lens
import           UserDefs
import           ApiBase
import           AppM
import           Utils
import           Lenses

createRole :: RoleIncoming -> AppM Role
createRole role = auditable <$> createRow roleTable role

