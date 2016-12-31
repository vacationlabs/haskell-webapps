{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}

module InternalClasses where

import UserDefs
import RoleDefs
import Lenses
import Control.Lens

class UpdatePair src dst where
  merge :: src -> dst -> dst

instance UpdatePair UserIncoming User where
  merge ui u = u & (firstname .~ (ui ^. firstname)) & (lastname .~ (ui ^. lastname))

instance UpdatePair RoleUpdate Role where
  merge ri r = r & (name .~ (ri ^. name)) & (permission .~ (ri ^. permission))
