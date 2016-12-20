module Utils where

import UserDefs
import CryptoDef
import InternalUtils
import Data.Text
import Ids

fillPassword :: UserIncoming -> BcryptPassword -> UserPoly () () () TenantId Text BcryptPassword (Maybe Text) (Maybe Text) ()
fillPassword incUser hash = incUser { _userpolyPassword = hash }
