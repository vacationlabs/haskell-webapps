module Utils where

import           CryptoDef
import           Data.Text
import           Ids
import           InternalUtils
import           UserDefs

fillPassword :: UserIncoming -> BcryptPassword -> UserPoly () () () TenantId Text BcryptPassword (Maybe Text) (Maybe Text) ()
fillPassword incUser hash = incUser { _userpolyPassword = hash }
