{-# LANGUAGE FlexibleContexts       #-}
module Helpers where

import           CryptoDef
import           Data.Text
import           Ids
import           InternalUtils
import           UserDefs
import           RoleDefs
import           OpaleyeDef
import           ApiBase
import           Classes
import           Opaleye

fillPassword :: UserIncoming -> BcryptPassword -> UserPoly () () () TenantId Text BcryptPassword (Maybe Text) (Maybe Text) ()
fillPassword incUser hash = incUser { _userpolyPassword = hash }

linkUserRole :: (DbConnection m) => UserId -> RoleId -> m ()
linkUserRole userid roleid = do
  createDbRows userRolePivotTable [(constant userid, constant roleid)] :: (DbConnection m) => m [(UserId, RoleId)]
  return ()

unlinkUserRole :: (DbConnection m) => UserId -> RoleId -> m ()
unlinkUserRole userid roleid = do
  removeRawDbRows userRolePivotTable matchFunc
  return ()
  where
    matchFunc (r_uid, r_rid) = (r_uid .== (constant userid)) .&& (r_rid .== (constant roleid))
