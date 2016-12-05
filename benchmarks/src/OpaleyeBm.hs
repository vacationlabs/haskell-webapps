{-# LANGUAGE Arrows                #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings       #-}

module OpaleyeBm
    ( 
     opaleye_benchmark
     ,clearTables
    ) where


import Opaleye
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Data.Profunctor.Product (p3, p4)
import Data.Profunctor.Product.Default
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)

import Database.PostgreSQL.Simple

import GHC.Int

import Criterion.Main
import Criterion.Types (Config(..))
import Control.Arrow

import Control.Monad
import Data.Monoid
import Prelude hiding (id)

data UserId = UserId Int
data TenantId = TenantId Int

data UserPoly id name email = User { id :: id, name :: name, email :: email }
data TenantPoly id name email owner_id = Tenant { tenant_id :: id, tenant_name :: name, tenant_email :: email, tenant_owner_id :: owner_id }

instance Default Constant UserId (Column PGInt4) where
  def = Constant (\(UserId x) -> pgInt4 x)

instance Default Constant (Maybe UserId) (Maybe (Column PGInt4)) where
  def = Constant def'
    where
      def' (Just x) = Just $ constant x
      def' Nothing =  Nothing

instance Default Constant UserId (Maybe (Column PGInt4)) where
  def = Constant (\x -> Just $ constant x)

instance FromField UserId where
  fromField field mb = UserId <$> fromField field mb

instance QueryRunnerColumnDefault PGInt4 (Maybe UserId) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromField TenantId where
  fromField field mb = TenantId <$> fromField field mb

instance QueryRunnerColumnDefault PGInt4 (Maybe TenantId) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

$(makeAdaptorAndInstance "pUser" ''UserPoly)
$(makeAdaptorAndInstance "pTenant" ''TenantPoly)

type UserPW = UserPoly (Maybe (Column PGInt4)) (Column PGText) (Column PGText)
type UserPR = UserPoly (Column PGInt4) (Column PGText) (Column PGText)
type User = UserPoly (Maybe UserId) String String

type TenantPW = TenantPoly (Maybe (Column PGInt4)) (Column PGText) (Column PGText) (Column PGInt4)
type TenantPR = TenantPoly (Column PGInt4) (Column PGText) (Column PGText) (Column PGInt4)
type Tenant = TenantPoly (Maybe TenantId) String String Int

userTable :: Table UserPW UserPR
userTable = Table "users" (pUser (User
  (optional "id")
  (required "name")
  (required "email")
  ))

tenantTable :: Table TenantPW TenantPR
tenantTable = Table "tenants" (pTenant Tenant {
  tenant_id = optional "id",
  tenant_name = required "name",
  tenant_email = required "email",
  tenant_owner_id = required "owner_id"
  })

getRows :: Connection -> IO [User]
getRows conn = do
  runQuery conn $ queryTable userTable

insertRow :: Connection -> User -> IO Int64
insertRow conn u = do
  runInsert conn userTable (constant u) 

insertRowReturning :: Connection -> User -> IO [Int]
insertRowReturning conn u = do
  runInsertReturning conn userTable (constant u) id

updateRow :: Connection -> User -> IO Int64
updateRow conn row = do
  runUpdate conn userTable (\_ -> constant row) (\(User id _ _) -> id .== (pgInt4 1))


updateRowReturning :: Connection -> User -> IO [User]
updateRowReturning conn row = do
  runUpdateReturning conn userTable (\_ -> constant row) (\(User id _ _) -> id .== (pgInt4 1)) (\x -> x)

twowayJoin :: Connection -> IO [(User, Tenant)]
twowayJoin conn = do
  runQuery conn $ proc () ->
    do
      user@User { id=u_id } <- queryTable userTable -< ()
      tenant@Tenant { tenant_owner_id = t_uid } <- queryTable tenantTable -< ()
      restrict -< (t_uid .== u_id)
      returnA -< (user, tenant)


opaleye_benchmark :: Connection -> IO ()
opaleye_benchmark conn = do
  let user = User Nothing "Max" "max@mail.com"
  defaultMainWith defaultConfig {resamples = 1000} [
      bench "Opaleye: getRows" $ nfIO $ replicateM_ 1000 $ getRows conn
    , bench "Opaleye: insertRow" $ nfIO $ replicateM_ 1000 $ insertRow conn user
    , bench "Opaleye: insertRowReturning" $ nfIO $ replicateM_ 1000 $ insertRowReturning conn user
    , bench "Opaleye: updateRow" $ nfIO $ replicateM_ 1000 $ updateRow conn user
    , bench "Opaleye: updateRowReturning" $ nfIO $ replicateM_ 1000 $ updateRowReturning conn user
    , bench "Opaleye: twowayJoin" $ nfIO $ replicateM_ 1000 $ twowayJoin conn 
    ]


clearTables :: Connection -> IO ()
clearTables conn = do
  execute_ conn "truncate table users"
  execute_ conn "alter sequence \"users_id_seq\" restart with 1"
  let user = User Nothing "Max" "max@mail.com"
  st <- insertRow conn user
  return ()

