{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module RoleDefs where

import           Auditable
import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.List.NonEmpty
import           Data.Maybe
import qualified Data.Profunctor.Product.Default      as D
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           Data.Text
import           Data.Text.Encoding
import           Data.Time                            (UTCTime)
import           Data.Vector
import           Database.PostgreSQL.Simple.FromField
import           Ids
import           Opaleye
import           OpaleyeDef
import           GHC.Generics

newtype RoleId = RoleId Int
  deriving (Show)

newtype RoleName = RoleName Text deriving (Show)

data RolePoly key tenant_id name permission created_at updated_at  = Role {
    _rolepolyId         :: key
  , _rolepolyTenantid   :: tenant_id
  , _rolepolyName       :: name
  , _rolepolyPermission :: permission
  , _rolepolyCreatedat  :: created_at
  , _rolepolyUpdatedat  :: updated_at
} deriving (Show, Generic)

$(makeAdaptorAndInstance "pRole" ''RolePoly)

type InternalRole = RolePoly RoleId TenantId RoleName (NonEmpty Permission) UTCTime UTCTime
type Role = Auditable InternalRole
type RoleIncoming = RolePoly () TenantId RoleName (NonEmpty Permission) () ()

type RoleTableW = RolePoly
  ()
  (Column PGInt4)
  (Column RoleName)
  (Column (PGArray PGText))
  (Maybe (Column PGTimestamptz)) -- createdAt
  (Column PGTimestamptz) -- updatedAt

type RoleTableR = RolePoly
  (Column PGInt4)
  (Column PGInt4)
  (Column RoleName)
  (Column (PGArray PGText))
  (Column PGTimestamptz) -- createdAt
  (Column PGTimestamptz) -- updatedAt

roleTable :: Table RoleTableW RoleTableR
roleTable = Table "roles" (pRole Role {
  _rolepolyId = (readOnly "id"),
  _rolepolyTenantid = required "tenant_id",
  _rolepolyName = required "name",
  _rolepolyPermission = required "permissions",
  _rolepolyCreatedat = optional "created_at",
  _rolepolyUpdatedat = required "updated_at"
  })

instance D.Default Constant RoleId (Column PGInt4) where
  def = Constant def'
    where
      def' :: RoleId -> (Column PGInt4)
      def' (RoleId id') = pgInt4 id'

instance D.Default Constant RoleId () where
  def = Constant (\_ -> ())

instance FromField RoleId where
  fromField f mdata = do
    x <- fromField f mdata
    return $ RoleId x

instance QueryRunnerColumnDefault PGInt4 RoleId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance D.Default Constant (NonEmpty Permission) (Column (PGArray PGText)) where
  def = Constant def'
    where
      def' :: (NonEmpty Permission) -> (Column (PGArray PGText))
      def' (ph :| pl) = pgArray pgStrictText $ toText <$> (ph : pl)
        where
          toText :: Permission -> Text
          toText Read   = "Read"
          toText Create = "Create"
          toText Update = "Update"
          toText Delete = "Delete"

instance QueryRunnerColumnDefault (PGArray PGText) (NonEmpty Permission) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromField Permission where
  fromField _ mdata = return $ makePermission mdata
    where
      makePermission (Just x) = toPermission $ decodeUtf8 x
      makePermission Nothing  = error "No data read from db"

toPermission :: Text -> Permission
toPermission "Read"   = Read
toPermission "Create" = Create
toPermission "Update" = Update
toPermission "Delete" = Delete
toPermission _        = error "Unrecognized permission"

instance FromField [Permission] where
  fromField f mdata =  (fmap toPermission) <$> Data.Vector.toList <$> fromField f mdata

instance FromField (NonEmpty Permission) where
  fromField f mdata = (fromJust.nonEmpty) <$> (fromField f mdata)

instance QueryRunnerColumnDefault PGText Permission where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
--

data Permission = Read | Create | Update | Delete
  deriving (Show)

instance ToJSON RoleId where
  toJSON (RoleId x) = toJSON x

instance ToJSON Permission where
  toJSON x = toJSON $ show x

instance (ToJSON a) => ToJSON (Auditable a) where
  toJSON Auditable {_data = x} = toJSON x

instance FromJSON RoleName where
  parseJSON j = RoleName <$> (parseJSON j)

instance ToJSON RoleName where
  toJSON (RoleName x) = toJSON x

instance FromField RoleName where
  fromField f mdata = RoleName <$> (fromField f mdata)

instance QueryRunnerColumnDefault RoleName RoleName where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance D.Default Constant RoleName (Column RoleName) where
  def = Constant def'
    where
      def' :: RoleName -> (Column RoleName)
      def' (RoleName x) = unsafeCoerceColumn $ pgStrictText x

instance ToJSON InternalRole where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = (fmap Data.Char.toLower).removePrefix }
    where
      removePrefix = Prelude.drop 11

instance FromJSON Permission where
  parseJSON t = toPermission <$> parseJSON t

instance FromJSON RoleIncoming where
  parseJSON (Object v) = Role () 
                              <$> v .: "Tenantid"
                              <*> v .: "rolename"
                              <*> (Data.List.NonEmpty.fromList <$> (v .: ("permissions"::Text)))
                              <*> (pure ())
                              <*> (pure ())
