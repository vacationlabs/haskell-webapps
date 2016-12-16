{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module OpaleyeDef where

import           Data.List.NonEmpty
import           Data.Maybe
import           Data.Profunctor.Product
import qualified Data.Profunctor.Product.Default      as D
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           Data.Text
import           Data.Text.Encoding
import           Data.Time
import           Database.PostgreSQL.Simple.FromField
import           Opaleye

import           Control.Lens
import           Data.Vector

readOnly :: String -> TableProperties () (Column a)
readOnly = lmap (const Nothing) . optional

userRolePivotTable :: Table (Column PGInt4, Column PGInt4) (Column PGInt4, Column PGInt4)
userRolePivotTable = Table "users_roles" (p2 (required "user_id", required "role_id"))

auditTable :: Table (
    ()
  , Column PGInt4
  , Maybe (Column (Nullable PGInt4))
  , Maybe (Column PGBool)
  , Column PGInt4
  , Column PGText
  , Column PGText
  , Column PGJsonb
  , Maybe (Column PGTimestamptz))
  (
    Column PGInt4
  , Column PGInt4
  , Column (Nullable PGInt4)
  , Column PGBool
  , Column PGInt4
  , Column PGText
  , Column PGText
  , Column PGJsonb
  , Column PGTimestamptz)
auditTable = Table "audit_logs" (p9 (
      readOnly "id"
    , required "tenant_id"
    , optional "user_id"
    , optional "changed_by_system"
    , required "auditable_id"
    , required "auditable_table_name"
    , required "summary"
    , required "changes"
    , optional "created_at"
  ))

instance D.Default Constant () (Maybe (Column PGInt4)) where
  def = Constant (\_ -> Nothing)

instance D.Default Constant () (Maybe (Column PGText)) where
  def = Constant (\_ -> Nothing)

instance D.Default Constant Text (Column (Nullable PGText)) where
  def = Constant (toNullable.pgStrictText)

instance D.Default Constant () (Maybe (Column PGTimestamptz)) where
  def = Constant (\() -> Nothing)

instance D.Default Constant () (Column PGTimestamptz) where
  def = Constant (\() -> pgUTCTime defaultutc)
    where
      defaultutc = UTCTime {
        utctDay = ModifiedJulianDay {
          toModifiedJulianDay = 0
          }
        , utctDayTime = secondsToDiffTime 0
      }

instance D.Default Constant UTCTime (Maybe (Column PGTimestamptz)) where
  def = Constant (\time -> Just $ pgUTCTime time)

instance QueryRunnerColumnDefault PGTimestamptz (Maybe UTCTime) where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
