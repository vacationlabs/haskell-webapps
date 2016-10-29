{-# LANGUAGE Arrows, FlexibleInstances, FlexibleContexts,
  MultiParamTypeClasses, OverloadedStrings #-}

module RoleApi
  (
   create_role,
  )
  where

import Control.Arrow (returnA, (<<<))
import Database.PostgreSQL.Simple (Connection)
import DataTypes
import OpaleyeDef
import Opaleye
       (Column, restrict, (.==), (.<=), (.&&), (.<),
       (.===), (.++), Nullable,
        Query, PGInt4, runInsertMany, queryTable, constant,
        pgStrictText, runQuery)
import Opaleye.PGTypes
import GHC.Int
import Data.Text
import Data.List.NonEmpty

create_role
  :: Connection -> Role -> IO GHC.Int.Int64
create_role conn Role {role_id=id, role_tenantid=tenant_id, role_name=name, role_permission=(ph :| pl)} = 
  runInsertMany conn roleTable
  (return (
           constant id
          ,constant tenant_id
          ,pgStrictText name
          ,pgArray pgStrictText $ fmap to_text (ph:pl)
          ))
  where
  to_text :: Permission -> Text
  to_text (Permission name) = name
