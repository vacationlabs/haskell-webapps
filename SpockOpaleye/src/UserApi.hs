{-# LANGUAGE Arrows, FlexibleInstances, FlexibleContexts,
  MultiParamTypeClasses, OverloadedStrings #-}

module UserApi
  (
   create_user
  )
  where


import           Control.Arrow (returnA, (<<<))
import Database.PostgreSQL.Simple (Connection)
import DataTypes
import OpaleyeDef
import Opaleye
       (Column, restrict, (.==), (.<=), (.&&), (.<),
       (.===), (.++), Nullable,
        Query, PGInt4, runInsertMany, queryTable, constant,
        pgStrictText, runQuery)
import qualified Opaleye.PGTypes as P
import GHC.Int
import Data.Text

create_user
  :: Connection -> User -> IO GHC.Int.Int64
create_user conn User{user_id = id, user_tenantid = tenant_id, user_username = username, user_firstname = first_name, user_lastname = last_name, user_status = status} = 
  runInsertMany conn userTable $
  (return (constant id
          ,constant tenant_id
          ,pgStrictText username
          ,pgStrictText username
          ,constant $ Just first_name
          ,constant $ Just last_name
          ,constant status
          ))
