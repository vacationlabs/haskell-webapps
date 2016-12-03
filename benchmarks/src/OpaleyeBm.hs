{-# LANGUAGE Arrows                #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings       #-}

module OpaleyeBm
    ( 
     opaleye_benchmark
     ,clearTables
    ) where


import Opaleye
import Database.PostgreSQL.Simple
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

data UserPoly id name email = User { id :: id, name :: name, email :: email }

instance Default Constant Int (Maybe (Column PGInt4)) where
  def = Constant (\x -> Just $ pgInt4 x)

instance QueryRunnerColumnDefault PGInt4 (Maybe Int) where
  queryRunnerColumnDefault = Just <$> fieldQueryRunnerColumn

$(makeAdaptorAndInstance "pUser" ''UserPoly)

type UserPW = UserPoly (Maybe (Column PGInt4)) (Column PGText) (Column PGText)
type UserPR = UserPoly (Column PGInt4) (Column PGText) (Column PGText)
type User = UserPoly (Maybe Int) String String

userTable :: Table UserPW UserPR
userTable = Table "users" (pUser (User
  (optional "id")
  (required "name")
  (required "email")
  ))

tenantTable :: Table 
  (Maybe (Column PGInt4), Column PGText, Column PGText, Column PGInt4) 
  (Column PGInt4, Column PGText, Column PGText, Column PGInt4)
tenantTable = Table "tenants" (p4 (
  optional "id",
  required "name",
  required "email",
  required "owner_id"
  ))

getRows :: Connection -> IO [User]
getRows conn = do
  runQuery conn $ queryTable userTable

insertRow :: Connection -> User -> IO Int64
insertRow conn u = do
  runInsertMany conn userTable [(constant u)] 

insertRowReturning :: Connection -> User -> IO [User]
insertRowReturning conn u = do
  runInsertManyReturning conn userTable [(constant u)] (\x -> x)

updateRowReturning :: Connection -> User -> IO [User]
updateRowReturning conn row = do
  runUpdateReturning conn userTable (\_ -> constant row) (\(User id _ _) -> id .== (pgInt4 1)) (\x -> x)

twowayJoin :: Connection -> IO [(String, String, String, String)]
twowayJoin conn = do
  runQuery conn $ proc () ->
    do
      User u_id u_name u_email <- queryTable userTable -< ()
      (t_id, t_name, t_email, t_uid) <- queryTable tenantTable -< ()
      restrict -< (t_uid .== u_id)
      returnA -< (u_name, u_email, t_name, t_email)


opaleye_benchmark :: Connection -> IO ()
opaleye_benchmark conn = do
  let user = User Nothing "Max" "max@mail.com"
  defaultMainWith defaultConfig {resamples = 1000} [
      bench "Opaleye: getRows" $ nfIO $ replicateM_ 1000 $ getRows conn
    , bench "Opaleye: insertRow" $ nfIO $ replicateM_ 1000 $ insertRow conn user
    , bench "Opaleye: insertRowReturning" $ nfIO $ replicateM_ 1000 $ insertRowReturning conn user
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

