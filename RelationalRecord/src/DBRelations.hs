{-# LANGUAGE LambdaCase, FlexibleContexts, DeriveFoldable, ScopedTypeVariables #-}

module  DBRelations where

import  Types.Tenant    as Tenant
import  Types.User      as User
import  Types.Role      as Role


import Database.HDBC                (IConnection, SqlValue, SqlError, handleSql, commit)

import Database.Relational.Query

import Database.Record

import Database.HDBC.Record.Query   (runQuery)
import Database.HDBC.Record.Delete  (runDelete)
import Database.HDBC.Record.Update  (runUpdate)

import Data.Time.LocalTime

import Data.Int

data DBResult a
    = ResEmpty
    | ResJust   a
    | ResMany   [a]
    | ResDBErr  SqlError
    deriving (Show, Eq, Foldable)


type TimestampedUpdate a = Update (ZonedTime, a)


handle :: IO (DBResult a) -> IO (DBResult a)
handle = handleSql (return . ResDBErr)


dbQuery :: (IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
       => conn -> Relation p a -> p -> IO (DBResult a)
dbQuery conn rel param = handle $
    runQuery conn (relationalQuery rel) param >>= return . \case
        []  -> ResEmpty
        [r] -> ResJust r
        rs  -> ResMany rs

dbDelete :: (IConnection conn, ToSql SqlValue p)
     => conn -> Delete p -> p -> IO (DBResult Integer)
dbDelete conn dlt param = handle $ do
    num     <- runDelete conn dlt param
    commit  conn
    return  $ if num > 0 then ResJust num else ResEmpty

dbUpdate :: (IConnection conn, ToSql SqlValue p)
     => conn -> TimestampedUpdate p -> p -> IO (DBResult Integer)
dbUpdate conn upd param = handle $ do
    tNow    <- getZonedTime
    res     <- runUpdate conn upd (tNow, param)
    commit  conn
    return  $ ResJust res


-- Tenant

allTenants :: Relation () Tenants
allTenants = relation (query tenants)

getTenant :: Relation Int32 Tenants
getTenant = relation' . placeholder $ \tenId -> do
    a       <- query tenants
    wheres  $ a ! Tenant.id' .=. tenId
    return  a

updateTenant :: TimestampedUpdate Int32
updateTenant = derivedUpdate $ \projection -> do
    (phTStamp, _)   <- placeholder (\tStamp -> Tenant.updatedAt' <-# tStamp)
    (phTenId, _)    <- placeholder (\tenId -> wheres $ projection ! Tenant.id' .=. tenId)
    return          $ phTStamp >< phTenId


-- Role

allRoles :: Relation () Roles
allRoles = relation (query roles)

getRole :: Relation Int32 Roles
getRole = relation' . placeholder $ \rolId -> do
    a       <- query roles
    wheres  $ a ! Role.id' .=. rolId
    return  a

deleteRole :: Delete Int32
deleteRole = derivedDelete $ \projection ->
    fst <$> placeholder (\rolId -> wheres $ projection ! Role.id' .=. rolId)

deleteRoleByName :: Delete String
deleteRoleByName = derivedDelete $ \projection ->
    fst <$> placeholder (\rolName -> wheres $ projection ! Role.name' .=. rolName)


-- User

getUser :: Relation Int32 Users
getUser = relation' . placeholder $ \usrId -> do
    a       <- query users
    wheres  $ a ! User.id' .=. usrId
    return  a
