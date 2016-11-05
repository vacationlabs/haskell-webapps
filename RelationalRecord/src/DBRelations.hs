{-# LANGUAGE LambdaCase, FlexibleContexts, DeriveFoldable #-}

module  DBRelations where

import  Types.Tenant    as Tenant
import  Types.User      as User
import  Types.Role      as Role


import Database.HDBC                (IConnection, SqlValue, SqlError, handleSql, commit)

import Database.Relational.Query

import Database.Record

import Database.HDBC.Record.Query   (runQuery)
import Database.HDBC.Record.Delete  (runDelete)


data DBResult a
    = ResEmpty
    | ResJust   a
    | ResMany   [a]
    | ResDBErr  SqlError
    deriving (Show, Eq, Foldable)


handle :: IO (DBResult a) -> IO (DBResult a)
handle = handleSql (return . ResDBErr)


dbQuery :: (IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
       => conn -> p -> Relation p a -> IO (DBResult a)
dbQuery conn param rel = handle $
    runQuery conn (relationalQuery rel) param >>= return . \case
        []  -> ResEmpty
        [r] -> ResJust r
        rs  -> ResMany rs

dbDelete :: (IConnection conn, ToSql SqlValue p)
     => conn -> p -> Delete p -> IO (DBResult Integer)
dbDelete conn param dlt = handle $ do
    num <- runDelete conn dlt param
    commit conn
    return $ if num > 0 then ResJust num else ResEmpty



-- Tenant

allTenants :: Relation () Tenants
allTenants = relation (query tenants)


getTenant :: Int -> Relation () Tenants
getTenant tenantId = relation $ do
    a       <- query tenants
    wheres  $ a ! Tenant.id' .=. value (fromIntegral tenantId)
    return  a


-- Role

getRole :: Int -> Relation () Roles
getRole roleId = relation $ do
    a       <- query roles
    wheres  $ a ! Role.id' .=. value (fromIntegral roleId)
    return  a

deleteRole :: Int -> Delete ()
deleteRole roleId =
    typedDelete tableOfRoles . restriction $ \projection ->
        wheres $ projection ! Role.id' .=. value (fromIntegral roleId)



-- User

getUser :: Int -> Relation () Users
getUser userId = relation $ do
    a       <- query users
    wheres  $ a ! User.id' .=. value (fromIntegral userId)
    return  a
