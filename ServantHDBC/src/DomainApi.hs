module DomainApi where

import Data.List
import Data.Maybe
import Data.Time.Clock
import Database.HDBC 
import Database.HDBC.PostgreSQL
import Types
import Util

getTenant :: Connection -> Int -> IO (Maybe Tenant)
getTenant conn id = do
    select <- prepare conn "select id, created_at, updated_at, name, first_name, last_name, email, phone, status, owner_id, backoffice_domain from tenants where id = ? "
    execute select [toSql id]
    row <- fetchRow select
    let t = fmap (mkData11 Tenant) row
    return $ case t of
        Just (Just t) -> Just t
        _ -> Nothing

createTenant :: Connection -> Tenant -> IO (Maybe Int)
createTenant conn (Tenant _ created updated name first last email phone status owner backoffice)  = do
    putStrLn $ (show status) ++ " " ++ (show (toSql status))
    now <- getCurrentTime
    ins <- prepare conn "insert into tenants (created_at, updated_at, name, first_name, last_name, email, phone, owner_id, backoffice_domain, status) values (?, ?, ?, ?, ?, ?, ?, ?, ?, 'new') returning id"
    execute ins [toSql now, toSql now, toSql name, toSql first, toSql last, toSql email, toSql phone, toSql owner, toSql backoffice]
    result <- fetchRow ins
    commit conn
    putStrLn "done"
    return $ case result of
        Just (x:[]) -> Just (fromSql x)
        _ -> Nothing
    
    
activateTenant :: Connection -> Tenant -> User -> IO ()
activateTenant conn (Tenant id _ _ _ _ _ _ _ _ _ _) (User owner _ _ _ _ _ _ _ _ ) = do
    now <- getCurrentTime
    upd <- prepare conn "update tenants set status='active',updated_at=?,owner_id=?  where id = ?"
    execute upd [toSql now, toSql owner, toSql id]
    commit conn
    return ()


createUser :: Connection -> User -> IO (Maybe Int)
createUser conn (User _ _ _ tenant_id username password first last _) = do
    now <- getCurrentTime
    ins <- prepare conn "insert into users (created_at, updated_at, tenant_id, username, password, first_name, last_name, status) values (?, ?, ?, ?, ?, ?, ?, 'inactive') returning id;"
    execute ins [toSql now, toSql now, toSql tenant_id, toSql username, toSql password, toSql first, toSql last ]
    result <- fetchRow ins
    commit conn
    return $ case result of
        Just (x:[]) -> Just (fromSql x)
        _ -> Nothing

getUser :: Connection -> Int -> IO (Maybe User)
getUser conn id = do
    select <- prepare conn "select id, created_at, updated_at, tenant_id, username, password, first_name, last_name, status from users where id = ? "
    execute select [toSql id]
    row <- fetchRow select
    let u = fmap (mkData9 User) row
    return $ case u of
        Just (Just u) -> Just u
        _ -> Nothing


setUserStatus :: Connection -> Int -> String -> IO ()
setUserStatus conn id status = do
    now <- getCurrentTime
    upd <- prepare conn "update users set status=? ,updated_at=? where id = ?"
    execute upd [toSql status, toSql now, toSql id]
    commit conn
    return ()

activateUser:: Connection -> User -> IO ()
activateUser conn user = setUserStatus conn (_uid user) "active"

deactivateUser:: Connection -> User -> IO ()
deactivateUser conn user = setUserStatus conn (_uid user) "inactive"

createRole :: Connection -> Role -> IO (Maybe Int)
createRole conn (Role _ tenant_id name permissions _ _) = do
    now <- getCurrentTime
    ins <- prepare conn "insert into roles (tenant_id, name, permissions, created_at, updated_at) values (?, ?, ?, ?, ?) returning id;"
    execute ins [toSql tenant_id, toSql name, toSql permissions, toSql now, toSql now ]
    result <- fetchRow ins
    putStrLn $ show result
    commit conn
    return $ case result of
        Just (x:[]) -> Just (fromSql x)
        _ -> Nothing

getRole :: Connection -> Int -> IO (Maybe Role)
getRole conn id = do
    select <- prepare conn "select id, tenant_id, name, permissions, created_at , updated_at from roles where id = ? "
    execute select [toSql id]
    row <- fetchRow select
    let role = fmap (mkData6 Role) row
    return $ case role of
        Just (Just role) -> Just role
        _ -> Nothing

addRole :: Connection -> User -> Role -> IO ()
addRole conn user role = do
    ins <- prepare conn "insert into users_roles (user_id, role_id) values (?, ?);"
    execute ins [toSql $ _uid user, toSql $ _rid role]
    commit conn

removeRole :: Connection -> User -> Role -> IO ()
removeRole conn user role = do
    del <- prepare conn "delete from users_roles where user_id = ? and role_id = ?;"
    execute del [toSql $ _uid user, toSql $ _rid role]
    commit conn

getPermissions :: Connection -> User -> IO [Permission]
getPermissions conn user = do
    select <- prepare conn "select permissions from roles inner join users_roles on roles.id = users_roles.role_id where users_roles.user_id = ?"
    execute select [toSql $ _uid user]
    row <- fetchAllRows select
    return $ nub $ sort $ concatMap permissionsFromSql $ map fromSql $ concat row
