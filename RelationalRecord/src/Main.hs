{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import DataSource       (getDataSource)
import DomainAPI
import Relations.Tenant as Tenant hiding (getTenant)
import Relations.User   hiding (getUser)
import Relations.Role   hiding (getRole, assignRole, removeRole)
import Types.Tenant     as Tenant
import Types.Role       as Role
import Types.AuditLog
-- import Types.EnumDummy
import DBInterface

import Data.Aeson       (ToJSON(..))
import Data.Text        (Text, pack)
import System.Random    (randomRIO)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad
import Data.Maybe


someTenant :: TenantInsert
someTenant = TenantInsert
    "TestTenant" "Sylvain" "Duchamps" "sly@champsxxx.fr" "3980" Nothing ""

someUser :: UserInsert
someUser = UserInsert
    1 "" "testpass" (Just "tesss") (Just "usserrr")


randomText :: Bool -> IO Text
randomText onlyDigits = do
    len     <- randomRIO (12, 20)
    pack    <$> replicateM len ((chars !!) <$> randomRIO (0, length chars - 1))
  where
    chars = (if onlyDigits then [] else ['a'..'z']) ++ ['0'..'9']

printJson :: ToJSON a => DBUniqueResult a -> IO ()
printJson (Left err)  = print err
printJson (Right val) = BL.putStrLn $
    encodePretty' defConfig {confCompare = compare} val

printLog :: DBConnector -> IO ()
printLog conn =
    dbQuery conn auditLogs () >>= mapM_ print

main :: IO ()
main = do
    conn    <- getDataSource
    let conn' = DBConnector (Just 1) Nothing conn

    putStrLn "creating some user..."
    un <- randomText False
    u1 <- createUser conn' someUser {iUsername = un}
    printJson u1

    putStrLn "creating some tenant..."
    bd <- randomText False
    t1 <- createTenant conn' someTenant {iBOD = bd}
    printJson t1

    putStrLn $ "activating and deactivating tenant..."
    ~(Right t2)     <- getTenant conn' 1
    when (isNothing $ Tenant.ownerId t2) $ void $
        updateTenant conn' t2 t2 {Tenant.ownerId = Just 1}
    ~(Right t2')    <- activateTenant conn' t2
    deactivateTenant conn' t2' >>= printJson

    putStrLn $ "creating a role and assigning it..."
    ~(Right u2) <- getUser conn' 1
    ~(Right r1) <- createRole conn' (RoleInsert 1 "NEWROLE!!" "foo,bar")
    assignRole conn' u2 r1 >>= print

    putStrLn $ "now listing all user/role pairs, maybe with a tenant..."
    dbQuery conn' getUserTenantRoles () >>= mapM_ (printJson . Right)

    putStrLn $ "removing role assignment and deleting role..."
    removeRole conn' u2 r1 >>= print
    deleteRole conn' (Left $ Role.id r1) >>= print

    putStrLn $ "changing some tenant data explicitly"
    ~(Right t3) <- getTenant conn' 2
    nm <- randomText False
    ph <- randomText True
    _  <- updateTenant conn' t3 t3 {Tenant.phone = ph, Tenant.name = nm}

    putStrLn "...done"
