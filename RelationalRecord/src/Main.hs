{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module Main where

import DataSource       (getDataSource)
import DomainAPI
import Relations.Tenant as Tenant hiding (getTenant)
import Relations.User   hiding (getUser)
import Relations.Role   (RoleAssignment(..), allRoles, allRoleAssignments)
import Relations.DB
import DBInterface

import Data.Default     (def)
import Data.Aeson       (ToJSON)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL


someTenant :: TenantInsert
someTenant = TenantInsert
    "TestTenant" "Sylvain" "Duchamps" "sly@champsxxx.fr" "3980" Nothing "sy.chasms.xxxx.fr"

someUser :: UserInsert
someUser = UserInsert
    1 "testueser2" "testpass" (Just "tesss") (Just "usserrr")


printJson :: ToJSON a => DBUniqueResult a -> IO ()
printJson (Left err)  = print err
printJson (Right val) = BL.putStrLn $
    encodePretty' defConfig {confCompare = compare} val


main :: IO ()
main = do
    conn    <- getDataSource
    let conn' = DBConnector (Just 1) Nothing conn

    createUser conn' someUser >>= print
    createTenant conn' someTenant >>= print
    activateTenant conn' 2 >>= print
    assignRole conn' (RoleAssignment 1 1) >>= print

    getTenant conn' 1 >>= printJson
    getUser conn' 1 >>= printJson

    deleteRole conn' (Left 4) >>= print

    dbQuery conn' allRoles () >>= print

    dbQuery conn' allTenants () >>= print

    _ <- updateTenant conn' 2 def {Tenant.uName = NewVal "asd"}

    _ <- updateTenant conn' 2 def {uPhone = NewVal "4578453453", Tenant.uName = NewVal "asdkl"}

    _ <- removeRole conn' (RoleAssignment 1 1)
    dbQuery conn' allRoleAssignments () >>= print

    putStrLn "...done"
