{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where
import           Database.PostgreSQL.Simple
import           DataTypes
import           JsonInstances              ()
import           TenantApi
import           Validations

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Text                  as T
import           Data.Time
import           Prelude                    hiding (id)
import Control.Lens
import CryptoDef

data MySession =
  EmptySession

data MyAppState = DummyAppState

connectDb :: IO Connection
connectDb = connect defaultConnectInfo { connectDatabase = "haskell-webapps" }

main :: IO ()
main = do
  spockCfg <-
    defaultSpockCfg
      EmptySession
      (PCConn $ ConnBuilder connectDb close (PoolCfg 10 10 10))
      DummyAppState
  runSpock 8080 (spock spockCfg app)

runAppM :: AppM a -> Connection -> IO a
runAppM x conn = do
  putStrLn "request"
  user <- getTestUser
  (item, lg) <- runReaderT (runWriterT x) (conn, Just $ auditable getTestTenant, Just $ auditable user)
  putStrLn lg
  return item

getTestTenant :: Tenant
getTestTenant = Tenant (TenantId 1) tz tz "tjhon" "John" "Jacob" "john@gmail.com" "2342424" TenantStatusNew Nothing "Bo domain"
  where
      tz = UTCTime {
        utctDay = ModifiedJulianDay {
          toModifiedJulianDay = 0
          }
        , utctDayTime = secondsToDiffTime 0
      }

getTestUser :: IO User
getTestUser = do
  Just password_ <- bcryptPassword "adsasda"
  return $ User (UserId 1) tz tz (TenantId 1) "John" password_  (Just "2342424") (Just "asdada") UserStatusActive
  where
      tz = UTCTime {
        utctDay = ModifiedJulianDay {
          toModifiedJulianDay = 0
          }
        , utctDayTime = secondsToDiffTime 0
      }


app :: SpockM Connection MySession MyAppState ()
app = do
  get ("tenants") $ do
    tenants <- runQuery $ runAppM $ readTenants
    json tenants
  post ("tenants/new") $
    do maybeTenantIncoming <- jsonBody
       case maybeTenantIncoming of
         Just incomingTenant -> do
           result <- runQuery $ runAppM $ validateIncomingTenant incomingTenant
           case result of
             Valid -> do
                  newTenant <- runQuery $ runAppM $ createTenant incomingTenant
                  let updatedTenant = newTenant & firstname .~ "Jake"
                  _ <- runQuery $ runAppM $ updateTenant updatedTenant
                  json newTenant
             Invalid err -> json $ T.pack ("Validation fail with " <> err)
         Nothing -> json $ T.pack "Unrecognized input"
