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

import           Email
import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Reader
import           Control.Monad.Writer
import           CryptoDef
import qualified Data.Text                  as T
import           Data.Time
import           Prelude                    hiding (id)
import           UserServices
import           Network.HTTP.Types.Status
import           Airbrake
import           Airbrake.WebRequest

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
  runSpock 8080 (spock spockCfg {spc_errorHandler=errorHandler} app)

runAppM :: AppM a -> Connection -> IO a
runAppM x conn = do
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

errorHandler :: Status -> ActionCtxT () IO ()
errorHandler status = do
  r <- request
  liftIO $ notifyReq conf (waiRequestToRequest r) (Error "Uncaught exception" (T.pack $ show status)) (("Filename", 5):|[])
  json $ T.pack "There was an error. Please try again later"
  where
      conf = airbrakeConf "61a1adfc070a9be9f21e43f586bbf5f7" "Env"

app :: SpockM Connection MySession MyAppState ()
app = do
  get ("tenants") $ do
    tenants <- runQuery $ runAppM $ readTenants
    json tenants
  post ("tenants/new") $
    do
      maybeTenantIncoming <- jsonBody
      either_newtenant <- runQuery $ runAppM $ do
        case maybeTenantIncoming of
            Just incomingTenant -> doCreateTenant incomingTenant
            Nothing -> return $ Left $ T.pack "Unrecognized input"
      case either_newtenant of
        Right new_tenant -> json new_tenant
        Left message     -> json message
