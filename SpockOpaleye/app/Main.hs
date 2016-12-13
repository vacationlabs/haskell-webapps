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
import           Control.Monad.Trans.Except
import           Control.Exception.Lifted
import           Airbrake

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

runAppM :: AppM a -> Connection -> IO (AppResult a)
runAppM x conn = do
  putStrLn "request"
  user <- getTestUser
  r <- runExceptT $ handle throwE $ runReaderT (runWriterT x) (conn, Just $ auditable getTestTenant, Just $ auditable user)
  case r of 
    Right (item, lg) -> do
      return $ AppOk item
    Left ex -> do
      let message = T.pack $ show ex
      notify conf (Error "Uncaught exception" message) (("sdfsfd", 5):|[])
      return $ AppErr message
  where
      conf = airbrakeConf "61a1adfc070a9be9f21e43f586bbf5f7" "Env"

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
    AppOk tenants <- runQuery $ runAppM $ readTenants
    json tenants
  post ("tenants/new") $
    do
      maybeTenantIncoming <- jsonBody
      either_newtenant <- runQuery $ runAppM $ do
        case maybeTenantIncoming of
            Just incomingTenant -> doCreateTenant incomingTenant
            Nothing -> return $ Left $ T.pack "Unrecognized input"
      case either_newtenant of
        AppOk (Right new_tenant) -> json new_tenant
        AppOk (Left message)     -> json message
        AppErr _ -> json $ T.pack "There was an error. Please try again later"
