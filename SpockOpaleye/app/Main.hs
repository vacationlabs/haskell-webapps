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
-- import           Control.Exception.Lifted
import           Airbrake
import           Airbrake.WebRequest
import           Data.ByteString (ByteString)
import qualified Network.Wai as Network.Wai
-- import Data.HVect
import Control.Exception.Base (SomeException)
-- import Control.Monad.Trans.Control(MonadBaseControl)
import Control.Monad.Catch

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

-- runAppM :: AppM a -> Connection -> IO a
-- runAppM x conn = do
--   user <- getTestUser
--   runReaderT (runWriterT x) (conn, Just $ auditable getTestTenant, Just $ auditable user)

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


errorHandler :: (MonadIO m) => Network.Wai.Request -> SomeException -> ActionT m a
errorHandler req err = do
  -- notifyReq conf (waiRequestToRequest req) (Error "Uncaught exception" (T.pack $ show err)) (("Filename", 5):|[])
  -- throwIO err
  liftIO $ putStrLn "Make call to Airbrake here. `notifyReq` is bringing in a MonadThrow constraint which might be hard to typecheck without major refactoring of the app's codebase."
  return undefined
  where
    conf = airbrakeConf "61a1adfc070a9be9f21e43f586bbf5f7" "Env"

runWithLogging :: (MonadThrow m, MonadIO m) => ActionT m a -> ActionT m a
runWithLogging action = do
  req <- request
  -- NOTE: We need one of the following for this to work:
  --
  -- instance MonadCatch (ActionT m a)
  -- 
  -- a new function to replace `catchAll` that can catch errors in MonadIO
  catchAll action (errorHandler req)


-- NOTE: Similarly, we need `instance MonadThrow (ActionT m a)` I believe
jsonfail = do
  error "Hardcoded to error out"
  json (1::Int)

app :: SpockM Connection MySession MyAppState ()
app = do
  liftIO $ putStrLn "started..."
  get "jsonfail" (runWithLogging jsonfail)
