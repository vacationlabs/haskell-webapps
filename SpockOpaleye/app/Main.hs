{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where
import           AppCore
import           Database.PostgreSQL.Simple
import           JsonInstances              ()
import           TenantApi
import           Validations

import           Email
import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Control.Monad.Reader as R
import qualified Data.Text                  as T
import           Data.Time
import           Prelude                    hiding (id)
import           UserServices
import           Control.Monad.Trans.Except
import           Control.Exception.Lifted
import           Airbrake
import           Airbrake.WebRequest
import           Data.ByteString (ByteString)

data MySession = EmptySession

data MyAppState = DummyAppState

data AppResult a = AppOk a | AppErr T.Text

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
  user <- getTestUser
  r <- runExceptT $ handle throwE $ runReaderT (runWriterT x) (conn, Just $ getTestTenant, Just $ user)
  case r of 
    Right (item, lg) -> do
      return $ AppOk item
    Left ex -> do
      let message = T.pack $ show ex
      return $ AppErr message

runWithLogging :: ActionT (WebStateM Connection MySession MyAppState) (AppResult a)
               -> (a -> ActionT (WebStateM Connection MySession MyAppState) ())
               -> ActionT (WebStateM Connection MySession MyAppState) ()
runWithLogging act sact = do
  r <- act
  case r of
    AppOk rOk -> sact rOk
    AppErr msg -> do
      r <- request
      liftIO $ notifyReq conf (waiRequestToRequest r) (Error "Uncaught exception" msg) (("Filename", 5):|[])
      json $ T.pack "There was an error. Please try again later"
  where
      conf = airbrakeConf "61a1adfc070a9be9f21e43f586bbf5f7" "Env"

app :: SpockM Connection MySession MyAppState ()
app = do
  get ("tenants") $ do
    runWithLogging (runQuery $ runAppM readTenants) (\tenants ->json tenants)
  post ("tenants/new") $ runWithLogging (do
      maybeTenantIncoming <- jsonBody
      runQuery $ runAppM $ do
        case maybeTenantIncoming of
            Just incomingTenant -> doCreateTenant incomingTenant
            Nothing -> return $ Left $ T.pack "Unrecognized input"
      ) (\et -> case et of
        Right new_tenant -> json new_tenant
        Left message     -> json message)
