{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

module App where

import           AppCore
import           AppM
import           Control.Exception          (SomeException)
import           Control.Exception.Lifted   (handle)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Except
import           Control.Monad.Writer
import           Data.Aeson
import           Data.Default
import           Data.Text.Lazy             (pack)
import           Data.Text.Lazy.Encoding
import           Database.PostgreSQL.Simple hiding ((:.))
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           TenantApi

import           Servant.Server.Experimental.Auth (AuthHandler)
import           Servant.Server.Experimental.Auth.Cookie
import           Crypto.Random (drgNew)

import           JsonValidation

import qualified Endpoints.Authentication as AuthenticationEp
import qualified Endpoints.Tenant as TenantEp
import qualified Endpoints.User as UserEp
import qualified Endpoints.Role as RoleEp

connectDb :: IO Connection
connectDb = connect defaultConnectInfo { connectDatabase = "haskell-webapps" }

-- * api

type Api = AuthenticationEp.Type :<|> TenantEp.Type :<|> UserEp.Type :<|> RoleEp.Type


api :: Proxy Api
api = Proxy

-- * app

--authCheck :: BasicAuthCheck String
--authCheck =
--  let check (BasicAuthData username password) =
--        return (Unauthorized)
--  in BasicAuthCheck check
--
--basicAuthServerContext :: Context (BasicAuthCheck String ': '[])
--basicAuthServerContext = authCheck :. EmptyContext

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  conn <- connectDb
  runSettings settings =<< (mkApp conn)

mkApp :: Connection -> IO Application
mkApp conn = do
  rs <- mkRandomSource drgNew 1000
  sk <- mkServerKey 16 Nothing
  let settings = def
  return $ serveWithContext 
                            api
                            ((defaultAuthHandler settings sk :: AuthHandler Request CookieData) :. EmptyContext)
                            (server conn settings {acsCookieFlags = []} rs sk)

server :: Connection -> AuthCookieSettings -> RandomSource -> ServerKey -> Server Api
server conn ac rs sk = enter (appmToServantM conn) (appMServerT ac rs sk)

appMServerT :: AuthCookieSettings -> RandomSource -> ServerKey -> ServerT Api AppM
appMServerT ac rs sk = (AuthenticationEp.server ac rs sk) :<|> TenantEp.server :<|> UserEp.server :<|> RoleEp.server

runAppM :: AppM a -> Connection -> IO (Either SomeException (a, String))
runAppM x conn = do
  user <- getTestUser
  runExceptT $ handle throwE $ runReaderT (runWriterT x) (conn, Just $ getTestTenant, Just $ user)

appmToServantM :: Connection -> (AppM  :~> ExceptT ServantErr IO)
appmToServantM conn = Nat (appmToServantM' conn)

appmToServantM' :: forall a. Connection -> AppM a  -> ExceptT ServantErr IO a
appmToServantM' conn appm = do
  r <- liftIO $ runAppM appm conn
  case r of
    Right (a, log) -> return a
    Left exp -> throwError err500 { errBody = encodeUtf8.pack $ show exp}
