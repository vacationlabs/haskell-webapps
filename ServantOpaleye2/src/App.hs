{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

import           AppCore
import           AppM
import           Control.Monad.Reader
import           Control.Monad.Writer
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Either
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO
import           Control.Monad.IO.Class
import           Database.PostgreSQL.Simple (Connection)
import           Control.Exception (SomeException)
import           Control.Exception.Lifted (handle)
import           Data.Text.Lazy.Encoding
import           Data.Text.Lazy (pack)

-- * api

type TenantApi =
  "item" :> Get '[JSON] String

itemApi :: Proxy TenantApi
itemApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve itemApi server

server :: Server TenantApi
server =
  getTenants

getTenants :: Handler String
getTenants = return "Asdasd"

test :: AppM String
test = return "asasdd"

runAppM :: AppM a -> Connection -> IO (Either SomeException (a, String))
runAppM x conn = do
  user <- getTestUser
  runExceptT $ handle throwE $ runReaderT (runWriterT x) (conn, Just $ getTestTenant, Just $ user)

appmToEither :: Connection -> AppM  :~> EitherT ServantErr IO
appmToEither conn = Nat (appmToEither' conn)

appmToEither' :: forall a. Connection -> AppM a  -> EitherT ServantErr IO a
appmToEither' conn appm = do
  r <- liftIO $ runAppM appm conn
  case r of
    Right (a, log) -> return a
    Left exp -> throwError err500 { errBody = encodeUtf8.pack $ show exp}
