{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Server
    where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.ByteString
import Data.Proxy
import Control.Monad.Except
import Control.Monad.Reader
import Servant.Server.Experimental.Auth.Cookie
import Auth
import Types
import API
import Environ

type TestAPI = SessionAPI :<|> ProtectEndpoints ProductAPI

testAPI :: Proxy TestAPI
testAPI = Proxy


newSession :: LoginForm -> App (Headers '[Header "set-cookie" ByteString] ())
newSession login = do
    Config{..} <- ask
    loginValid <- validateLogin login
    inDevel $ liftIO $ print login
    let session = Session (username login)
    if loginValid
       then addSession authSettings randomSource serverKey session ()
       else error "Invalid Login"

testSessionHandler :: ServerT TestAPI App
testSessionHandler = newSession :<|> productHandler

productHandler :: ServerT (ProtectEndpoints ProductAPI) App
productHandler = (\id -> either handleError (liftIO . print))
            :<|> (\s -> return [()])
  where handleError NotPresent = throwError $ err403 { errBody = "Please log in" }
        handleError _ = throwError $ err403 { errBody = "Session invalid" }

testServer :: Config -> Server TestAPI
testServer config = enter (Nat $ flip runReaderT config) testSessionHandler

