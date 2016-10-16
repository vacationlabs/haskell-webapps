{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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
import DBApi
import Data.Time
import Safe

type TestAPI = API

testAPI :: Proxy TestAPI
testAPI = Proxy

newTenant :: TenantInput -> App (Headers '[Header "location" String] TenantID)
newTenant ti = do
    time <- liftIO $ getCurrentTime
    let t :: Tenant NewT
        t = liftTB $ TB ti time time ()
    result <- dbCreateTenant t 
    case result of
         Nothing -> throwError $ err400 { errBody = "Tenant already exists" }
         Just id -> return $ addHeader (show id) id

getTenant :: TenantID -> App TenantOutput
getTenant tid = do
    result <- dbGetTenant tid
    case result of
         Nothing -> throwError $ err404 { errBody = "Tenant doesn't exist" }
         Just t -> return t

newSession :: LoginForm -> App (Headers '[Header "set-cookie" ByteString] ())
newSession login = do
    Config{..} <- ask
    loginValid <- validateLogin login
    inDevel $ liftIO $ print login
    let session = Session (username login)
    if loginValid
       then addSession authSettings randomSource serverKey session ()
       else throwError $ err400 { errBody = "Invalid login." }

testSessionHandler :: ServerT TestAPI App
testSessionHandler = (newTenant :<|> getTenant) :<|> newSession :<|> productHandler

productHandler :: ServerT (ProtectEndpoints ProductAPI) App
productHandler = (\id -> either handleError (liftIO . print))
            :<|> (either handleError $ const $ return [()])
  where handleError NotPresent = throwError $ err403 { errBody = "Please log in" }
        handleError _ = throwError $ err403 { errBody = "Session invalid" }

testServer :: Config -> Server TestAPI
testServer config = enter (Nat $ flip runReaderT config) testSessionHandler

