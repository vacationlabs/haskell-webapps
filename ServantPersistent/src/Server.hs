{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Server
    where

import           API
import           Auth
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.ByteString
import           DBTypes
import           Domain.Tenant
import           Environ
import           Servant
import           Servant.Server.Experimental.Auth.Cookie
import           Types


type TestAPI = API

testAPI :: Proxy TestAPI
testAPI = Proxy

newTenant :: TenantInput -> App (Headers '[Header "location" String] TenantID)
newTenant ti = do
    result <- dbCreateTenant ti
    case result of
         Nothing -> throwError $ err400 { errBody = "Tenant already exists" }
         Just tid -> return $ addHeader (show tid) tid

getTenant :: TenantID -> App TenantOutput
getTenant tid = do
    result <- dbGetTenant tid
    case result of
         Nothing -> throwError $ err404 { errBody = "Tenant doesn't exist" }
         Just t  -> return t

newSession :: LoginForm -> App (Headers '[Header "set-cookie" ByteString] ())
newSession login = do
    Config{..} <- ask
    loginValid <- validateLogin login
    inDevel $ liftIO $ print login
    let session = Session (loginUsername login)
    if loginValid
       then addSession authSettings randomSource serverKey session ()
       else throwError $ err400 { errBody = "Invalid login." }

testSessionHandler :: ServerT TestAPI App
testSessionHandler = (newTenant :<|> getTenant) :<|> newSession :<|> productHandler

productHandler :: ServerT (ProtectEndpoints ProductAPI) App
productHandler = (\_ -> either handleError (liftIO . print))
            :<|> (either handleError $ const $ return [()])
  where handleError NotPresent = throwError $ err403 { errBody = "Please log in" }
        handleError _ = throwError $ err403 { errBody = "Session invalid" }

testServer :: Config -> Server TestAPI
testServer config = enter (Nat $ flip runReaderT config) testSessionHandler

