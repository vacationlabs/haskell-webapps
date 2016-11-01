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
import           Domain.Product
import           Environ
import           Servant
import           Servant.Server.Experimental.Auth.Cookie
import           Types
import ProductQuery
import Operation


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
    let session = Session undefined
    if loginValid
       then addSession authSettings randomSource serverKey session ()
       else throwError $ err400 { errBody = "Invalid login." }

testSessionHandler :: ServerT TestAPI App
testSessionHandler = (newTenant :<|> getTenant) :<|> newSession :<|> productHandler

productHandler :: ServerT (ProtectEndpoints ProductAPI) App
productHandler = productGetHandler
            :<|> productListHandler

productGetHandler :: ProductID -> Either CookieError User -> App Product
productGetHandler pid session = do
  case session of
       (Right user) -> do
                    result <- handleDBError $
                              handlePermissionError $
                              runExceptT $
                              runOperation (dbGetProduct pid) user
                    return result
       (Left _) -> throwError err403

handlePermissionError :: App (Either PermissionError a) -> App a
handlePermissionError x = do
  a <- x
  case a of
    (Left _) -> throwError err403
    (Right a) -> return a

handleDBError :: App (Either DBError a) -> App a
handleDBError x = do
  a <- x
  case a of
    (Left _) -> throwError err404
    (Right a) -> return a


productListHandler :: [ProductFilter] -> [ProductView] -> Either CookieError User -> App [Product]
productListHandler pfs pvs session =
  let pf = mconcat pfs
      pv = mconcat pvs
  in case session of
       (Right user) -> do
                    result <- handlePermissionError (runExceptT $ runOperation (dbGetProductList pf) user)
                    return result
       (Left _) -> throwError err403

testServer :: Config -> Server TestAPI
testServer config = enter (Nat $ flip runReaderT config) testSessionHandler

