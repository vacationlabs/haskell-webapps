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
import           Data.Time
import           DBTypes
import           Domain.Product
import           Domain.Tenant
import           Environ
import           Models
import           Operation
import           ProductQuery
import           Servant
import           Servant.Server.Experimental.Auth.Cookie
import           Types


type TestAPI = API

testAPI :: Proxy TestAPI
testAPI = Proxy

newTenant :: TenantInput -> App (Headers '[Header "location" String] TenantId)
newTenant ti = do
    result <- runTransaction $ dbCreateTenant ti
    case result of
         Nothing -> throwError $ err400 { errBody = "Tenant already exists" }
         Just (tid, key) -> return $ addHeader (show tid) tid

getTenant :: TenantId -> App TenantOutput
getTenant tid = do
    result <- runTransaction $ dbGetTenant tid
    case result of
         Nothing -> throwError $ err404 { errBody = "Tenant doesn't exist" }
         Just t  -> return t

newSession :: LoginForm -> App (Headers '[Header "set-cookie" ByteString] ())
newSession login = do
    Config{..} <- ask
    loginValid <- validateLogin login
    inDevel $ liftIO $ print login
    time <- liftIO getCurrentTime
    case loginValid of
      Left _ -> throwError $ err400 { errBody = "Invalid login." }
      Right uid -> addSession authSettings randomSource serverKey (Session uid time) ()

testSessionHandler :: ServerT TestAPI App
testSessionHandler = (newTenant :<|> getTenant) :<|> newSession :<|> productHandler

productHandler :: ServerT (ProtectEndpoints ProductAPI) App
productHandler = productGetHandler
            :<|> productListHandler

productGetHandler :: ProductId -> Either CookieError User -> App Product
productGetHandler pid session = do
  case session of
       (Right user) -> do
                    result <- handleDBError $
                              handlePermissionError $
                              runTransaction $
                              runOperation (dbGetProduct pid) (Just user)
                    return result
       (Left _) -> throwError err403

handlePermissionError :: App (Either PermissionError a) -> App a
handlePermissionError x = do
  a <- x
  case a of
    (Left _)  -> throwError err403
    (Right a) -> return a

handleDBError :: App (Either DBError a) -> App a
handleDBError x = do
  a <- x
  case a of
    (Left _)  -> throwError err404
    (Right a) -> return a


productListHandler :: [ProductFilter] -> [ProductView] -> Either CookieError User -> App [Product]
productListHandler pfs pvs session =
  let pf = mconcat pfs
      pv = mconcat pvs
  in case session of
       (Right user) -> do
                    result <- handlePermissionError $
                              runTransaction $
                              (runOperation (dbGetProductList pf) $ Just user)
                    return result
       (Left _) -> throwError err403

testServer :: Config -> Server TestAPI
testServer config = enter (Nat $ flip runReaderT config) testSessionHandler

