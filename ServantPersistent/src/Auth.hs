{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Auth
    where

import Servant
import Network.Wai
import Servant.Server.Experimental.Auth.Cookie
import Servant.Server.Experimental.Auth
import Control.Monad.Catch (try)
import Control.Monad.Trans
import Control.Monad.Reader
import Types
import DBTypes
import Models
import Database.Persist
import Operation
import Domain.User

type instance AuthCookieData = Either CookieError User

type family ProtectEndpoints a where
    ProtectEndpoints (a :<|> b) = (ProtectEndpoints a) :<|> (ProtectEndpoints b)
    ProtectEndpoints (a :> b) = a :> ProtectEndpoints b
    ProtectEndpoints a = AppAuth :> a

type AppAuth = AuthProtect "cookie-auth"

instance HasLink sub => HasLink (AppAuth :> sub) where
  type MkLink (AppAuth :> sub) = MkLink sub
  toLink _ = toLink (Proxy :: Proxy sub)

cookieAuthHandler ::  Config -> AuthHandler Request (Either CookieError User)
cookieAuthHandler config@(Config{..}) = mkAuthHandler $ \request -> flip runReaderT config $ do
    result <- lift $ try $ getSession authSettings serverKey request
    case result :: Either AuthCookieException (Maybe Session) of
         Left a -> return $ Left $ AuthError a
         Right Nothing -> return (Left NotPresent)
         Right (Just s) -> do
           user <- unsafeRunOperation $ dbGetUser (sessionUserID s)
           case user of
             Left _ -> return $ Left NotPresent
             Right x -> return $ Right x


validateLogin :: LoginForm -> App Bool
validateLogin _ = return True

