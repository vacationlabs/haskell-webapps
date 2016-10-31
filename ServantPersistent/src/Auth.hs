{-# LANGUAGE DataKinds       #-}
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
import Types

type instance AuthCookieData = Either CookieError Session

type family ProtectEndpoints a where
    ProtectEndpoints (a :<|> b) = (ProtectEndpoints a) :<|> (ProtectEndpoints b)
    ProtectEndpoints (a :> b) = a :> ProtectEndpoints b
    ProtectEndpoints a = AppAuth :> a

type AppAuth = AuthProtect "cookie-auth"

instance HasLink sub => HasLink (AppAuth :> sub) where
  type MkLink (AppAuth :> sub) = MkLink sub
  toLink _ = toLink (Proxy :: Proxy sub)

cookieAuthHandler ::  AuthCookieSettings -> ServerKey -> AuthHandler Request (Either CookieError Session)
cookieAuthHandler authSettings serverKey = mkAuthHandler $ \request -> do
    result <- try $ getSession authSettings serverKey request
    case result :: Either AuthCookieException (Maybe Session) of
         Left a -> return $ Left $ AuthError a
         Right a -> return $ maybe (Left NotPresent) Right a


validateLogin :: LoginForm -> App Bool
validateLogin _ = return True

