{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeFamilies #-}
module Auth
    where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import Servant.Server.Experimental.Auth
import Data.ByteString
import Data.Proxy
import GHC.Generics
import Data.Serialize
import Data.Text
import Control.Monad.Catch (try)
import Types

type family ProtectEndpoints a where
    ProtectEndpoints (a :<|> b) = (ProtectEndpoints a) :<|> (ProtectEndpoints b)
    ProtectEndpoints (a :> b) = a :> ProtectEndpoints b
    ProtectEndpoints a = AppAuth :> a

type AppAuth = AuthProtect "cookie-auth"

instance HasLink sub => HasLink (AppAuth :> sub) where
  type MkLink (AppAuth :> sub) = MkLink sub
  toLink _ = toLink (Proxy :: Proxy sub)

cookieAuthHandler ::  AuthCookieSettings -> ServerKey ->AuthHandler Request (Either CookieError Session)
cookieAuthHandler authSettings serverKey = mkAuthHandler $ \request -> do
    result <- try $ getSession authSettings serverKey request
    case result :: Either AuthCookieException (Maybe Session) of
         Left a -> return $ Left $ AuthError a
         Right a -> return $ maybe (Left NotPresent) Right a

type instance AuthCookieData = Either CookieError Session

validateLogin :: LoginForm -> App Bool
validateLogin _ = return True

