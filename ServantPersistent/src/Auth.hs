{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Auth
    where

import           Control.Lens
import           Control.Monad.Catch                     (try)
import           Control.Monad.Reader
import           Data.Text
import           Database.Persist
import           DBTypes

import           Domain.User
import           Models
import           Network.Wai
import           Operation
import           Servant
import           Servant.Server.Experimental.Auth
import           Servant.Server.Experimental.Auth.Cookie
import           Types

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
cookieAuthHandler config@Config{..} = mkAuthHandler $ \request -> flip runReaderT config $ do
    result <- lift $ try $ getSession authSettings serverKey request
    case result :: Either AuthCookieException (Maybe Session) of
         Left a -> return $ Left $ AuthError a
         Right Nothing -> return (Left NotPresent)
         Right (Just s) -> do
           user <-  runTransaction $ unsafeRunOperation $ dbGetUser (sessionUserID s)
           case user of
             Left _  -> return $ Left SessionInvalid
             Right u@UserB{_userStatus=ActiveU} -> return $ Right u
             Right _ -> return $ Left InactiveUser

data LoginError = UsernameNotFound Text
                | TenantInactive TenantId
                | WrongPassword

validateLogin :: LoginForm -> App (Either LoginError UserId)
validateLogin Login{..} =
  runTransaction $ do
    muser <- getBy (UniqueUsername loginUsername)
    case muser of
      Nothing -> return $ Left $ UsernameNotFound loginUsername
      Just Entity{entityVal=user, entityKey=uid} -> do
                 mtenant <- get (view dBUserTenantID user)
                 case mtenant of
                   Nothing -> return $ Left $ TenantInactive (view dBUserTenantID user)
                   Just tenant -> case view dBTenantStatus tenant of
                                    ActiveT -> if loginPassword == user ^. dBUserPassword
                                               then return $ Right $ uid
                                               else return $ Left WrongPassword
                                    _ -> return $ Left $ TenantInactive (user ^. dBUserTenantID)

