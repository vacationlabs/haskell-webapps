{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE MultiParamTypeClasses   #-}

module JsonValidation where

import           Servant
import           Servant.Server.Internal
import           Data.Aeson
import           Network.Wai
import           Control.Monad.IO.Class
import           AppCore
import           AppM
import           Data.Text

import qualified Text.Digestive as DIG
import qualified Text.Digestive.Aeson as DIGA

class Validatable a where
  form :: DIG.Form Text IO a

validate :: (Validatable a) => Value -> IO (Either Int a)
validate v = do
  (_, result) <- DIGA.digestJSON form v
  return $ case result of
    Nothing -> Left 0
    Just b -> Right b

data ReqBodyVal a = ReqBodyVal a

instance Validatable UserIncoming where
  form = User () () () <$> (TenantId <$> "tenantid" DIG..:gtZero)
                       <*> "username" DIG..:nonEmptyText
                       <*> "password" DIG..:nonEmptyText
                       <*> pure Nothing
                       <*> pure Nothing
                       <*> pure ()

instance (Validatable a, HasServer sublayout context) => HasServer (ReqBodyVal a :> sublayout) context where
    type ServerT (ReqBodyVal a :> sublayout) m = a -> ServerT sublayout m

    route Proxy context subserver = do
        route (Proxy :: Proxy sublayout) context (addBodyCheck subserver bodyCheck)
      where
        bodyCheck :: DelayedIO a
        bodyCheck = withRequest $ \request -> do
          mrqBodyDecoded <- liftIO $ eitherDecode' <$> lazyRequestBody request 
          case mrqBodyDecoded of
            Left _ -> delayedFailFatal err415
            Right mrqBody -> do
              validate_result <- liftIO $ validate $ mrqBody
              case validate_result of 
                Left _ -> delayedFailFatal err415 { errBody = "Json failed to validate" }
                Right v -> return v
