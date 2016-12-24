{-# LANGUAGE OverloadedStrings #-}

module UserServices where

import           AppCore
import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Monoid
import qualified Data.Text              as T
import           Email
import           TenantApi
import           UserApi
import           Validations
import           Servant

doCreateTenant :: (DbConnection m) => TenantIncoming -> m (Either T.Text Tenant)
doCreateTenant  incomingTenant = do
  result <- validateIncomingTenant incomingTenant
  case result of
    Valid -> do
         newTenant <- createTenant incomingTenant
         liftIO $ sendTenantActivationMail newTenant
         return $ Right newTenant
    Invalid err -> return $ Left $ T.concat ["Validation fail with ", T.pack err]
