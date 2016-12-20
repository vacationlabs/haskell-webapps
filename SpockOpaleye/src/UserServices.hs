{-# LANGUAGE OverloadedStrings     #-}

module UserServices where

import           AppCore
import           Control.Lens
import           Email
import qualified Data.Text                  as T
import           Data.Monoid
import           Control.Monad.IO.Class
import           Validations
import           TenantApi

doCreateTenant :: (DbConnection m, MonadIO m) => TenantIncoming -> m (Either T.Text Tenant) 
doCreateTenant  incomingTenant = do
  result <- validateIncomingTenant incomingTenant
  case result of
    Valid -> do
         newTenant <- createTenant incomingTenant
         f <- return (head [])
         liftIO $ putStrLn f
         liftIO $ sendTenantActivationMail newTenant
         return $ Right newTenant
    Invalid err -> return $ Left $ T.concat ["Validation fail with ", T.pack err]
