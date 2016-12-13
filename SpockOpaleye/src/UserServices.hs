{-# LANGUAGE OverloadedStrings     #-}

module UserServices where

import           Control.Lens
import           ApiBase
import           Email
import           DataTypes
import qualified Data.Text                  as T
import           Data.Monoid
import           Control.Monad.IO.Class
import           Validations
import           TenantApi

doCreateTenant :: TenantIncoming -> AppM (Either T.Text (Auditable Tenant)) 
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
