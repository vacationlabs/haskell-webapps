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
         f <- return (head [])
         liftIO $ putStrLn f
         liftIO $ sendTenantActivationMail newTenant
         return $ Right newTenant
    Invalid err -> return $ Left $ T.concat ["Validation fail with ", T.pack err]

--doAuthenticate :: (DbConnection m) => T.Text -> T.Text -> m Bool
--doAuthenticate username pass = do
--  users <- readUserByName username
--  -- FIXME: do this in constant time 
--  if (checkPassword users) 
--    then addHeader "SetCookie" ("asdasdadad"::String)
--    else addHeader "SetCookie" (""::String)
--  where
--    checkPassword :: [User] -> Bool
--    checkPassword (u:_) = verifyPassword pass (u ^. password)
--    checkPassword [] = False
