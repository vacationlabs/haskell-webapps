{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Email where

import           AppCore
import           Network.Mail.Mime
import           Network.Mail.SMTP  hiding (simpleMail)

import           Conf               (apikey)
import           Control.Concurrent
import           Control.Lens
import           Data.ByteString    hiding (putStrLn)
import           Data.Monoid
import           Data.String
import           Data.String.Here
import qualified Data.Text          as T
import qualified Data.Text.Lazy     as LT
import qualified Data.ByteString.Lazy as L
import           System.FilePath (takeFileName)

sendgridMail :: Mail -> IO ()
sendgridMail mail = do
  threadId <- forkIO $ sendMailWithLogin' "smtp.sendgrid.net" 587 "apikey" apikey mail
  putStrLn $ show $ T.concat ["Sending mail from thread ", fromString $ show threadId]

addLogo :: Mail -> IO Mail
addLogo mail = addAttachmentCid "image/png" "apple.png" "logocid@haskellwebapps.com" mail

sendTenantActivationMail :: Tenant -> IO ()
sendTenantActivationMail newTenant = do
  makeMail from to activationLink >>= addLogo >>= sendgridMail
  where
    -- @TODO Make key random
    key :: T.Text
    key = "cmFuZG9tdyBlaXJqd28gZWlyandvZWlyaiB3b2VyaWpvd2Vpcmogb3F3ZWlyb3F3ZWl1aHIgb3dxZXVoaXJ3b2Vpcmggb3dldWZob2kgcmV1d2ZpcmVxdWZoaXFld3VyaG9wIHF3dWh3aQ=="
    activationLink :: T.Text
    activationLink =  T.concat ["http://haskellwebapps.vacationlabs.com/activateTenantKey/", key]
    from = Address { addressName = Just "VacationLabs", addressEmail = "webapps@vacationlabs.com"}
    to = Address { addressName = Just $ T.concat [tenantFname, " ", tenantLname], addressEmail = tenantEmail}
    tenantFname = (newTenant ^. firstname )
    tenantLname = (newTenant ^. lastname )
    tenantEmail = (newTenant ^. email )
    makeMail :: Address -> Address -> T.Text -> IO Mail
    makeMail from to activationLink  = simpleMail to from subject text (LT.fromStrict html) []
      where
      subject = "Registration Email from abc.com"
      text = [template|email-templates/tenant-activation-text.tpl|]
      html :: T.Text
      html = [template|email-templates/tenant-activation.tpl|]
