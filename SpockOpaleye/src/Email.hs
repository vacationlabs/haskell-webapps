{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE QuasiQuotes         #-}

module Email where

import Network.Mail.SMTP hiding (simpleMail)
import Network.Mail.Mime

import Conf (apikey)
import Data.Text
import Data.String.Here

sendTenantActivation :: Text -> Text -> IO ()
sendTenantActivation to_addr activation_link = do
  let from = Address { addressName = Just "Sandeep.C.R", addressEmail = "sandeepcr2@gmail.com"}
  let to = Address { addressName = Just to_addr, addressEmail = "saurabh@vacationlabs.com"}
  mail <- makeMail from to
  --mail_with_attachment <- addAttachment "Test attache" "tips.txt" mail
  sendMailWithLogin' "smtp.sendgrid.net" 587 "apikey" apikey mail
  return ()
  where
    makeMail :: Address -> Address -> IO Mail
    makeMail from to  = simpleMail to from subject text html []
    subject = "Registration Email from abc.com"
    text = [iTrim|
Hi,
   Thank you for your registration. You can login
at www.abc.com using your username and password.
Hope to see you soon.
Regards,
abc.com|]
    html = [template|email-templates/tenant-activation.tpl|]
