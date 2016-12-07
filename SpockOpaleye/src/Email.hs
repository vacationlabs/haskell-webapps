{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Email where

import           Network.Mail.Mime
import           Network.Mail.SMTP hiding (simpleMail)

import           ApiBase
import           Conf              (apikey)
import           Control.Lens
import           Data.ByteString   hiding (putStrLn)
import           Data.Monoid
import           Data.String.Here
import qualified           Data.Text as T
import           DataTypes

sendgridMail :: Mail -> IO ()
sendgridMail mail = sendMailWithLogin' "smtp.sendgrid.net" 587 "apikey" apikey mail

setCid :: T.Text -> T.Text -> Mail -> Mail
setCid filename cid mail@Mail {mailParts = alternatives} = mail { mailParts = (setCidInAlternative filename cid) <$> alternatives}
  where
  setCidInAlternative :: T.Text -> T.Text -> Alternatives -> Alternatives
  setCidInAlternative fp cid parts = (setCidForPart filename cid) <$> parts
  setCidForPart :: T.Text -> T.Text -> Part -> Part
  setCidForPart filename cid p@Part{partFilename = Just fn, partHeaders = ph} =
    if fn == filename then p{partHeaders = (makeContentIdHeader cid):ph} else p
  setCidForPart filename cid  p = p
  makeContentIdHeader :: T.Text -> (ByteString, T.Text)
  makeContentIdHeader cid = ("Content-ID", T.concat ["<", cid,  ">"])

addLogo :: Mail -> IO Mail
addLogo mail = do
  mail_with_attachment <- addAttachment "image/png" "apple.png" mail
  return $ setCid "apple.png" "logocid" mail_with_attachment

sendTenantActivationMail :: Auditable Tenant -> IO ()
sendTenantActivationMail newTenant = do
  makeMail from to activation_link >>= addLogo >>= sendgridMail
  where
    activation_link =  "<a href='#'>Click here</a>"
    from = Address { addressName = Just "VacationLabs", addressEmail = "webapps@vacationlabs.com"}
    to = Address { addressName = Just $ T.concat [tenant_fname, " ", tenant_lname], addressEmail = tenant_email}
    tenant_fname = (newTenant ^. firstname )
    tenant_lname = (newTenant ^. lastname )
    tenant_email = (newTenant ^. email )
    makeMail :: Address -> Address -> T.Text -> IO Mail
    makeMail from to activation_link  = simpleMail to from subject text html []
      where
      subject = "Registration Email from abc.com"
      text = [iTrim|
  Hi,
     Thank you for your registration. You can login
  at www.abc.com using your username and password.
  Hope to see you soon.
  Regards,
  abc.com|]
      html = [template|email-templates/tenant-activation.tpl|]
