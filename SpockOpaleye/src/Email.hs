{-# LANGUAGE OverloadedStrings       #-}

module Email where

import Network.Mail.SMTP
import Network.Mail.Mime

import Sendgrid (apikey)


testmail :: IO ()
testmail = do
  let address = Address { addressName = Just "Sandeep.C.R", addressEmail = "sandeepcr2@gmail.com"}
  let mail = simpleMail' address address "test mail" "test content"
  sendMailWithLogin' "smtp.sendgrid.net" 587 "apikey" apikey mail
  return ()


