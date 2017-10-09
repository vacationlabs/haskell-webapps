{-# LANGUAGE ExplicitForAll, FlexibleContexts, NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures, RankNTypes, RecursiveDo      #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, TypeFamilies #-}
{-# LANGUAGE TypeOperators                                       #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import ClassyPrelude
import Data.Proxy
import Reflex
import Reflex.Dom
import Servant.API
import Servant.Reflex
import qualified Language.Javascript.JSaddle.Warp as JSWarp (run)

import MockAPI

main :: IO ()
main = JSWarp.run 8081 $ mainWidget body

url :: BaseUrl
url = BaseFullUrl Http "localhost" 8081 ""

allApi :: forall t m. (MonadWidget t m) => _
allApi = client (Proxy @MockApi) (Proxy :: Proxy m) (constDyn url)

invokeAPI :: forall t m. (MonadWidget t m) =>
             Dynamic t (Either Text User) -> Event t () -> m (Event t (ReqResult Text))
(invokeAPI :<|> _ :<|> _) = allApi

body :: forall t m. MonadWidget t m => m ()
body = do
  -- A description of the visual elements
  divClass "login-clean" $ do
    el "form" $ do
      rec hiddenTitle
          icon
          mail <- _textInput_value <$> mailInputElement
          pass <- _textInput_value <$> passInputElement
          let userResult = liftA2 (User) mail pass
          send <- buttonElement send responseEvent
          forgot
          -- The actual API call
          apiResponse <- invokeAPI (Right <$> userResult) send
          let responseEvent = const () <$> apiResponse
      -- A visual feedback on authentication
      r <- holdDyn "" $ fmap parseR apiResponse
      el "h2" (dynText r)

--------------------------------------------------------------------------------
-- Implementation of the visual elements:

hiddenTitle, icon :: DomBuilder t m => m ()
hiddenTitle = elClass "h2" "sr-only" (text "Login Form")
icon = divClass "illustration" (elClass "i" "icon ion-ios-navigate" $ pure ())

mailInputElement :: MonadWidget t m => m (TextInput t)
mailInputElement = textInput $
  def & textInputConfig_attributes .~ constDyn
        ("class" =: "form-control" <> "name" =: "email" <> "placeholder" =: "Email")
      & textInputConfig_inputType .~ "email"

passInputElement :: MonadWidget t m => m (TextInput t)
passInputElement = textInput $
  def & textInputConfig_attributes .~ constDyn
        ("class" =: "form-control" <> "name" =: "password" <> "placeholder" =: "Password")
      & textInputConfig_inputType .~ "password"

buttonElement :: DomBuilder t m => Event t () -> Event t () -> m (Event t ())
buttonElement disable enable = divClass "form-group" (styledButton conf "Log in")
  where
    conf = def & elementConfig_initialAttributes .~ initialAttr
               & elementConfig_modifyAttributes  .~ mergeWith (\_ b -> b)
                   [ const disableAttr <$> disable
                   , const enableAttr <$> enable ]
    initialAttr = "class" =: "btn btn-primary btn-block" <> "type" =: "button"
    disableAttr = fmap Just initialAttr  <> "disabled" =: Just "true"
    enableAttr  = fmap Just initialAttr  <> "disabled" =: Nothing

forgot :: DomBuilder t m => m ()
forgot = elAttr "a"
  ("href" =: "#" <> "class" =: "forgot")
  (text "Forgot your email or password?")

----- This function should be contributed back to reflex-frp
styledButton :: DomBuilder t m => ElementConfig EventResult t m -> Text -> m (Event t ())
styledButton conf t = do
  (e, _) <- element "button" conf (text t)
  return (domEvent Click e)


--------------------------------------------------------------------------------
-- Parse the response from the API
parseR :: ReqResult Text -> Text
parseR (ResponseSuccess a _) = a
parseR (ResponseFailure a _) = "ResponseFailure: " <> a
parseR (RequestFailure s) = "RequestFailure: " <> s
