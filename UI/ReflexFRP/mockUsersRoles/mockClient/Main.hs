{-# LANGUAGE ExplicitForAll, NoImplicitPrelude, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecursiveDo                  #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications                        #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import ClassyPrelude
import Data.Proxy
import Reflex
import Reflex.Dom
import Servant.API
import Servant.Reflex

import ReflexJsx

import MockAPI

main :: IO ()
main = mainWidget rolesPage

rolesPage :: MonadWidget t m => m ()
rolesPage = [jsx|
<body>
    <div>
        <nav class="navbar navbar-inverse navigation-clean-search">
            <div class="container">
                <div class="navbar-header"><a class="navbar-brand navbar-link" href="#">Tenant name comes here</a>
                    <button class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navcol-1"><span class="sr-only">Toggle navigation</span><span class="icon-bar"></span><span class="icon-bar"></span><span class="icon-bar"></span></button>
                </div>
                <div class="collapse navbar-collapse" id="navcol-1">
                    <ul class="nav navbar-nav">
                        <li class="active" role="presentation"><a href="#">Link 1</a></li>
                        <li role="presentation"><a href="#">Link 2</a></li>
                        <li role="presentation"><a href="#">Link 3</a></li>
                    </ul>
                    <form class="navbar-form navbar-left" target="_self">
                        <div class="form-group">
                            <label class="control-label" for="search-field"><i class="glyphicon glyphicon-search"></i></label>
                            <input class="form-control search-field" type="search" name="search" id="search-field"/>
                        </div>
                    </form>
                </div>
            </div>
        </nav>
    </div>
    <div></div>
    <div>
        <div class="container">
            <div class="row">
                <div class="col-md-3 secton-menu">
                    <ul class="nav nav-pills nav-stacked">
                        <li class="active"><a href="#">Account Settings</a></li>
                        <li><a href="#">Products </a></li>
                        <li><a href="#">Orders </a></li>
                    </ul>
                </div>
                <div class="col-md-9">
                    <ol class="breadcrumb">
                        <li><a><span>Account settings</span></a></li>
                        <li><a><span>Roles </span></a></li>
                    </ol>
                    <button class="btn btn-primary pull-right" type="button">New role</button>
                    <h1 class="page-heading">Roles </h1>
                    <div class="table-responsive">
                        <table class="table">
                            <thead>
                                <tr>
                                    <th>Role name</th>
                                    <th>Permissions </th>
                                    <th>Users </th>
                                </tr>
                            </thead>
                            <tbody>
                                <tr>
                                    <td>Account administrator<a href="role-edit.html"> (edit)</a></td>
                                    <td><em>All permissions</em></td>
                                    <td>
                                        <ul>
                                            <li>admin@mydomain.com </li>
                                            <li>otheradmin@mydomain.com <a href="#">(revoke) </a></li>
                                            <li>yetanotheradmin@mydomain.com <a href="#">(revoke) </a></li>
                                        </ul>
                                    </td>
                                </tr>
                                <tr>
                                    <td>Product administrator <a href="role-edit.html">(edit) </a></td>
                                    <td>
                                        <ul>
                                            <li>View product</li>
                                            <li>Edit product textual content</li>
                                            <li>Edit product properties</li>
                                            <li>Edit product price</li>
                                            <li> <a href="#">+ 8 more</a></li>
                                        </ul>
                                    </td>
                                    <td>
                                        <ul>
                                            <li>user1@mydomain.com <a href="#">(revoke) </a></li>
                                            <li>user2@mydomain.com <a href="#">(revoke) </a></li>
                                            <li>user3@mydomain.com <a href="#">(revoke) </a></li>
                                        </ul>
                                    </td>
                                </tr>
                                <tr>
                                    <td>Product editor <a href="role-edit.html">(edit) </a></td>
                                    <td>
                                        <ul>
                                            <li>View product</li>
                                            <li>Edit product textual content</li>
                                            <li>Edit product images</li>
                                        </ul>
                                    </td>
                                    <td>
                                        <ul>
                                            <li>user4@mydomain.com <a href="#">(revoke) </a></li>
                                            <li>user3@mydomain.com <a href="#">(revoke) </a></li>
                                            <li>user7@mydomain.com <a href="#">(revoke) </a></li>
                                            <li> <a href="#">+ 5 more</a></li>
                                        </ul>
                                    </td>
                                </tr>
                            </tbody>
                        </table>
                    </div>
                </div>
            </div>
        </div>
    </div>
    <script src="assets/js/jquery.min.js"></script>
    <script src="assets/bootstrap/js/bootstrap.min.js"></script>
</body>
|]

--------------------------------------------------------------------------------
-- Old Code
--------------------------------------------------------------------------------

body :: forall t m. MonadWidget t m => m ()
body = do
  -- Instructions to use the server at localhost and to invoke the api
  let url = BaseFullUrl Http "localhost" 8081 ""
      (invokeAPI :<|> _ :<|> _) = client (Proxy @MockApi) (Proxy @m) (constDyn url)

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
