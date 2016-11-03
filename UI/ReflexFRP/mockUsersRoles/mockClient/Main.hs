{-# LANGUAGE AllowAmbiguousTypes, ExplicitForAll, FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction          #-}
{-# LANGUAGE OverloadedStrings, PartialTypeSignatures, QuasiQuotes #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, TypeApplications    #-}
{-# LANGUAGE TypeFamilies, TypeOperators                           #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import ClassyPrelude
import Data.Proxy
import Reflex
import Reflex.Dom
import Reflex.Dom.Xhr
import Servant.API
import Servant.Reflex
import Reflex.Dom.Contrib.Widgets.DynamicList
import Control.Lens
import Control.Lens.Wrapped

import ReflexJsx
import MockAPI
import ExRoles
import Permissions

main :: IO ()
main = mainWidget $ do
  rolesPage (tableSection exRoles)

displayRoles :: MonadWidget t m => m ()
displayRoles = do
  b <- button "show state"
  rolesResponse <- showRoles b
  result <- holdDyn "" $ fmap parseR rolesResponse
  el "h2" (dynText result)

url :: BaseUrl
url = BaseFullUrl Http "localhost" 8081 ""

apiClients :: forall t m. (MonadWidget t m) => _
apiClients = client (Proxy @MockApi) (Proxy :: Proxy m) (constDyn url)

deleteUser :: (MonadWidget t m) => Dynamic t (Either Text Text) -> Dynamic t (Either Text User) -> Event t () -> m (Event t (ReqResult NoContent))
showRoles :: (MonadWidget t m) => Event t () -> m (Event t (ReqResult Roles))
(deleteUser :<|> showRoles :<|> _) = apiClients

parseR :: (Show a) => ReqResult a -> Text
parseR (ResponseSuccess a _) = tshow a
parseR (ResponseFailure a _) = "ResponseFailure: " <> a
parseR (RequestFailure s)    = "RequestFailure: " <> s

-- @@ Markup

-- This function embeds the table, which is the main component of the page, in
-- the remaining of the page, read directly from the html of the mock page. I
-- feel that when https://github.com/dackerman/reflex-jsx/issues/2 will be
-- resolved, a much more enjoyable workflow will be opened to interact with
-- markup.
rolesPage :: MonadWidget t m => m () -> m ()
rolesPage table = [jsx|
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
                    {table}
                    </div>
                </div>
            </div>
        </div>
    </div>
    <script src="assets/js/jquery.min.js"></script>
    <script src="assets/bootstrap/js/bootstrap.min.js"></script>
</body>
|]

-- @@ Dynamic lists

-- Corrisponde a:
-- <table class="table">
--     <thead>
--         <tr>
--             <th>Role name</th>
--             <th>Permissions </th>
--             <th>Users </th>
--         </tr>
--     </thead>
--     <tbody>
--     ...
--     </tbody>
-- </table>
tableSection :: MonadWidget t m => Roles -> m ()
tableSection roles = elClass "table" "table" $ do
  el "thead" $ do
    el "tr" $ do
      el "th" $ text "Role name"
      el "th" $ text "Permissions"
      el "th" $ text "Users"
  el "tbody" $ do
    mapM_ roleSection (roles ^. _Wrapped' . to mapToList)

-- roleSection corrisponde a:
-- <tr>
--     <td>Account administrator<a href="role-edit.html"> (edit)</a></td>
--     <td><em>All permissions</em></td>
--     <td>
--         <ul>
--             <li>admin@mydomain.com </li>
--             <li>otheradmin@mydomain.com <a href="#">(revoke) </a></li>
--             <li>yetanotheradmin@mydomain.com <a href="#">(revoke) </a></li>
--         </ul>
--     </td>
-- </tr>
roleSection :: MonadWidget t m => (RoleName, RoleAttributes) -> m ()
roleSection (rolename, roleattrs) = const () <$$> el "tr" $ do
  el "td" $ text rolename
  el "td" $ el "em" $ permissionList (roleattrs ^. rolePermission)
  el "td" $ el "ul" $ listComponent (rolename, roleattrs)

permissionList :: forall t m. MonadWidget t m => Set Permission  -> m (Dynamic t [((), Event t ())])
permissionList ps =
  dynamicList displayPermission snd (const never) never (ps ^. to setToList)
  where
    displayPermission _ p _ = el "li" $ do
      text $ toUserLabel p
      return ((), never)

listComponent :: forall t m. MonadWidget t m => (RoleName, RoleAttributes) -> m (Dynamic t [((), Event t ())])
listComponent (rolename, roleattrs) =
  dynamicList (displayItem rolename)
              snd
              (const never)
              never
              (roleattrs ^. roleAssociatedUsers . to setToList)

-- In the original markup this was:
-- <li>otheradmin@mydomain.com <a href="#">(revoke) </a></li>
displayItem  :: MonadWidget t m => RoleName -> Int -> User -> Event t User -> m ((), Event t ())
displayItem rolename _ u _ = el "li" $ do
  text (userMail u <> " ")
  deleteEvent <- clickLabel "(revoke)"
  deleteConfirmed <- const () <$$> deleteUser (constDyn $ Right rolename) (constDyn $ Right u) deleteEvent
  return ((), deleteConfirmed)

clickLabel :: MonadWidget t m => Text -> m (Event t ())
clickLabel t = do
  (e, _) <- elAttr' "a" ("href" =: "#") (text t)
  return $ domEvent Click e

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
