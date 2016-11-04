{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}
-- {-# OPTIONS_GHC -fdefer-typed-holes #-}

module Pages.Overview where

import MockAPI
import Permissions

import ClassyPrelude
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.DynamicList
import Pages.Common
import Control.Lens
import Control.Lens.Wrapped

import Utils

overview :: MonadWidget t m => m () -> m (Event t ())
overview table = el "body" $ do
  pageHeader
  mainContentWith table

mainContentWith :: MonadWidget t m => m a -> m (Event t ())
mainContentWith table =
  el "div" $ divClass "container" $ divClass "row" $ do
    lateralNavigation
    divClass "col-md-9" $ do
      sitePosition
      newRole <- styledButton "New Role"
      elClass "h1" "page-heading" $ text "Roles"
      elClass "div" "table-responsive" $ table
      return newRole

sitePosition :: MonadWidget t m => m ()
sitePosition =
  elAttr "ol" ("class"=:"breadcrumb") $ do
    el "li" $ el "a" $ el "span" $ text "Account settings"
    el "li" $ el "a" $ el "span" $ text "Roles"

styledButton :: MonadWidget t m => Text -> m (Event t ())
styledButton t = do
  (b, _) <- elAttr' "button"
                    ("class" =: "btn btn-primary pull-right" <> "type" =: "button")
                    (text t)
  return (domEvent Click b)

tableSection :: MonadWidget t m => Roles -> m ()
tableSection roles = elClass "table" "table" $ do
  el "thead" $ do
    el "tr" $ do
      el "th" $ text "Role name"
      el "th" $ text "Permissions"
      el "th" $ text "Users"
  el "tbody" $ do
    mapM_ roleSection (roles ^. _Wrapped' . to mapToList)

roleSection :: MonadWidget t m => (RoleName, RoleAttributes) -> m ()
roleSection (rolename, roleattrs) = const () <$$> el "tr" $ do
  el "td" $ text rolename
  el "td" $ el "em" $ permissionList (roleattrs ^. rolePermission)
  el "td" $ el "ul" $ listComponent (rolename, roleattrs)

permissionList :: MonadWidget t m => Set Permission  -> m (Dynamic t [((), Event t ())])
permissionList ps =
  dynamicList displayPermission snd (const never) never (ps ^. to setToList)
  where
    displayPermission _ p _ = el "li" $ do
      text $ toUserLabel p
      return ((), never)

listComponent :: MonadWidget t m => (RoleName, RoleAttributes) -> m (Dynamic t [((), Event t ())])
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
