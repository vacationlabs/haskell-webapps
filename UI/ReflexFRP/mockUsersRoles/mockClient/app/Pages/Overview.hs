{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

module Pages.Overview where

import MockAPI

import ClassyPrelude
import Reflex.Dom
import Reflex.Dom.Contrib.Widgets.DynamicList
import Pages.Common
import Control.Lens

import Utils

overview :: MonadWidget t m => Roles -> Roles -> m (Event t AppState)
overview serverState clientState = el "body" $ do
  pageHeader
  el "div" $ divClass "container" $ divClass "row" $ do
    lateralNavigation
    divClass "col-md-9" $ do
      sitePosition ["Account Settings", "Roles"]
      newRole <- buttonClass "btn btn-primary pull-right" "New Role"
      elClass "h1" "page-heading" $ text "Roles"
      change <- divClass "table-responsive" $ (tableSection serverState clientState)
      return $ leftmost [change, Edit serverState clientState ("New role", emptyRoleAttributes) <$ newRole]

tableSection :: MonadWidget t m => Roles -> Roles -> m (Event t AppState)
tableSection serverState clientState = elClass "table" "table" $ do
  el "thead" $ do
    el "tr" $ do
      el "th" $ text "Role name"
      el "th" $ text "Permissions"
      el "th" $ text "Users"
  el "tbody" $ do
    leftmost <$> mapM (roleSection serverState clientState) (clientState ^. _Wrapped' . to mapToList)

roleSection :: MonadWidget t m => Roles -> Roles -> (RoleName, RoleAttributes) -> m (Event t AppState)
roleSection serverState clientState (rolename, roleattrs) = el "tr" $ do
  edit <- el "td" $ do
    text rolename
    link " (edit)"
  _ <- el "td" $ el "em" $ permissionList (roleattrs ^. rolePermission)
  _ <- el "td" $ el "ul" $ listComponent (rolename, roleattrs)
  return (Edit serverState clientState (rolename, roleattrs) <$ _link_clicked edit)

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
