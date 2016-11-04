{-# LANGUAGE OverloadedStrings, NoImplicitPrelude #-}

-- {-# OPTIONS_GHC -fdefer-typed-holes #-}

module Pages.Edit where

import ClassyPrelude
import Reflex.Dom


import Permissions
import Pages.Common

editPage :: MonadWidget t m => m ()
editPage = do
  el "body" $ do
    pageHeader
    el "div" $
      elAttr "div" ("class"=:"container") $ do
        elAttr "div" ("class"=:"row") $ do
          lateralNavigation
          elAttr "div" ("class"=:"col-md-9") $ do
            sitePosition ["Account Settings", "Roles", "Edit role: Product Editor"]
            form
            elAttr "div" ("class"=:"form-group") $ do
              elAttr "button" ("class"=:"btn btn-primary" <> "type"=:"submit") $ text "Save"
              elAttr "a" ("href"=:"#" <> "class"=:"cancel text-danger") $ text "cancel"

form :: MonadWidget t m => m ()
form = do
  el "form" $ do
    divClass "form-group" $ do
      elAttr "label" ("class"=:"control-label") $ text "Role name"
      elAttr "input" ("class"=:"form-control" <> "type"=:"text" <> "value"=:"Product editor") $ pure ()
    divClass "form-group" $ do
      elAttr "label" ("class"=:"control-label") $ text "Permissions"
      divClass "row" $ do
        permissionCheckboxes "Products" (map PP [minBound..maxBound])
        permissionCheckboxes "Orders"   (map OP [minBound..maxBound])
        permissionCheckboxes "Users"    (map UP [minBound..maxBound])
    deleteUserWidget

usersWithThisRole :: MonadWidget t m => m ()
usersWithThisRole = do
    elClass "label" "control-label" $ text "Users with this role:"
    el "ul" $ do
      el "li" $ do
        text "user1@mydomain.com"
        elAttr "a" ("href"=:"#") $ text "(revoke)"
      el "li" $ do
        text "user2@mydomain.com"
        elAttr "a" ("href"=:"#") $ text "(revoke)"
      el "li" $ do
        text "user3@mydomain.com"
        elAttr "a" ("href"=:"#") $ text "(revoke)"
      el "li" $ do
        text "user4@mydomain.com"
        elAttr "a" ("href"=:"#") $ text "(revoke)"

deleteUserWidget :: MonadWidget t m => m ()
deleteUserWidget =
  divClass "form-group" $ do
    usersWithThisRole
    addAnotherUser

addAnotherUser :: MonadWidget t m => m ()
addAnotherUser =
 divClass "row" $ do
   divClass "col-lg-6 col-md-8 col-sm-8 col-xs-12" $ do
     divClass "input-group" $ do
       divClass "input-group-addon" $
         el "span" $ text "Add another user"
       elAttr "input" ("class"=:"form-control" <> "type"=:"text") $ pure ()
       divClass "input-group-btn" $
         void $ buttonClass "btn btn-default" "Add"

permissionCheckboxes :: MonadWidget t m => Text -> [Permission] -> m ()
permissionCheckboxes groupName permissions =
  divClass "col-lg-4 col-md-4 col-md-offset-0 col-sm-6 col-xs-12" $ do
    divClass "checkbox permission-group-heading" $
      elClass "label" "control-label" $ do
        _ <- checkbox False def
        el "strong" $ text groupName
    forM_ permissions $ \p ->
      divClass "checkbox" $
        elClass "label" "control-label" $ do
          _ <- checkbox False def
          text (toUserLabel p)
