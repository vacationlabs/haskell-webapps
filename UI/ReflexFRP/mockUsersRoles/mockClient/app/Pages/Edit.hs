{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, RecursiveDo #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Pages.Edit where

import ClassyPrelude
import Control.Lens
import Reflex.Dom

import MockAPI
import Pages.Common
import Utils

import Reflex.Dom.Contrib.Widgets.DynamicList

editPage :: MonadWidget t m => RoleName -> RoleAttributes -> m (Event t (RoleName, RoleAttributes))
editPage roleName roleAttrs = do
  el "body" $ do
    pageHeader
    el "div" $
      divClass "container" $ do
        divClass "row" $ do
          lateralNavigation
          divClass "col-md-9" $ do
            sitePosition ["Account Settings", "Roles", "Edit role: " <> roleName]
            newData <- form roleName roleAttrs
            divClass "form-group" $ do
              btn <- buttonClass "btn btn-primary" "Save"
              elAttr "a" ("href"=:"#" <> "class"=:"cancel text-danger") $ text "cancel"
              return (tagPromptlyDyn newData btn)

form :: MonadWidget t m => RoleName -> RoleAttributes -> m (Dynamic t (RoleName, RoleAttributes))
form roleName roleAttrs =
  el "form" $ do
    newRoleName <- divClass "form-group" $ do
      elAttr "label" ("class"=:"control-label") $ text "Role name"
      fmap _textInput_value . textInput $ def
        & textInputConfig_initialValue .~ roleName
        & textInputConfig_attributes   .~ constDyn ("class"=:"form-control")
    newRolePerms <- divClass "form-group" $ do
      elClass "label" "control-label" $ text "Permissions"
      divClass "row" $ do
        pp <- permissionCheckboxes "Products" (roleAttrs^.rolePermission) (map PP [minBound..maxBound])
        op <- permissionCheckboxes "Orders"   (roleAttrs^.rolePermission) (map OP [minBound..maxBound])
        up <- permissionCheckboxes "Users"    (roleAttrs^.rolePermission) (map UP [minBound..maxBound])
        return (mconcat [pp, op, up])
    updatedUsers <- updateUsers (roleAttrs^.roleAssociatedUsers)
    return $ liftA2 (,) newRoleName (RoleAttributes <$> newRolePerms <*> updatedUsers)

updateUsers :: MonadWidget t m => Set User -> m (Dynamic t (Set User))
updateUsers users = setFromList . map fst <$$>
  (divClass "form-group" $ do
    elClass "label" "control-label" $ text "Users with this role:"
    do rec a <- el "ul" $
             dynamicList renderUser snd (const never) moreUsers (setToList users)
           moreUsers <- addAnotherUser
       return a)

addAnotherUser :: MonadWidget t m => m (Event t User)
addAnotherUser =
 divClass "row" $
   divClass "col-lg-6 col-md-8 col-sm-8 col-xs-12" $
     divClass "input-group" $ do
       divClass "input-group-addon" $ el "span" $ text "Add another user"
       userInput <- textInput $ def & (textInputConfig_attributes .~ constDyn ("class"=:"form-control"))
       let user = User <$> userInput ^. textInput_value
       addUser <- divClass "input-group-btn" $ buttonClass "btn btn-default" "Add"
       return (tagPromptlyDyn user addUser)

renderUser :: MonadWidget t m => Int -> User -> Event t User -> m (User, Event t ())
renderUser _ u _ =
  el "li" $ do
    text (userMail u)
    revokeUser <- link " (revoke)"
    return (u, _link_clicked revokeUser)

permissionCheckboxes :: MonadWidget t m => Text -> Set Permission -> [Permission] -> m (Dynamic t (Set Permission))
permissionCheckboxes groupName permissions groupToDisplay =
  divClass "col-lg-4 col-md-4 col-md-offset-0 col-sm-6 col-xs-12" $ do
    rec
      allGroup <- divClass "checkbox permission-group-heading" $
        elClass "label" "control-label" $ do
          let groupComplete = (\ps -> length ps == length groupToDisplay) <$> temp
          groupChecked <- _checkbox_change <$>
            checkbox (all (\p -> p `member` permissions) groupToDisplay)
                     (def & checkboxConfig_setValue .~ updated groupComplete)
          el "strong" $ text groupName
          return groupChecked
      temp <- fmap mconcat $ forM groupToDisplay $ \p ->
        divClass "checkbox" $
          elClass "label" "control-label" $ do
            temp1 <- _checkbox_value <$> checkbox (p `member` permissions) (def
                                          & checkboxConfig_setValue .~ allGroup)
            text (toUserLabel p)
            return $ bool (mempty :: Set Permission) (singletonSet p) <$> temp1
    return temp
