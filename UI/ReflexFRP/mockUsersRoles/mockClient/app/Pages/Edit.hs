{-# LANGUAGE DataKinds, FlexibleContexts, NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings, RankNTypes, RecursiveDo, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Pages.Edit where

import ClassyPrelude
import Control.Lens
import Reflex.Dom
import Prelude ()
import MockAPI
import Pages.Common
import Utils

import ReflexJsx

import Reflex.Dom.Contrib.Widgets.DynamicList

editPage :: MonadWidget t m => RoleName -> RoleAttributes -> m (Event t (RoleName, RoleAttributes))
editPage roleName roleAttrs = do
  el "body" $ do
    template
    el "div" $
      divClass "container" $ do
        divClass "row" $ do
          lateralNavigation
          divClass "col-md-9" $ do
            sitePosition ["Account Settings", "Roles", "Edit role: " <> roleName]
            form roleName roleAttrs

saveButtonWidget :: MonadWidget t m => m (Event t ())
saveButtonWidget = do
  [jsx| <div class="form-group">
            {buttonClass "btn btn-primary" "Save"}
            <a href="#" class="cancel text-danger">cancel </a>
        </div> |]

roleNameWidget :: MonadWidget t m => RoleName  -> m (Dynamic t RoleName)
roleNameWidget roleName = do
  rec (rawRoleName, _) <- [jsx|
          <div {...maybeErrorAttr}>
            <label class="control-label">Role name</label>
            {fmap _textInput_value . textInput $ def
              & textInputConfig_initialValue .~ roleName
              & textInputConfig_attributes   .~ constDyn ("class"=:"form-control")}
            <span class="help-block">{dynText helpMsg}</span>
          </div>|]
      let helpMsg = bool "Role name can't be blank" "" . (/="") <$> rawRoleName
          maybeErrorAttr = ffor rawRoleName $ \e ->
                              if e /= ""
                              then ("class"=:"form-group")
                              else ("class"=:"form-group has-error")
  return rawRoleName


rolePermsWidget :: MonadWidget t m => RoleAttributes -> m (Dynamic t (Set Permission))
rolePermsWidget roleAttrs = do
  (pp, op, up) <- [jsx|
      <div class="form-group">
          <label class="control-label">Permissions </label>
          <div class="row">
            {permissionCheckboxes "Products" (roleAttrs^.rolePermission) (map PP [minBound..maxBound])}
            {permissionCheckboxes "Orders"   (roleAttrs^.rolePermission) (map OP [minBound..maxBound])}
            {permissionCheckboxes "Users"    (roleAttrs^.rolePermission) (map UP [minBound..maxBound])}
          </div>
      </div>|]
  return (mconcat [pp, op, up])

form :: MonadWidget t m => RoleName -> RoleAttributes -> m (Event t (RoleName, RoleAttributes))
form roleName roleAttrs = do
  rec _ <- [jsx| <div {...dangerclass}>
                     <span>Please fix the errors highlighted in <strong>red</strong> below:</span>
                        <ul>
                            <li>
                                {dynText dangerText}
                            </li>
                        </ul>
                    </div>|]

      (newRoleName, newRolePerms, updatedUsers, saveEvent) <-
        [jsx| <form>
                  {roleNameWidget roleName}
                  {rolePermsWidget roleAttrs}
                  {updateUsers (roleAttrs^.roleAssociatedUsers)}
                  {saveButtonWidget}
              </form>|]
      let dangerText = bool "" "You must select at least one permission for this role"
                             . (== mempty) <$> newRolePerms
          dangerclass = bool ("class"=:"alert alert-danger"<>"hidden"=:"true")
                             ("class"=:"alert alert-danger"<>"role"=:"alert")
                             . (== mempty) <$> newRolePerms
          role = liftA2 (,) newRoleName (RoleAttributes <$> newRolePerms <*> updatedUsers)
  return $ tagPromptlyDyn role saveEvent

usersDynList :: MonadWidget t m => Set User -> Event t User -> m (Dynamic t (Set User))
usersDynList users addAnotherUser = setFromList . map fst <$$> do
  dynamicList renderUser snd (const never) addAnotherUser (setToList users)

updateUsers :: MonadWidget t m => Set User -> m (Dynamic t (Set User))
updateUsers users = do
  rec
    (newSet,rawUserDyn,addButton,_) <- [jsx|
        <div {...maybeErrorAttr}>
            <label class="control-label">Users with this role</label>
            <ul>
                {usersDynList users validatedUserEvent}
            </ul>
            <div class="row">
                <div class="col-lg-6 col-md-8 col-sm-8 col-xs-12">
                    <div class="input-group">
                        <div class="input-group-addon"><span>Add another user</span></div>
                        {User <$$> textInputClassValue "form-control"}
                        <div class="input-group-btn">
                            {buttonClass "btn btn-default" "Add"}
                        </div>
                    </div>
                </div>
            </div>
            <span {...maybeHiddenAttr}>
                {dynText helpMsg}
            </span>
        </div>|]

    userOrError <- updatedOnButton addButton (Left noUserError) (validateUser <$> rawUserDyn)

    let (_,validatedUserEvent) = fanEither $ tagPromptlyDyn (validateUser <$> rawUserDyn) addButton

        maybeHiddenAttr = ffor userOrError $ \e ->
                            if isRight e || e == Left noUserError
                            then ("class"=:"help-block hidden")
                            else ("class"=:"help-block")

        maybeErrorAttr = ffor userOrError $ \e ->
                            if isRight e || e == Left noUserError
                            then ("class"=:"form-group")
                            else ("class"=:"form-group has-error")

        helpMsg = either (maybe "" id . userShapedMail) (const "") <$> userOrError

  return newSet


updatedOnButton :: (MonadWidget t m) => Event t b -> a -> Dynamic t a -> m (Dynamic t a)
updatedOnButton btn init dyn = holdDyn init (tagPromptlyDyn dyn btn)


textInputClassValue :: MonadWidget t m => Text -> m (Dynamic t Text)
textInputClassValue c = view textInput_value <$>
  textInput (def & textInputConfig_attributes .~ constDyn ("class"=:c))


isRight = either (const False) (const True)


type Markup = forall t m a. (MonadWidget t m) => m a -> m a


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


dangerDiv :: MonadWidget t m => m ()
dangerDiv = do
  elAttr "div" ("class"=:"alert alert-danger" <> "role"=:"alert") $ do
    el "span" $ do
      text "Please fix the errors highlighted in "
      el "strong" $ text "red "
      text "below:"
    el "ul" $ do
      el "li" $ text "Error not related to a specific field #1, eg. you must select at least one permission for this role"
      el "li" $ text "Error not related to a specific field #2"
