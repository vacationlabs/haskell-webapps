{-# LANGUAGE DataKinds, FlexibleContexts, NoMonomorphismRestriction   #-}
{-# LANGUAGE OverloadedStrings, RankNTypes, RecursiveDo, TypeFamilies #-}
{-# LANGUAGE QuasiQuotes, NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

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
    pageHeader
    el "div" $
      divClass "container" $ do
        divClass "row" $ do
          lateralNavigation
          divClass "col-md-9" $ do
            sitePosition ["Account Settings", "Roles", "Edit role: " <> roleName]
            dangerDiv
            newData <- form roleName roleAttrs
            divClass "form-group" $ do
              btn <- buttonClass "btn btn-primary" "Save"
              elAttr "a" ("href"=:"#" <> "class"=:"cancel text-danger") $ text "cancel"
              return (tagPromptlyDyn newData btn)

form :: MonadWidget t m => RoleName -> RoleAttributes -> m (Dynamic t (RoleName, RoleAttributes))
form roleName roleAttrs =
  el "form" $ do
    newRoleName <- divClass "form-group has-error" $ do
      elAttr "label" ("class"=:"control-label") $ text "Role name"
      newRoleName <- fmap _textInput_value . textInput $ def
        & textInputConfig_initialValue .~ roleName
        & textInputConfig_attributes   .~ constDyn ("class"=:"form-control")
      elClass "span" "help-block" $ text "Role name can't be blank"
      return newRoleName
    newRolePerms <- divClass "form-group" $ do
      elClass "label" "control-label" $ text "Permissions"
      divClass "row" $ do
        pp <- permissionCheckboxes "Products" (roleAttrs^.rolePermission) (map PP [minBound..maxBound])
        op <- permissionCheckboxes "Orders"   (roleAttrs^.rolePermission) (map OP [minBound..maxBound])
        up <- permissionCheckboxes "Users"    (roleAttrs^.rolePermission) (map UP [minBound..maxBound])
        return (mconcat [pp, op, up])
    updatedUsers <- updateUsers' (roleAttrs^.roleAssociatedUsers)
    return $ liftA2 (,) newRoleName (RoleAttributes <$> newRolePerms <*> updatedUsers)

-- updateUsers :: MonadWidget t m => Set User -> m (Dynamic t (Set User))
-- updateUsers users = setFromList . map fst <$$>
--   (divClass "form-group has-error" $ do
--     elClass "label" "control-label" $ text "Users with this role:"
--     do rec a <- el "ul" $
--              dynamicList renderUser snd (const never) moreUsers (setToList users)
--            moreUsers <- addAnotherUser
--        return a)

usersDynList :: MonadWidget t m => Set User -> Event t User -> m (Dynamic t (Set User))
usersDynList users addAnotherUser = setFromList . map fst <$$> do
  dynamicList renderUser snd (const never) addAnotherUser (setToList users)

updateUsers' :: MonadWidget t m => Set User -> m (Dynamic t (Set User))
updateUsers' users = do
  rec
    (newSet,rawInputDyn,addButton) <- [jsx|
<div {...maybeErrorAttr}>
    <label class="control-label">Users with this role</label>
    <ul>
        {usersDynList users validatedUserEvent}
    </ul>
    <div class="row">
        <div class="col-lg-6 col-md-8 col-sm-8 col-xs-12">
            <div class="input-group">
                <div class="input-group-addon"><span>Add another user</span></div>
                {textInputClassValue "form-control"}
                <div class="input-group-btn">
                    {buttonClass "btn btn-default" "Add"}
                </div>
            </div>
        </div>
    </div>
    <span {...maybeHiddenAttr}>
        That doesn't look like a valid user. Does the user exist?
    </span>
</div>|]
    let userDyn = User <$> rawInputDyn
        (_,validatedUserEvent) = fanEither $ tagPromptlyDyn (validateUser' <$> userDyn) addButton
    eventualError <- updatedOnButton addButton noUserError (validateUser <$> userDyn)
    let maybeHiddenAttr = bool ("class"=:"help-block") ("class"=:"help-block hidden")
                        . (== noUserError) <$> eventualError
        maybeErrorAttr = bool ("class"=:"form-group has-error") ("class"=:"form-group")
                        . (== noUserError) <$> eventualError
        helpMsg = userShapedMail <$> eventualError
  return newSet

updatedOnButton :: (MonadWidget t m) => Event t b -> a -> Dynamic t a -> m (Dynamic t a)
updatedOnButton btn init dyn = holdDyn init (tagPromptlyDyn dyn btn)

textInputClassValue :: MonadWidget t m => Text -> m (Dynamic t Text)
textInputClassValue c = view textInput_value <$>
  textInput (def & textInputConfig_attributes .~ constDyn ("class"=:c))

isRight = either (const False) (const True)

filterLefts :: (Reflex t) => Event t (Either a b) -> Event t a
filterLefts ev = fforMaybe ev selectLeft
  where
    selectLeft (Left a)  = Just a
    selectLeft (Right _) = Nothing

filterRights :: (Reflex t) => Event t (Either a b) -> Event t b
filterRights ev = fforMaybe ev selectRight
  where
    selectRight (Left _)  = Nothing
    selectRight (Right a) = Just a

type Markup = forall t m a. (MonadWidget t m) => m a -> m a

nestedDivClasses :: MonadWidget t m => [Text] -> m a -> m a
nestedDivClasses []     content = content
nestedDivClasses (c:cs) content = divClass c $ nestedDivClasses cs content

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
