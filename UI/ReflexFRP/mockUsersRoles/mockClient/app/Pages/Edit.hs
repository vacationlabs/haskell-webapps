{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Pages.Edit where

import ReflexJsx
import Reflex
import Reflex.Dom

import Pages.Common

page :: MonadWidget t m => m ()
page = do
  el "body" $ do
    pageHeader
    el "div" $
      elAttr "div" ("class"=:"container") $ do
        elAttr "div" ("class"=:"row") $ do
          lateralNavigation
          elAttr "div" ("class"=:"col-md-9") $ do
            positionBar
            form
            elAttr "div" ("class"=:"form-group") $ do
              elAttr "button" ("class"=:"btn btn-primary" <> "type"=:"submit") $ text "Save"
              elAttr "a" ("href"=:"#" <> "class"=:"cancel text-danger") $ text "cancel"

form = do
  el "form" $ do
    divClass "form-group" $ do
      elAttr "label" ("class"=:"control-label") $ text "Role name"
      elAttr "input" ("class"=:"form-control" <> "type"=:"text" <> "value"=::"Product editor") $ pure ()
    divClass "form-group" $ do
      elAttr "label" ("class"=:"control-label") $ text "Permissions"
      divClass "row"
        productPermissionWidget
        ordersPermissionWidget
        usersPermissionWidget
    deleteUserWidget

-- deleteUserWidget =
--   <div class="form-group">
--     <label class="control-label">Users with this role</label>
--     <ul>
--       <li>user1@mydomain.com <a href="#">(revoke) </a> </li>
--       <li>user2@mydomain.com <a href="#">(revoke) </a> </li>
--       <li>user3@mydomain.com <a href="#">(revoke) </a> </li>
--       <li>user4@mydomain.com <a href="#">(revoke) </a> </li>
--     </ul>
--     <div class="row">
--       <div class="col-lg-6 col-md-8 col-sm-8 col-xs-12">
--         <div class="input-group">
--           <div class="input-group-addon"><span>Add another user</span></div>
--           <input class="form-control" type="text">
--           <div class="input-group-btn">
--               <button class="btn btn-default" type="button">Add </button>
--           </div>
--         </div>
--       </div>
--     </div>
--   </div>

-- productPermissionWidget =
--         divClass "col-lg-4 col-md-4 col-md-offset-0 col-sm-6 col-xs-12"
--           divClass "checkbox permission-group-heading"
--             elAttr "label" ("class"=:"control-label") $
--               elAttr "input" ("type"=:"checkbox") $ el "strong" $ text "Product"
--           <ECCETERA>

-- ordersPermissionWidget =
--         <div class="col-lg-4 col-md-4 col-md-offset-0 col-sm-6 col-xs-12">
--           <div class="checkbox permission-group-heading">
--             <label class="control-label">
--               <input type="checkbox"><strong>Orders</strong></label>
--           <ECCETERA>

-- usersPermissionWidget =
--         <div class="col-lg-4 col-md-4 col-md-offset-0 col-sm-6 col-xs-12">
--           <div class="checkbox permission-group-heading">
--             <label class="control-label">
--               <input type="checkbox"><strong>Users</strong></label>
--           </div>
--           <div class="checkbox">
--             <label class="control-label">
--               <input type="checkbox">View user details</label>
--           </div>
--           <ECCETERA>
