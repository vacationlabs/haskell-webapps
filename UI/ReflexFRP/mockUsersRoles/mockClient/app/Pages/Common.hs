{-# LANGUAGE OverloadedStrings #-}

module Pages.Common where

import Reflex.Dom
import Data.Monoid

pageHeader :: MonadWidget t m => m ()
pageHeader =
  el "div" $ do
    elClass "nav" "navbar navbar-inverse navigation-clean-search" $
      divClass "container" $ do
        divClass "navbar-header" $ do
          elAttr "a" ("class"=:"navbar-brand navbar-link" <> "href"=:"#") $ text "Tenant name comes here"
          elAttr "button" ("class"=:"navbar-toggle collapsed"<>"data-toggle"=:"collapse"<>"data-target"=:"#navcol-1") $ do
            elClass "span" "sr-only" $ text "Toggle navigation"
            elClass "span" "icon-bar" $ pure ()
            elClass "span" "icon-bar" $ pure ()
            elClass "span" "icon-bar" $ pure ()
        elAttr "div" ("class"=:"collapse navbar-collapse" <> "id"=:"navcol-1") $ do
          elClass "ul" "nav navbar-nav" $ do
            elAttr "li" ("class"=:"active" <> "role"=:"presentation") $ do
              elAttr "a" ("href"=:"#") $ text "Link 1"
            elAttr "li" ("role"=:"presentation") $ do
              elAttr "a" ("href"=:"#") $ text "Link 2"
            elAttr "li" ("role"=:"presentation") $ do
              elAttr "a" ("href"=:"#") $ text "Link 3"
          elAttr "form" ("class"=:"navbar-form navbar-left" <> "target"=:"_self") $ do
            elAttr "div" ("class"=:"form-group") $ do
              elAttr "label" ("class"=:"control-label" <> "for"=:"search-field") $ do
                elAttr "i" ("class"=:"glyphicon glyphicon-search") $ pure ()
              elAttr "input" ("class"=:"form-control search-field" <> "type"=:"search"
                              <> "name"=:"search" <> "id"=:"search-field") $ pure ()

lateralNavigation :: MonadWidget t m => m ()
lateralNavigation =
  elAttr "div" ("class"=:"col-md-3 secton-menu") $
    elAttr "ul" ("class"=:"nav nav-pills nav-stacked") $ do
      elAttr "li" ("class"=:"active") $
        elAttr "a" ("href"=:"#") $ text "Account Settings"
      el "li" $
        elAttr "a" ("href"=:"#") $ text "Products"
      el "li" $
        elAttr "a" ("href"=:"#") $ text "Orders"
