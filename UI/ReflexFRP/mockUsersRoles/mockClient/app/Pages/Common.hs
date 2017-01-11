{-# LANGUAGE OverloadedStrings, NoImplicitPrelude, QuasiQuotes #-}

module Pages.Common where

import ClassyPrelude
import Reflex.Dom
import ReflexJsx

pageHeader' :: MonadWidget t m => m ()
pageHeader' =
  el "div" $ do
    elClass "nav" "navbar navbar-inverse navigation-clean-search" $
      divClass "container" $ do
        divClass "navbar-header" $ do
          elAttr "a" ("class"=:"navbar-brand navbar-link" <> "href"=:"") $ text "Tenant name comes here"
          elAttr "button" ("class"=:"navbar-toggle collapsed"<>"data-toggle"=:"collapse"<>"data-target"=:"#navcol-1") $ do
            elClass "span" "sr-only" $ text "Toggle navigation"
            elClass "span" "icon-bar" $ pure ()
            elClass "span" "icon-bar" $ pure ()
            elClass "span" "icon-bar" $ pure ()
        elAttr "div" ("class"=:"collapse navbar-collapse" <> "id"=:"navcol-1") $ do
          elClass "ul" "nav navbar-nav" $ do
            elAttr "li" ("class"=:"active" <> "role"=:"presentation") $ do
              elAttr "a" ("href"=:"") $ text "Link 1"
            elAttr "li" ("role"=:"presentation") $ do
              elAttr "a" ("href"=:"") $ text "Link 2"
            elAttr "li" ("role"=:"presentation") $ do
              elAttr "a" ("href"=:"") $ text "Link 3"
          elAttr "form" ("class"=:"navbar-form navbar-left" <> "target"=:"_self") $ do
            elAttr "div" ("class"=:"form-group") $ do
              elAttr "label" ("class"=:"control-label" <> "for"=:"search-field") $ do
                elAttr "i" ("class"=:"glyphicon glyphicon-search") $ pure ()
              elAttr "input" ("class"=:"form-control search-field" <> "type"=:"search"
                              <> "name"=:"search" <> "id"=:"search-field") $ pure ()

pageHeader :: MonadWidget t m => m ()
pageHeader = [jsx|
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
|]

lateralNavigation' :: MonadWidget t m => m ()
lateralNavigation' =
  elAttr "div" ("class"=:"col-md-3 secton-menu") $
    elAttr "ul" ("class"=:"nav nav-pills nav-stacked") $ do
      elAttr "li" ("class"=:"active") $
        elAttr "a" ("href"=:"") $ text "Account Settings"
      el "li" $
        elAttr "a" ("href"=:"") $ text "Products"
      el "li" $
        elAttr "a" ("href"=:"") $ text "Orders"

lateralNavigation :: MonadWidget t m => m ()
lateralNavigation = [jsx|
<div class="col-md-3 secton-menu">
    <ul class="nav nav-pills nav-stacked">
        <li class="active"><a href="#">Account Settings</a></li>
        <li><a href="#">Products</a></li>
        <li><a href="#">Orders</a></li>
    </ul>
</div>
|]

sitePosition :: MonadWidget t m => [Text] -> m ()
sitePosition = elClass "ol" "breadcrumb"
             . mapM_ (el "li" . el "a" . el "span" . text)

buttonClass :: MonadWidget t m => Text -> Text -> m (Event t ())
buttonClass cls t = do
  (b, _) <- elAttr' "button"
                    ("class" =: cls <> "type" =: "button")
                    (text t)
  return (domEvent Click b)

template :: MonadWidget t m => m ()
template = [jsx|
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
|]
