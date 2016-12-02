{-# LANGUAGE DataKinds, DeriveGeneric, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE RankNTypes, RecursiveDo, ScopedTypeVariables                   #-}
{-# LANGUAGE TypeApplications, TypeOperators                                #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import ClassyPrelude
import Control.Lens
import Reflex.Dom
import Reflex.Dom.Contrib.Router

import MockAPI
import Pages.Overview
import Pages.Edit
import Utils

main :: IO ()
main = mainWidget $ mdo
  Just initialUrl <- stripPrefix "http://localhost:8081" <$> getUrlText'
  putStrLn $ "The initial url is: " <> initialUrl

  r :: Route _ Text <- partialPathRoute "" $ def { _routeConfig_pushState = appStateToText <$> renderAndSwitch }
  let routeValue = traceDyn "routeValue: " $ uniqDyn $ value r

  renderAndSwitch <- domMorph app currentState

  currentState    <- traceDyn "pagui "<$> (holdDyn (BootApp initialUrl) $ leftmost
    [ renderAndSwitch
    , fmapMaybe textToAppState $ updated routeValue
    ])

  return ()

appStateToText :: AppState -> Text
appStateToText (BootApp _)       = "/bootApp"
appStateToText (Overview _ _)    = "/overview"
appStateToText (Edit _ _ (rn,_)) = "/edit/" <> rn
appStateToText _                 = "Not yet defined in appStateToText"

textToAppState :: Either Text Text -> Maybe AppState
textToAppState (Right "/overview") = Just (BootApp "/overview")
textToAppState (Right "/edit/AccountAdministrator") = Just (BootApp "/edit/AccountAdministrator")
textToAppState _ = Nothing

app :: MonadWidget t m => AppState -> m (Event t AppState)
app (BootApp "/overview") = do
  putStrLn $ "Hey I got the overview page"
  text "Collecting roles..."
  e <- getPostBuild
  roles <- parseR <$$> showRoles e
  return $ leftmost [ (\r -> Overview r r) <$> pick Success roles
                    , const (BootApp "")   <$> pick Failure roles]

app (BootApp "/edit/AccountAdministrator") = do
  putStrLn $ "Hey I got the AccountAdministrator edit page"
  text "Collecting roles..."
  e <- getPostBuild
  roles <- parseR <$$> showRoles e
  return $ leftmost [ (\r -> Edit r r ("AccountAdministrator", emptyRoleAttributes)) <$> pick Success roles
                    , const (BootApp "") <$> pick Failure roles ]

app (Overview serverState clientState) = do
  overview serverState clientState

app (Edit serverState clientState (rolename, roleattrs)) = do
  n <- editPage rolename roleattrs
  let saveClient (Roles clientRoles) (rName, rAttr) = Roles $ insertMap rName rAttr clientRoles
  return $ leftmost [Overview serverState <$> saveClient clientState <$> n]

app NotFound = do
  text "I'm really sorry, there is nothing here!"
  return never

--------------------------------------------------------------------------------
-- Wrapper around the pages:
--------------------------------------------------------------------------------

overviewPage :: forall t m. MonadWidget t m => m (Event t Text)
overviewPage = do
  e <- getPostBuild
  roles <- parseR' <$$> showRoles e
  let a = fmap behavior (traceEvent "Roles e' stato usato " roles)
  b <- widgetHold (return never) a
  let c = traceEvent "b e' adesso: " $ switchPromptlyDyn b
  return c

behavior :: MonadWidget t m => Either Text Roles -> m (Event t Text)
behavior (Left t) = do
  el "div" . el "h1" . text $ "Problems with server" <> t
  return never
behavior (Right r) = do
  a <- overview r r
  return $ fmap textLink a

editPage' :: MonadWidget t m => RoleName -> m (Event t Text)
editPage' roleName = do
  e <- getPostBuild
  liftIO $ putStrLn $ "roleName: " <> roleName
  roles <- parseR' <$$> showRoles e
  let roleAttributes' = view (_Wrapped' . at roleName) <$$> roles
      roleAttributes = (,) roleName <$$> (normalize <$> roleAttributes')
  let a = fmap behavior2 roleAttributes
  b <- widgetHold (do text "heyheyhey"; return never) a
  return $ switchPromptlyDyn b

normalize :: Either Text (Maybe a) -> Either Text a
normalize (Left t) = Left t
normalize (Right Nothing) = Left "Role not found"
normalize (Right (Just a)) = Right a

behavior2 :: MonadWidget t m => Either Text (RoleName, RoleAttributes) -> m (Event t Text)
behavior2 (Left t) = do
  el "div" . el "h1" . text $ "Problems with server" <> t
  return never
behavior2 (Right (n,a)) = do
  res <- editPage n a
  return $ ("edit" <$ res)
