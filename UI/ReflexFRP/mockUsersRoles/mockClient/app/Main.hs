{-# LANGUAGE DataKinds, DeriveGeneric, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE RankNTypes, RecursiveDo, ScopedTypeVariables                   #-}
{-# LANGUAGE TypeApplications, TypeOperators                                #-}

-- {-# LANGUAGE PartialTypeSignatures #-}
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

import URI.ByteString

main'' :: IO ()
main'' = mainWidget $ mdo
  initialURI <- getURI
  let initialUrl = initialURI ^. pathL . to decodeUtf8
  -- currentState <- foldDyn mix (BootApp initialUrl) upd
  renderAndSwitch <- domMorph app currentState
  a <- route (appStateToText <$> renderAndSwitch)
  let routeValue = uniqDyn $ (view (pathL . to decodeUtf8)) <$> a
  -- let routeValue = (view (pathL . to decodeUtf8)) <$> a
  let upd = updated routeValue
  currentState    <- uniqDyn <$> (holdDyn (BootApp initialUrl) $ leftmost
    [ renderAndSwitch
    -- , attachPromptlyDynWith (flip mix) currentState upd
    , attachWith (flip mix) (current currentState) upd
    -- , fmapMaybe textToAppState $ updated routeValue
    ])
  return ()

type InternalAddress = Text

mix :: InternalAddress -> AppState -> AppState
mix "/edit/AccountAdministrator" (Overview ss cs)
  = Edit ss cs ("AccountAdministrat.", cs ^?! to unRoles . at "AccountAdministrator" . _Just)
mix _ _ = NotFound

mix2 :: MonadWidget t m => Text -> AppState -> m (Event t AppState)
mix2 "/overview" (BootApp _) = do
  e <- getPostBuild
  roles <- parseR <$$> showRoles e
  return $ leftmost [ (\r -> Overview r r) <$> pick Success roles
                    , const (BootApp "")   <$> pick Failure roles]
mix2 "/overview" o@(Overview _ _) = app o
mix2 str appSt = terror $ "mix2 called with " <> str <> " and " <> tshow appSt

main :: IO ()
main = mainWidget $ mdo
  initialURI <- getURI
  let initialUrl = initialURI ^. pathL . to decodeUtf8
  appState <- holdDyn (BootApp initialUrl) update
  -- update <- dyn (mix2 <$> rut <*> appState) >>= switchPromptly never
  update <- (holdDyn (return never) $ attachPromptlyDynWith (flip mix2) appState (updated rut))
         >>= dyn
         >>= switchPromptly never
  rut <- (view (pathL . to decodeUtf8)) <$$> route (appStateToText <$> update)
  return ()





























appStateToText :: AppState -> Text
appStateToText (BootApp _)       = "/bootApp"
appStateToText (Overview _ _)    = "/overview"
appStateToText (Edit _ _ (rn,_)) = "/edit/" <> rn
appStateToText _                 = "Not yet defined in appStateToText"

textToAppState :: Text -> Maybe AppState
textToAppState "/overview"                  = Just (BootApp "/overview")
textToAppState "/edit/AccountAdministrator" = Just (BootApp "/edit/AccountAdministrator")
textToAppState _                            = Nothing

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

app (BootApp _) = do
  putStrLn $ "HELP!"
  text $ "HELP!"
  return never

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
  let a = fmap behavior roles
  b <- widgetHold (return never) a
  let c = switchPromptlyDyn b
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
