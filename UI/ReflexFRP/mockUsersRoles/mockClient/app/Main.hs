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

import URI.ByteString hiding (URI)

-- main'' :: IO ()
-- main'' = mainWidget $ mdo
--   initialURI <- getURI
--   let initialUrl = initialURI ^. pathL . to decodeUtf8
--   -- currentState <- foldDyn mix (BootApp initialUrl) upd
--   renderAndSwitch <- domMorph app currentState
--   a <- route (appStateToText <$> renderAndSwitch)
--   let routeValue = uniqDyn $ (view (pathL . to decodeUtf8)) <$> a
--   -- let routeValue = (view (pathL . to decodeUtf8)) <$> a
--   let upd = updated routeValue
--   currentState    <- uniqDyn <$> (holdDyn BootApp $ leftmost
--     [ renderAndSwitch
--     -- , attachPromptlyDynWith (flip mix) currentState upd
--     , attachWith (flip mix) (current currentState) upd
--     -- , fmapMaybe textToAppState $ updated routeValue
--     ])
--   return ()

type InternalAddress = Text

mix :: InternalAddress -> AppState -> AppState
mix "/edit/AccountAdministrator" Overview
  = Edit "AccountAdministrator"

mix2 :: MonadWidget t m => Text -> AppState -> m (Event t AppState)
mix2 "/overview" BootApp = do
  e <- getPostBuild
  roles <- parseR <$$> showRoles e
  return $ leftmost [ (\r -> Overview) <$> pick Success roles
                    , const BootApp   <$> pick Failure roles]
mix2 str appSt = terror $ "mix2 called with " <> str <> " and " <> tshow appSt

-- main :: IO ()
-- main = mainWidget $ mdo
--   initialURI <- getURI
--   let initialUrl = initialURI ^. pathL . to decodeUtf8
--   appState <- holdDyn BootApp update
--   -- update <- dyn (mix2 <$> rut <*> appState) >>= switchPromptly never
--   update <- (holdDyn (return never) $ attachPromptlyDynWith (flip mix2) appState (updated rut))
--          >>= dyn
--          >>= switchPromptly never
--   rut <- (view (pathL . to decodeUtf8)) <$$> route (appStateToText <$> update)
--   return ()

main :: IO ()
main = mainWidget $ mdo
  initialURI <- getURI
  putStrLn $ tshow initialURI
  r               <- route (appStateToText <$> renderAndSwitch)
  renderAndSwitch <- domMorph app currentState
  currentState    <- traceDyn "pagui "<$> (holdDyn (uriToAppState initialURI) $ leftmost [updated (uriToAppState <$> r)])
  return ()

appStateToText :: AppState -> Text
appStateToText BootApp   = "/bootApp"
appStateToText Overview  = "/overview"
appStateToText (Edit rn) = "/edit/" <> rn
appStateToText _         = "Not yet defined in appStateToText"

uriToAppState :: URI -> AppState
uriToAppState u = case (u ^. pathL . to decodeUtf8) of
  "/overview" -> Overview
  -- "edit" -> Edit

textToAppState :: Either Text Text -> Maybe AppState
textToAppState (Right "/overview") = Just $BootApp
textToAppState (Right "/edit/AccountAdministrator") = Just $BootApp
textToAppState _ = Nothing

app :: MonadWidget t m => AppState -> m (Event t AppState)
app BootApp = do
  putStrLn $ "Hey I got the overview page"
  text "Collecting roles..."
  e <- getPostBuild
  roles <- parseR <$$> showRoles e
  return $ leftmost [ (\r -> Overview) <$> pick Success roles
                    , const BootApp    <$> pick Failure roles]

app Overview = do
  -- TODO Grab state from the server
  putStrLn "bau sono io"
  e     <- getPostBuild
  roles <- (snd . fanEither) <$> (parseR' <$$> showRoles e)
  -- _what <$> (overview <$> roles)
  -- opage <- overview roles
  undefined

app (Edit rolename) = do
  -- TODO Sync from the server
  let roleattrs = undefined
  n <- editPage rolename roleattrs
  let saveClient (Roles clientRoles) (rName, rAttr) = Roles $ insertMap rName rAttr clientRoles
  -- TODO Save the state to the server
  let clientState = undefined
  return $ leftmost [const Overview <$> saveClient clientState <$> n]

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
  a <- overview r
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
