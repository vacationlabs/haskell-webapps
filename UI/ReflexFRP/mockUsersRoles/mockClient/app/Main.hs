{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecursiveDo #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import ClassyPrelude
import Reflex.Dom

import MockAPI

import Pages.Overview
import Pages.Edit
import Utils

import Reflex.Dom.Contrib.Router

import ClassyPrelude
import           Data.Proxy
import           Reflex.Dom
import           Reflex.Dom.Contrib.Router
import           Servant.API
import           Servant.Router
import Control.Lens
import Control.Lens.Wrapped
-- main :: IO ()
-- main = mainWidget $ do
--   rec rendererAndSwitch <- domMorph app currentState
--       currentState      <- holdDyn BootApp rendererAndSwitch
--   return ()

app :: MonadWidget t m => AppState -> m (Event t AppState)
app BootApp = do
  text "Collecting roles..."
  e <- getPostBuild
  roles <- parseR <$$> showRoles e
  return $ leftmost [ (\r -> Overview r r) <$> pick Success roles
                    , const BootApp        <$> pick Failure roles]

app (Overview serverState clientState) = do
  overview serverState clientState

app (Edit serverState clientState (rolename, roleattrs)) = do
  n <- editPage rolename roleattrs
  let saveClient (Roles clientRoles) (rName, rAttr) = Roles $ insertMap rName rAttr clientRoles
  return $ leftmost [Overview serverState <$> saveClient clientState <$> n]

----------------------------------------------

main :: IO ()
main = routeSite $ \uri -> do
  let handler = overviewPage :<|> editPage'
  result <- runRoute uri (Proxy @Navigation) handler
  case result of
    Left _ -> do
      el "div" $ text "Incorrect address"
      return never
    Right e -> return e

overviewPage :: forall t m. MonadWidget t m => m (Event t Text)
overviewPage = do
  e <- getPostBuild
  roles <- parseR' <$$> showRoles e
  let a = fmap behavior roles
  b <- widgetHold (return never) a
  return $ switchPromptlyDyn b

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
