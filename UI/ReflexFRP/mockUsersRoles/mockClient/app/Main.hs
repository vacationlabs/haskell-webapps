{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecursiveDo #-}
module Main where

import ClassyPrelude
import Reflex.Dom

import MockAPI
import ExRoles

import Pages.Overview
import Pages.Edit
import Utils
import Data.Map (insert)

main :: IO ()
main = mainWidget $ do
  rec rendererAndSwitch <- domMorph app currentState
      currentState      <- holdDyn BootApp rendererAndSwitch
  return ()

app :: MonadWidget t m => AppState -> m (Event t AppState)
app BootApp = do
  text "Collecting roles..."
  e <- getPostBuild
  roles <- parseR <$$> showRoles e
  return $ leftmost [ (\r -> Overview r r) <$> pick Success roles
                    , const BootApp        <$> pick Failure roles]

app (Overview serverState clientState) = do
  overview serverState clientState
      -- return $ leftmost [change, Edit undefined undefined ("ciplla", undefined) <$ newRole]
  -- return $ Edit undefined ("cipolla", undefined) <$ a

app (Edit serverState clientState (rolename, roleattrs)) = do
  n <- editPage rolename roleattrs
  performEvent_ $ (liftIO $ putStrLn "ciao") <$ n
  let saveClient (Roles clientRoles) (rName, rAttr) = Roles $ insert rName rAttr clientRoles
  return $ leftmost [Overview serverState <$> saveClient clientState <$> n]