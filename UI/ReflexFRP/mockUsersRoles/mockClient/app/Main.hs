{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, RecursiveDo #-}
module Main where

import ClassyPrelude
import Reflex.Dom

import MockAPI
import ExRoles

import Pages.Overview
import Pages.Edit
import Utils

data AppState = BootApp | Overview Roles | Edit Roles (RoleName, RoleAttributes)

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
  return $ leftmost [ Overview      <$> pick Success roles
                    , const BootApp <$> pick Failure roles]

app (Overview _) = do
  a <- overview (tableSection exRoles)
  return $ Edit undefined ("cipolla", undefined) <$ a

app (Edit _ (rolename, _)) = do
  editPage
  return $ leftmost []
