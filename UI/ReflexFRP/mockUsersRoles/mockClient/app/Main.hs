{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds, DeriveGeneric, NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE RankNTypes, RecursiveDo, ScopedTypeVariables                   #-}
{-# LANGUAGE TypeApplications, TypeOperators                                #-}

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
import qualified Data.ByteString as BS
import qualified Web.Routes.PathInfo as WR
import qualified URI.ByteString as U

main :: IO ()
main = mainWidget run

run :: forall t m .MonadWidget t m => m ()
run = mdo
  initialUrl <- note "Error in first url parsing" . BS.stripPrefix "/" . view U.pathL <$> getURI
  let initialApp = WR.fromPathInfo =<< initialUrl
  loop dispatch (either (const Overview) id initialApp)

loop :: forall t m . MonadWidget t m => (AppState -> m (Event t AppState)) -> AppState -> m ()
loop f a = mdo
  rout <- pff e
  e <- switchPromptlyDyn <$> widgetHold (f a) (dispatch <$> updated rout)
  return ()

pff :: MonadWidget t m => Event t AppState -> m (Dynamic t AppState)
pff = (fmap . fmap) (either (const Overview) id) . webRoute ""

dispatch :: MonadWidget t m => AppState -> m (Event t AppState)
dispatch Overview = overviewPage
dispatch (Edit t) = editPage' t
dispatch _        = do
  text "There is not supposed to be a page here"
  return never

--------------------------------------------------------------------------------
-- Wrapper around the pages:
--------------------------------------------------------------------------------

overviewPage :: forall t m. MonadWidget t m => m (Event t AppState)
overviewPage = do
  e <- getPostBuild
  serverResponse <- parseR' <$$> showRoles e
  switchPromptlyDyn <$>
    widgetHold
      (do text "Loading..."; return never)
      (either serverError overview <$> serverResponse)

editPage' :: forall t m . MonadWidget t m => RoleName -> m (Event t AppState)
editPage' "" = do
  res <- editPage "" emptyRoleAttributes
  return $ (Overview <$ res)
editPage' roleName = do
  e <- getPostBuild
  serverResponse <- parseR' <$$> showRoles e
  let roleAttributes = second (roleName,)
                     . normalize
                     . second (view $ _Wrapped' . at roleName)
                   <$> serverResponse
  switchPromptlyDyn <$>
    widgetHold (do text "Loading..."; return never)
               (either serverError (uncurry editPage) <$> roleAttributes)

normalize :: Either Text (Maybe a) -> Either Text a
normalize (Left t)         = Left t
normalize (Right Nothing)  = Left "Role not found"
normalize (Right (Just a)) = Right a

serverError :: MonadWidget t m => Text -> m (Event t AppState)
serverError t = do
  el "div" . el "h1" . text $ "Problems with server: " <> t
  return never
