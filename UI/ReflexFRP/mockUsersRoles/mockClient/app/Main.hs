{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
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

-- import ClassyPrelude
-- import           Data.Proxy
-- import           Reflex.Dom
-- import qualified Reflex.Dom.Contrib.Router as RR
-- import           Servant.API
-- import           Servant.Router
import Control.Lens
-- import Control.Lens.Wrapped
import Web.Routes.PathInfo
-- import GHC.Generics

-- import Data.Time.Clock

main :: IO ()
main = mainWidget $ mdo
  -- pb <- getPostBuild
  r :: Route _ Text <- partialPathRoute "" $ def { _routeConfig_pushState = transl <$> renderAndSwitch }
  renderAndSwitch <- domMorph app currentState
  currentState    <- holdDyn BootApp $ leftmost
    [ renderAndSwitch
    , fmapMaybe viewSwitcher $ updated (uniqDyn (value r))
    ]
  return ()

transl :: AppState -> Text
transl (BootApp)         = "bootApp"
transl (Overview _ _)    = "overview"
transl (Edit _ _ (rn,_)) = "edit/" <> rn
transl _                 = "Not yet defined in transl"

viewSwitcher :: Either Text Text -> Maybe AppState
viewSwitcher (Right "overview") = Just BootApp
viewSwitcher _ = Nothing

run :: forall t m . MonadWidget t m => m ()
run = mdo
  pb <- getPostBuild
  text "2"
  r :: Route t MyType <- webRoute "" $ def { _routeConfig_pushState = fmapMaybe id $ tag (current vs) go }
  xs <- textInput def { _textInputConfig_setValue = (pack . show) <$>
                         fmapMaybe hush (leftmost [updated (value r), tag (current $ value r) pb]) }
  let vs :: Dynamic t (Maybe MyType) = traceDyn "vs" $ (readMay . unpack) <$> value xs
  go <- button "Go"
  return ()

-- main :: IO ()
-- main = mainWidget $ do
--   rec rendererAndSwitch <- domMorph app currentState
--       currentState      <- holdDyn BootApp rendererAndSwitch
--   return ()

internalRouting :: Text -> AppState
internalRouting "http://localhost:8081/overwiew" = BootApp
internalRouting "http://localhost:8081/edit/" = Dispatcher "http://localhost:8081/overwiew"
internalRouting other = traceShow other NotFound


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

app NotFound = do
  text "I'm really sorry, there is nothing here!"
  return never

-- app (Dispatcher t)
--   | t == "http://localhost:8081/overwiew" = do
--       pb <- getPostBuild
--       text "This is the dispatcher: you entered "
--       text t
--       return $ (const BootApp) <$> pb
--   | "http://localhost:8081/edit/" `isPrefixOf` t = do

-- ----------------------------------------------
-- -- | delay an Event by the amount of time specified in its value
-- drivenDelay     :: MonadWidget t m
--                 => Event t (NominalDiffTime,a) -- ^ delay time in seconds + value
--                 -> m (Event t a)
-- drivenDelay e =  performEventAsync . ffor e $ \(dt,a) cb -> liftIO . void . forkIO $ do
--   threadDelay . ceiling $ dt * 1000000
--   cb a


-- main :: IO ()
-- main = routeSite $ \uri -> do
--   let handler = overviewPage :<|> editPage'
--   result <- runRoute uri (Proxy @Navigation) handler
--   case result of
--     Left _ -> do
--       el "div" $ text "Incorrect address"
--       return never
--     Right e -> do
--       let e' = traceEvent "sono in main: " e
--       performEvent_ $ ffor e $ \t -> liftIO (setWindowLocationHref t)
--       return e'


-- New routing example

-- main :: IO ()
-- main = mainWidget run

-- Greg's example on webroutes
data MyType = Cat
            | Dog Int
  deriving (Eq, Show, Read,Generic)

instance PathInfo MyType

-- run :: forall t m . MonadWidget t m => m ()
-- run = mdo
--   pb <- getPostBuild
--   text "2"
--   r :: Route t MyType <- webRoute "" $ def { _routeConfig_pushState = fmapMaybe id $ tag (current vs) go }
--   xs <- textInput def { _textInputConfig_setValue = (pack . show) <$>
--                          fmapMaybe hush (leftmost [updated (value r), tag (current $ value r) pb]) }
--   let vs :: Dynamic t (Maybe MyType) = traceDyn "vs" $ (readMay . unpack) <$> value xs
--   go <- button "Go"
--   return ()

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _         = Nothing

translateCat :: Maybe MyType -> Maybe Text
translateCat Nothing = Nothing
translateCat (Just Cat) = Just "cat"
translateCat (Just (Dog n)) = Just $ "dog" <> tshow n

-- Master translation of greg's example
-- run :: forall t m . MonadWidget t m => m ()
-- run = mdo
--   pb <- getPostBuild
--   text "2"
--   r :: Route t <- RR.route def { _routeConfig_pushState = translate2 <$> tag (current vs) go }
--   xs <- textInput def
--     -- { _textInputConfig_setValue = tshow <$>
--     --                      fmapMaybe translate1 (leftmost [updated (value r), tag (current $ value r) pb]) }
--   let vs :: Dynamic t (Maybe AppState) = traceDyn "vs" $ translate1 <$> value xs
--   go <- button "Go"
--   dynText $ value r
--   return ()

translate1 :: Text -> Maybe AppState
translate1 "http://localhost:8081/overviewInit" = Just OverviewInit
translate1 "http://localhost:8081/editInit"     = Just EditInit
translate1 _ = Just BootApp

-- translate2 :: Maybe AppState -> Text
-- translate2 (Just OverviewInit) = "overviewInit"
-- translate2 (Just EditInit) = "editInit"
-- translate2 Nothing = "overview"

-- hush :: Either e a -> Maybe a
-- hush (Right a) = Just a
-- hush _         = Nothing

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

-- flattening :: Event t (Event t a) -> Event t a
-- flattening ee = V

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
