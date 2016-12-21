{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
module Pages.ProgressBar where

import Reflex
import Reflex.Dom

import ClassyPrelude
import Data.Functor.Constant

import Data.Semigroup
import Control.Monad.Trans.Class

import Utils
import Data.Time.Clock

data MyInt = MyInt Int

initialBarAttributes :: Map Text Text
initialBarAttributes =
     "height"   =: "2px"
  <> "position" =: "static"
  <> "right"    =: "0"
  <> "bottom"   =: "0"
  <> "width"    =: "40%"
  <> "background-color" =: "blue"

asStyle :: Map Text Text -> Map Text Text
asStyle = ("style" =:) . intercalate ";" . map formatAttribute . mapToList
  where
    formatAttribute (a,b) = a <> ":" <> b

progressBar :: MonadWidget t m => Dynamic t Int -> m ()
progressBar width = do
  elDynAttr "div" (asStyle <$> liftA2 setWidth width (pure initialBarAttributes)) blank

setWidth :: Int -> Map Text Text -> Map Text Text
setWidth w = adjustMap (const $ tshow w <> "%") "width"

-- With EventWriter
global :: MonadWidget t m => m ()
global = mdo
  progressBar lengthBar
  (_, progressBarSig)        <- runEventWriterT wrapper
  -- lengthBar :: Dynamic t Int <- traceDyn "length" <$> count progressBarSig
  -- lengthBar :: Dynamic t Int <- foldDyn (\_ b -> b + (100-b)`div`4) 0 (updated c)
  let lengthBar = round <$> a
  currTime <- liftIO getCurrentTime
  c <- clockLossy (0.2 :: NominalDiffTime) currTime
  display c

  text "anim: "
  a <- anim (\t -> 100*(1 - exp(-t)))
  display a
  text "end of global"

data Progress = StartProgress | HaltProgress

instance Semigroup Progress where

instance Monoid Progress where
  mempty                                = HaltProgress
  StartProgress `mappend` StartProgress = StartProgress
  _             `mappend` _             = HaltProgress

wrapper :: (MonadWidget t m) => EventWriterT t Progress m ()
wrapper = do
  simpleButton1
  simpleButton1

simpleButton1 :: (MonadWidget t m) => EventWriterT t Progress m ()
simpleButton1 = do
  start <- lift $ do
    el "div" $ text "hi"
    el "div" $ button "hi"
  tellEvent (StartProgress <$ start)

anim :: forall t m . MonadWidget t m => (Float -> Float) -> m (Dynamic t Float)
anim f = do
  currTime <- liftIO getCurrentTime
  c <- clockLossy (0.05 :: NominalDiffTime) currTime
  let c' :: Dynamic t Float = f . realToFrac . timeFrom currTime <$> c
  pure c'

timeFrom :: UTCTime -> TickInfo -> NominalDiffTime
timeFrom u t = (_tickInfo_alreadyElapsed t) + (_tickInfo_lastUTC t `diffUTCTime` u)
