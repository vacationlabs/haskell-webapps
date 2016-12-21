{-# LANGUAGE DeriveGeneric, OverloadedStrings, RecursiveDo, ScopedTypeVariables, RankNTypes #-}

module Client where

import Control.Lens
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Monoid
import GHC.Generics
import Reflex.Dom
import Reflex.Dom.Contrib.Router
import qualified Data.Text.Encoding as T
import Web.Routes.PathInfo
import qualified Web.Routes.PathInfo as WR
import qualified URI.ByteString as U

data MyType = Overview
            | Edit T.Text
  deriving (Eq, Show, Read, Generic)

instance PathInfo MyType

mainClient :: IO ()
mainClient = mainWidget run

run :: forall t m .MonadWidget t m => m ()
run = mdo
  initialUrl <- note "Error in first url parsing" . BS.stripPrefix "/" . view U.pathL <$> getURI
  let initialApp = WR.fromPathInfo =<< initialUrl
  loop dispatch (either (const Overview) id initialApp)

-- Note and Hush are the natural transformations between Either and Maybe:

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _         = Nothing

note :: e -> Maybe a -> Either e a
note _ (Just a) = Right a
note e _        = Left e

-- The widget for the pages:

dispatch :: MonadWidget t m => MyType -> m (Event t MyType)
dispatch Overview = overviewPage
dispatch (Edit t) = editPage t

overviewPage :: MonadWidget t m => m (Event t MyType)
overviewPage = do
  text "I'm in the overview."
  a <- button "hi"
  return (Edit "3" <$ a)

editPage :: MonadWidget t m => T.Text -> m (Event t MyType)
editPage t = do
  text $ "I'm in the edit " <> t <> " page."
  overLink <- link "Return to the overview."
  return (Overview <$ _link_clicked overLink)

-- Lower level routing combinators

loop :: forall t m . MonadWidget t m => (MyType -> m (Event t MyType)) -> MyType -> m ()
loop f a = mdo
  rout <- pff e
  e <- switchPromptlyDyn <$> widgetHold (f a) (dispatch <$> updated rout)
  return ()

pff :: MonadWidget t m => Event t MyType -> m (Dynamic t MyType)
pff = (fmap . fmap) (either (const Overview) id) . webRoute ""

webRoute
  :: (MonadWidget t m, WR.PathInfo a)
  => T.Text     -- ^ The part of the URL not related to SPA routing, starting with '/'
  -> Event t a
  -> m (Dynamic t (Either T.Text a))
webRoute pathBase aUpdates = do
  route' encoder decoder aUpdates
 where
   encoder u a = u & U.pathL .~ T.encodeUtf8 (pathBase <> WR.toPathInfo a)
   decoder u = first T.pack . WR.fromPathInfo =<<
     note (pathBase <> " is a Bad prefix for " <> T.decodeUtf8 (U.serializeURIRef' u))
     (BS.stripPrefix (T.encodeUtf8 pathBase) (u ^. U.pathL))

