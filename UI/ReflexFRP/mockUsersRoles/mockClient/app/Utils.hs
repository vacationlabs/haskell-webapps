{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs, ScopedTypeVariables, DataKinds, FlexibleContexts, Rank2Types, ConstraintKinds, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, NoMonomorphismRestriction, NoImplicitPrelude, CPP #-}

{-# LANGUAGE TypeApplications, OverloadedStrings, PartialTypeSignatures #-}

module Utils where

import MockAPI

import ClassyPrelude
import Reflex.Dom
import Servant.API
import Servant.Reflex
import Web.Routes.PathInfo
import Control.Lens
import           Data.Proxy             (Proxy (..))

import Reflex.Dom.Contrib.Router
import URI.ByteString

type ServerState = Roles
type ClientState = Roles

data AppState = BootApp
              | Overview
              | Edit RoleName
              deriving (Eq, Show, Read, Generic)

instance PathInfo AppState

-- showForUrl :: AppState -> Text
-- showForUrl Overview = "overwiew"
-- showForUrl (Edit rn) = "edit/" ++ rn
-- showForUrl other = trace ("In the function showForUrl I received " <> show other) "overview"

-- textLink :: AppState -> Text
-- textLink Overview = "/overview"
-- textLink (Edit roleName) = "/edit/" <> roleName
-- textLink BootApp = "Non ci dovrebbe essere"

-- -------------- These four functions are being used after a discussion with Paolo -------------
-- type Morph t m a = Dynamic t (m a) -> m (Event t a)

-- mapMorph  :: (MonadHold t m, Reflex t) => Morph t m (Event t b) -> (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
-- mapMorph dyn' f d = dyn' (f <$> d) >>= joinE

-- joinE :: (Reflex t, MonadHold t f) => Event t (Event t a) -> f (Event t a)
-- joinE = fmap switch . hold never

-- Se ho un funtore comparabile che contiene un valore, e ho uno sparaeventi di
-- mappe taggate (pensiamo ad una Gadt generalizzata), allora posso ridare un
-- evento di a.

-- -- EventSelector t k ~~~ select :: forall a. k a -> Event t a
-- -- fan :: GCompare k => Event t (DMap k Identity) -> EventSelector t k
-- pick :: (GCompare k, Reflex t) => k a -> Event t (DMap.DMap k Identity) -> Event t a
-- pick x r = select (fan r) x

-- -- If I have a functions from a state, which renders
-- domMorph ::     MonadWidget t m
--                 => (a -> m (Event t b))  -- widget rewriter
--                 -> Dynamic t a           -- driver for rewritings
--                 -> m (Event t b)         -- signal the happened rewriting
-- domMorph = mapMorph dyn

-- domMorph' ::     MonadWidget t m
--                  => (a -> m (Event t b))  -- widget rewriter
--                  -> Dynamic t a           -- driver for rewritings
--                  -> m (Event t b)         -- signal the happened rewriting
-- -- domMorph' f d = dyn (f <$> d) >>= joinE
-- domMorph' f d = dyn (f <$> d) >>= switchPromptly never

-- domMorph'' ::     MonadWidget t m
--                  => (a -> m (Event t b))  -- widget rewriter
--                  -> Dynamic t a           -- driver for rewritings
--                  -> m (Event t b)         -- signal the happened rewriting
-- domMorph'' f d = dyn ((\a -> _what) <$> d) >>= switchPromptly never

----------------------------- ENDPOINT DELL'API

url :: BaseUrl
url = BaseFullUrl Http "localhost" 8081 ""

apiClients :: forall t m. (MonadWidget t m) => _
apiClients = client (Proxy @MockApi) (Proxy @m) (constDyn url)

deleteUser :: (MonadWidget t m)
           => Dynamic t (Either Text Text)
           -> Dynamic t (Either Text User)
           -> Event t ()
           -> m (Event t (ReqResult NoContent))

addRole :: (MonadWidget t m)
        => Dynamic t (Either Text Text)
        -> Dynamic t (Either Text RoleAttributes)
        -> Event t ()
        -> m (Event t (ReqResult NoContent))

showRoles :: (MonadWidget t m)
          => Event t ()
          -> m (Event t (ReqResult Roles))

(deleteUser :<|> addRole :<|> showRoles :<|> _) = apiClients


-- ---------------------------- Risposte del server
-- data ServerResponse b a where
--   Success :: ServerResponse b b
--   Failure :: ServerResponse b Text

-- instance GEq (ServerResponse b) where
--   Success `geq` Success = Just Refl
--   Failure `geq` Failure   = Just Refl
--   _ `geq` _             = Nothing

-- instance GCompare (ServerResponse a) where
--   Success `gcompare` Failure = GLT
--   Failure `gcompare` Success = GGT
--   Success `gcompare` Success = GEQ
--   Failure `gcompare` Failure = GEQ

-- parseR :: ReqResult a -> DMap.DMap (ServerResponse a) Identity
-- parseR (ResponseSuccess a _) = DMap.singleton Success (Identity a)
-- parseR (ResponseFailure t _) = DMap.singleton Failure (Identity $ "Response failure: " <> t)
-- parseR (RequestFailure t)    = DMap.singleton Failure (Identity $ "Request failure: "  <> t)

parseR' :: ReqResult a -> Either Text a
parseR' (ResponseSuccess a _) = Right a
parseR' (ResponseFailure t _) = Left $ "Response failure: " <> t
parseR' (RequestFailure t)    = Left $ "Request failure: "  <> t

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
infixl 4 <$$>

webRoute
  :: (MonadWidget t m, PathInfo a)
  => Text     -- ^ The part of the URL not related to SPA routing, starting with '/'
  -> Event t a
  -> m (Dynamic t (Either Text a))
webRoute pathBase aUpdates = do
  route' encoder decoder aUpdates
 where
   encoder u a = u & pathL .~ encodeUtf8 (pathBase <> toPathInfo a)
   decoder u = first pack . fromPathInfo =<<
     note (pathBase <> " is a Bad prefix for " <> decodeUtf8 (serializeURIRef' u))
     (stripPrefix (encodeUtf8 pathBase) (u ^. pathL))

hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush _         = Nothing

note :: e -> Maybe a -> Either e a
note _ (Just a) = Right a
note e _        = Left e
