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
import GHC.Generics

import qualified Data.Dependent.Map     as DMap
import           Data.GADT.Compare
import           Data.Proxy             (Proxy (..))

-- import GHCJS.Foreign

type ServerState = Roles
type ClientState = Roles

data AppState = BootApp
              | Overview ServerState ClientState
              | Edit ServerState ClientState (RoleName, RoleAttributes)
              | OverviewInit
              | EditInit
              | NotFound
              | Dispatcher Text
              deriving (Eq, Show, Read, Generic)

showForUrl :: AppState -> Text
showForUrl (Overview _ _) = "overwiew"
showForUrl (Edit _ _ (rn,_)) = "edit/" ++ rn
showForUrl (Dispatcher t) = t
showForUrl other = trace ("In the function showForUrl I received " <> show other) "overview"

textLink :: AppState -> Text
textLink (Overview _ _) = "/overview"
textLink (Edit _ _ (roleName,_)) = "/edit/" <> roleName
textLink BootApp = "Non ci dovrebbe essere"

-------------- These four functions are being used after a discussion with Paolo -------------
type Morph t m a = Dynamic t (m a) -> m (Event t a)

mapMorph  :: (MonadHold t m, Reflex t) => Morph t m (Event t b) -> (a -> m (Event t b)) -> Dynamic t a -> m (Event t b)
mapMorph dyn' f d = dyn' (f <$> d) >>= joinE

joinE :: (Reflex t, MonadHold t f) => Event t (Event t a) -> f (Event t a)
joinE = fmap switch . hold never

-- Se ho un funtore comparabile che contiene un valore, e ho uno sparaeventi di
-- mappe taggate (pensiamo ad una Gadt generalizzata), allora posso ridare un
-- evento di a.

-- EventSelector t k ~~~ select :: forall a. k a -> Event t a
-- fan :: GCompare k => Event t (DMap k Identity) -> EventSelector t k
pick :: (GCompare k, Reflex t) => k a -> Event t (DMap.DMap k Identity) -> Event t a
pick x r = select (fan r) x

-- If I have a functions from a state, which renders
domMorph ::     MonadWidget t m
                => (a -> m (Event t b))  -- widget rewriter
                -> Dynamic t a           -- driver for rewritings
                -> m (Event t b)         -- signal the happened rewriting
domMorph = mapMorph dyn

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


---------------------------- Risposte del server
data ServerResponse b a where
  Success :: ServerResponse b b
  Failure :: ServerResponse b Text

instance GEq (ServerResponse b) where
  Success `geq` Success = Just Refl
  Failure `geq` Failure   = Just Refl
  _ `geq` _             = Nothing

instance GCompare (ServerResponse a) where
  Success `gcompare` Failure = GLT
  Failure `gcompare` Success = GGT
  Success `gcompare` Success = GEQ
  Failure `gcompare` Failure = GEQ

parseR :: ReqResult a -> DMap.DMap (ServerResponse a) Identity
parseR (ResponseSuccess a _) = DMap.singleton Success (Identity a)
parseR (ResponseFailure t _) = DMap.singleton Failure (Identity $ "Response failure: " <> t)
parseR (RequestFailure t)    = DMap.singleton Failure (Identity $ "Request failure: "  <> t)

parseR' :: ReqResult a -> Either Text a
parseR' (ResponseSuccess a _) = Right a
parseR' (ResponseFailure t _) = Left $ "Response failure: " <> t
parseR' (RequestFailure t)    = Left $ "Request failure: "  <> t

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
infixl 4 <$$>

-- setWindowLocationHref :: Text -> IO ()
-- #ifdef ghcjs_HOST_OS
-- setWindowLocationHref = js_setWindowLocationHref . toJSString . unpack

-- foreign import javascript unsafe
--   "window.location.href=$1"
--   js_setWindowLocationHref :: JSString -> IO ()
-- #else
-- setWindowLocationHref = error "setWindowLocationHref: only works in GHCJS"
-- #endif
