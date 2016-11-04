{-# LANGUAGE GADTs, ScopedTypeVariables, DataKinds, FlexibleContexts, Rank2Types, ConstraintKinds, TypeFamilies, MultiParamTypeClasses, FlexibleInstances, NoMonomorphismRestriction, NoImplicitPrelude #-}

{-# LANGUAGE TypeApplications, OverloadedStrings, PartialTypeSignatures #-}

module Utils where

import MockAPI

import ClassyPrelude
import Reflex.Dom
import Servant.API
import Servant.Reflex

import           Control.Monad.Identity (Identity)
import qualified Data.Dependent.Map     as DMap
import           Data.GADT.Compare
import           Data.Proxy             (Proxy (..))


-------------- Thanks to Paolo! -------------

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

deleteUser :: (MonadWidget t m) => Dynamic t (Either Text Text)
                                -> Dynamic t (Either Text User)
                                -> Event t ()
                                -> m (Event t (ReqResult NoContent))
showRoles  :: (MonadWidget t m) => Event t ()
                                -> m (Event t (ReqResult Roles))
(deleteUser :<|> showRoles :<|> _) = apiClients


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
