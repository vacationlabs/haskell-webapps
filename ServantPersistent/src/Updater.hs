{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
module Updater
    where

import Control.Lens
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Types
import Types


type family AllC (cs :: [k -> Constraint]) (a :: k) :: Constraint where
    AllC '[] a = ()
    AllC (c ': cs) a = (c a, AllC cs a)

newtype Updater cs = U { runUpdate :: forall a. AllC cs a => a -> a }

instance forall cs. Monoid (Updater cs) where
    mempty = U id
    (U a) `mappend` (U b) = U $ (a . b)

parseUpdater :: forall cs a. FromJSON a => Object -> Text -> (a -> Updater cs) -> Parser (Updater cs)
parseUpdater v t setter = maybe (mempty :: Updater cs) setter <$> v .:? t

type TenantUpdater = Updater '[HasName, HasBackofficeDomain]

tu :: (forall a. (HasName a, HasBackofficeDomain a) => a -> a) -> TenantUpdater
tu = U

instance FromJSON TenantUpdater where
    parseJSON (Object v) =
      mconcat <$> f
        where f = sequence
                    [ parseUpdater v "name" (\x-> tu $ set name x)
                    , parseUpdater v "backoffice_domain" (\x-> tu $ set backofficeDomain x)
                    ]
    parseJSON _ = fail "Need an Object"

type UserUpdater = Updater '[HasHumanName, HasContactDetails]

uu :: (forall a. (HasHumanName a, HasContactDetails a) => a -> a) -> UserUpdater
uu = U

instance FromJSON UserUpdater where
    parseJSON (Object v) =
        mconcat <$> f
            where f = sequence
                        [ parseUpdater v "first_name" (\x -> uu $ set firstName x)
                        , parseUpdater v "last_name" (\x -> uu $ set lastName x)
                        , parseUpdater v "email" (\x -> uu $ set email x)
                        , parseUpdater v "phone" (\x -> uu $ set phone x)
                        ]
    parseJSON _ = fail "Need an Object"
