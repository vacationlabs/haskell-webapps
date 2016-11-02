{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, TypeOperators #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import MockAPI
import Servant
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip

import qualified Control.Category           as C
import           Control.Concurrent         (threadDelay)
import           Control.Lens
import           Control.Lens.Wrapped
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)

server :: Server MockApi
server = enter (generalizeNat C.. evalStateTSNat exRoles) removeUser
    :<|> serveAssets
    :<|> serveJS
  where
    serveAssets = serveDirectory "../mockClient/assets"
    serveJS     = serveDirectory "../mockClient/js/"

removeUser :: (MonadState Roles m) => RoleName -> User -> m ()
removeUser rolename user =
  _Wrapped' . at rolename . _Just . roleAssociatedUsers . contains user .= False

main :: IO ()
main = run 8081 (gzip gzipSettings $ serve (Proxy @MockApi) server)
  where
    gzipSettings = def { gzipFiles = GzipCompress }
