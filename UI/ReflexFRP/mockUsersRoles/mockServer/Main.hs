{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, OverloadedStrings, TypeApplications, TypeOperators #-}

{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Main where

import MockAPI
import ExRoles
import Servant
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip

import qualified Control.Category           as C
import           Control.Concurrent         (threadDelay)
import           Control.Lens
import           Control.Lens.Wrapped
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import qualified Data.Map                   as M
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import Control.Concurrent.STM

data Config = Config { rolesTVar :: TVar Roles }

server :: Config -> Server MockApi
server config
       = enter (runReaderTNat config) removeUser
    :<|> enter (runReaderTNat config) showRoles
    :<|> serveAssets
    :<|> serveJS
  where
    serveAssets = serveDirectory "../mockClient/assets"
    serveJS     = serveDirectory "../mockClient/js/"

showRoles :: (MonadReader Config m, MonadIO m) => m Roles
showRoles = do
  Config { rolesTVar = roles } <- ask
  liftIO $ readTVarIO roles

removeUser :: (MonadReader Config m, MonadIO m) => RoleName -> User -> m NoContent
removeUser rolename user = do
  Config { rolesTVar = roles } <- ask
  liftIO $ atomically $ modifyTVar' roles
    (_Wrapped' . at rolename . _Just . roleAssociatedUsers . contains user .~ False)
  return NoContent

main :: IO ()
main = do
  state <- atomically $ newTVar exRoles
  let config = Config state
  run 8081 (gzip gzipSettings $ serve (Proxy @MockApi) (server config))
  where
    gzipSettings = def { gzipFiles = GzipCompress }
