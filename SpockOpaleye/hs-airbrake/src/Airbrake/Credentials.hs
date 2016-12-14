{-# LANGUAGE CPP #-}

-- | Utilities for loading the API key from the environment.
module Airbrake.Credentials (
    APIKey,
    credentialsDefaultFile, credentialsDefaultKey,
    loadCredentialsFromFile, loadCredentialsFromEnv,
    loadCredentialsFromEnvOrFile, loadCredentialsDefault
) where

import Control.Monad          (liftM)
import Control.Monad.IO.Class
import Data.List              (find)
import System.Directory
import System.Environment
import System.FilePath

#if __GLASGOW_HASKELL__ <= 708
import Control.Applicative    ((<$>))
#endif

type APIKey = String

-- | The file where API credentials are loaded when using
-- 'loadCredentialsDefault'.
--
-- Default: @$HOME/.airbrake-keys@
credentialsDefaultFile :: MonadIO m => m FilePath
credentialsDefaultFile = liftIO $ liftM (</> ".airbrake-keys") getHomeDirectory

-- | The key to be used in the loaded API credentials file, when using
-- 'loadCredentialsDefault'.
--
-- Default: @default@
credentialsDefaultKey :: String
credentialsDefaultKey = "default"

-- | Load API credentials from a text file given a key name.
--
-- The file should consist of newline-separated credentials in the
-- following format:
--
-- @keyName apiKey@
loadCredentialsFromFile :: MonadIO m => FilePath -> String -> m (Maybe APIKey)
loadCredentialsFromFile file key = liftIO $ do
    contents <- map words . lines <$> readFile file
    return $ do
        [_k, apikey] <- find (hasKey key) contents
        return apikey
    where
        hasKey _ [] = False
        hasKey k (k2 : _) = k == k2

-- | Load API credentials from the environment if possible, or alternately
-- from the default file with the default key name.
--
-- Default file: @$HOME/.airbrake-keys@
--
-- Default key: @default@
--
-- See 'loadCredentialsFromEnv' and 'loadCredentialsFromFile'.
loadCredentialsDefault :: MonadIO m => m (Maybe APIKey)
loadCredentialsDefault = do
    file <- credentialsDefaultFile
    loadCredentialsFromEnvOrFile file credentialsDefaultKey

-- | Load API credentials from the environment variable @AIRBRAKE_API_KEY@.
loadCredentialsFromEnv :: MonadIO m => m (Maybe APIKey)
loadCredentialsFromEnv = liftIO $ do
    env <- getEnvironment
    let lk = flip lookup env
        key = lk "AIRBRAKE_API_KEY"
    return key

-- | Load API credentials from the environment, or, failing that, from the
-- given file with the given key name.
--
-- See 'loadCredentialsFromEnv' and 'loadCredentialsFromFile'.
loadCredentialsFromEnvOrFile :: MonadIO m => FilePath -> String -> m (Maybe APIKey)
loadCredentialsFromEnvOrFile file key = do
    envcr <- loadCredentialsFromEnv
    case envcr of
        Just cr -> return (Just cr)
        Nothing -> loadCredentialsFromFile file key
