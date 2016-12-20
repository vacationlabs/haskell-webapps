{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- | Utilities for notifying Airbrake of errors. An 'Error' type is
-- provided; you can convert any instance of 'Exception' to an 'Error'
-- using 'toError', which uses the exception's 'Typeable' instance.
--
-- Airbrake requires a stack trace for any reported exception, but stack
-- trace information isn't readily available for Haskell exceptions.
-- 'notifyQ' and 'notifyReqQ' are provided for the purpose of providing the
-- current file position as the stack trace.
module Airbrake (
    -- * Notifying
    notify, notifyReq,
    notifyQ, notifyReqQ,

    -- * Notification metadata
    -- *** Location lists
    NonEmpty (..), Location, Locations,

    -- *** Wrapping errors
    toError, Error (..),

    -- * Configuration building
    APIKey, Environment,
    airbrakeConf, defaultApiEndpoint,
    AirbrakeConf (..),
    Server (..),

    -- * Convenience exports
    module Airbrake.Credentials
) where

import           Airbrake.Credentials        hiding (APIKey)
import qualified Airbrake.WebRequest         as W
import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.ByteString.Lazy        (ByteString)
import           Data.Foldable
import           Data.List.NonEmpty
import           Data.String
import           Data.Text                   (pack)
import qualified Data.Text                   as T (Text)
import           Data.Typeable               (typeOf)
import           Data.Version
import           Language.Haskell.TH.Syntax  hiding (report)
import           Network.HTTP.Conduit
import qualified Paths_airbrake              as P
import           Prelude                     hiding (error)
import           Text.Blaze
import           Text.Blaze.Internal
import           Text.Blaze.Renderer.Utf8

type APIKey = String
type Environment = String

data Error = Error
           { errorType        :: T.Text
           , errorDescription :: T.Text
           }

-- | Information to use when communicating with Airbrake.
data AirbrakeConf = AirbrakeConf
                  { acApiEndpoint :: String
                  , acApiKey      :: APIKey
                  , acServer      :: Server
                  }

-- | Metadata about the server.
data Server = Server
            { serverEnvironment :: Environment
            , serverAppVersion  :: Maybe Version
            , serverRoot        :: Maybe FilePath
            }

-- | A @(filename, line)@ pair.
type Location = (FilePath, Int)

type Locations = NonEmpty Location

-- | @"http:\/\/api.airbrake.io\/notifier_api\/v2\/notices"@
defaultApiEndpoint :: String
defaultApiEndpoint = "http://api.airbrake.io/notifier_api/v2/notices"

airbrakeConf :: APIKey -> Environment -> AirbrakeConf
airbrakeConf k env =
    AirbrakeConf defaultApiEndpoint k (Server env Nothing Nothing)

performNotify :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
              => Locations -> AirbrakeConf -> Maybe W.WebRequest -> Error -> m ()
performNotify loc conf req e = do
    let report = buildReport loc conf req e
    req' <- parseUrl (acApiEndpoint conf)
    let rq = req' { requestBody = RequestBodyLBS report, method = "POST" }
    man <- liftIO $ newManager tlsManagerSettings
    _ <- httpLbs rq man
    return ()

-- | Notify Airbrake of an exception.
notify :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
       => AirbrakeConf -> Error -> Locations -> m ()
notify conf e l = performNotify l conf Nothing e

-- | Notify Airbrake of an exception, providing request metadata along with
-- it.
notifyReq :: (MonadBaseControl IO m, MonadIO m, MonadThrow m)
          => AirbrakeConf -> W.WebRequest -> Error -> Locations -> m ()
notifyReq conf req e l = performNotify l conf (Just req) e

-- | 'notify', fetching the current file location using Template Haskell.
--
-- @
-- $notifyQ :: ('MonadBaseControl' 'IO' m, 'MonadThrow' m, 'MonadIO' m)
--          => 'AirbrakeConf' -> 'Error' -> m ()
-- @
notifyQ :: Q Exp
notifyQ = do
    Loc fn _ _ (st, _) _ <- qLocation
    [| \ cc ee -> notify cc ee ((fn, st) :| []) |]

-- | 'notifyReq', fetching the current file location using Template
-- Haskell.
--
-- @
-- $notifyReqQ :: ('MonadBaseControl' 'IO' m, 'MonadThrow' m, 'MonadIO' m, 'W.WebRequest' req)
--             => 'AirbrakeConf' -> req -> 'Error' -> m ()
-- @
notifyReqQ :: Q Exp
notifyReqQ = do
    Loc fn _ _ (st, _) _ <- qLocation
    [| \ cc r ee -> notifyReq cc r ee ((fn, st) :| []) |]

-- | Convert any 'Exception' to an 'Error'.
toError :: Exception e => e -> Error
toError (toException -> SomeException e) =
    Error (pack (show (typeOf e))) (pack (show e))

buildReport :: Locations -> AirbrakeConf -> Maybe W.WebRequest -> Error -> ByteString
buildReport locs conf req err = renderMarkup $ do
    preEscapedText "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    notice ! nversion "2.3" $ do
        api_key . toMarkup $ acApiKey conf

        notifier $ do
            name "airbrake"
            version . toMarkup $ showVersion P.version
            url "http://hackage.haskell.org/package/airbrake"

        error $ do
            class_ (toMarkup (errorType err))
            message (toMarkup (errorDescription err))
            backtrace $ forM_ locs $ \ (filename, line') ->
                line ! file (toValue filename)
                     ! number (toValue line')

        forM_ req $ \ r -> request $ do
            url (toMarkup . show $ W.requestUrl r)
            forM_ (W.requestRoute r) $ \ rt -> component (toMarkup rt)
            forM_ (W.requestAction r) $ \ act -> action (toMarkup act)
            cgi_data . forM_ (W.requestOtherVars r) $ \ (k, v) ->
                var ! key (toValue k) $ toMarkup v

        let serv = acServer conf
        server_environment $ do
            environment_name . toMarkup $ serverEnvironment serv
            forM_ (serverAppVersion serv) $ \ v ->
                app_version (toMarkup $ showVersion v)

            forM_ (serverRoot serv) $ \ v ->
                project_root (toMarkup v)
    where
        notice = Parent "notice" "<notice" "</notice>"
        name = Parent "name" "<name" "</name>"
        notifier = Parent "notifier" "<notifier" "</notifier>"
        api_key = Parent "api-key" "<api-key" "</api-key>"
        version = Parent "version" "<version" "</version>"
        url = Parent "url" "<url" "</url>"
        class_ = Parent "class" "<class" "</class>"
        error = Parent "error" "<error" "</error>"
        message = Parent "message" "<message" "</message>"
        backtrace = Parent "backtrace" "<backtrace" "</backtrace>"
        line = Leaf "line" "<line" " />"
        file = attribute "file" " file=\""
        number = attribute "number" " number=\""
        server_environment = Parent "server-environment" "<server-environment"
                                 "</server-environment>"
        environment_name = Parent "environment-name" "<environment-name"
                               "</environment-name>"
        app_version = Parent "app-version" "<app-version" "</app-version>"
        project_root = Parent "project-root" "<project-root" "</project-root>"
        request = Parent "request" "<request" "</request>"
        cgi_data = Parent "cgi-data" "<cgi-data" "</cgi-data>"
        action = Parent "action" "<action" "</action>"
        component = Parent "component" "<component" "</component>"
        var = Parent "var" "<var" "</var>"
        key = attribute "key" " key=\""
        nversion = attribute "version" " version=\""
