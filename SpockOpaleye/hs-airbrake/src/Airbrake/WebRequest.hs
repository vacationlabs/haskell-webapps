{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | A datatype representing the request attributes Airbrake wants to hear
-- about. Conversion functions are provided for:
--
-- * wai
module Airbrake.WebRequest (
    WebRequest (..), waiRequestToRequest
) where

import           Data.ByteString.UTF8 (toString)
import           Data.Maybe
import           Network.URI
import qualified Network.Wai          as Wai

data WebRequest = WebRequest
                {
                -- | The request URL.
                requestUrl         :: URI
                -- | Current route.
                -- This is a carryover from Rails-style MVC and is optional.
                , requestRoute     :: Maybe String
                -- | Controller action being used.
                -- This is a carryover from Rails-style MVC and is optional.
                , requestAction    :: Maybe String
                -- | Any other request metadata that you would like to include
                -- (server name, user agent, etc.)
                , requestOtherVars :: [(String, String)]
                } deriving (Eq, Ord, Show)

waiRequestToRequest :: Wai.Request -> WebRequest
waiRequestToRequest req = WebRequest{..} where
    requestUrl = fromMaybe
        (error "Failure producing URI from wai request.")
        (parseURI uriS)
    uriS = (if Wai.isSecure req then "https://" else "http://")
        ++ show (Wai.remoteHost req)
        ++ toString (Wai.rawPathInfo req)
        ++ toString (Wai.rawQueryString req)
    requestRoute = Nothing
    requestAction = Nothing
    requestOtherVars = catMaybes
        [ k "Host" "HTTP_HOST"
        , k "User-Agent" "HTTP_USER_AGENT"
        , k "Referer" "HTTP_REFERER"
        , k "Cookie" "HTTP_COOKIE"
        , if Wai.isSecure req then Just ("HTTPS", "on") else Nothing]
        where k hdr key = fmap (\ v -> (key, toString v))
                               (lookup hdr (Wai.requestHeaders req))
