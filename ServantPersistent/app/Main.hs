module Main where

import API
import Auth
import Data.Proxy
import Crypto.Random
import Servant
import Network.Wai
import Servant.API.Experimental.Auth
import Servant.Server.Experimental.Auth.Cookie
import Servant.Server.Experimental.Auth
import Types
import Data.Default
import Network.Wai.Handler.Warp
import Server


main :: IO ()
main = do
    randomSource <- mkRandomSource drgNew 10000
    serverKey <- mkServerKey 256 Nothing
    let config = Config (def {acsCookieFlags = ["HttpOnly"]}) randomSource serverKey Devel
    run 8080 $ serveWithContext (testAPI)
                                ((cookieAuthHandler def serverKey) :. EmptyContext)
                                (testServer config)
