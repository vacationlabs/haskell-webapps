module Main where

import Lib
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


main :: IO ()
main = do
    randomSource <- mkRandomSource drgNew 1000
    serverKey <- mkServerKey 16 Nothing
    let config = Config def randomSource serverKey
    run 8080 $ serveWithContext (Proxy :: Proxy TestAPI)
                                ((defaultAuthHandler def serverKey :: AuthHandler Request Session) :. EmptyContext)
                                (server config)
