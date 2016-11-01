{-# LANGUAGE OverloadedStrings #-}
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
import Models
import Control.Monad.Logger
import Database.Persist.Postgresql


main :: IO ()
main = do
    randomSource <- mkRandomSource drgNew 10000
    serverKey <- mkServerKey 256 Nothing
    pool <- makePool
    flip runSqlPool pool $ runMigration migrateAll
    let config = Config { authSettings = (def {acsCookieFlags = ["HttpOnly"]}) 
                        , randomSource = randomSource
                        , serverKey = serverKey
                        , environment = Devel
                        , dbPool = pool
                        }
    run 8080 $ serveWithContext (testAPI)
                                ((cookieAuthHandler config) :. EmptyContext)
                                (testServer config)

connStr = "host=localhost dbname=perservant user=test password=test port=5432"

makePool = runStdoutLoggingT $ createPostgresqlPool connStr 1
