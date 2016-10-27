module Main where

import Database.PostgreSQL.Simple
import DomainApi

main = 
  do conn <- connect defaultConnectInfo {connectDatabase = "haskell-webapps"}
     tenants <- read_tenants conn
     putStrLn $ show tenants
     return ()
