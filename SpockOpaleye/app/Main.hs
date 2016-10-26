module Main where

import Database.PostgreSQL.Simple
import DomainApi

main :: IO ()
main = 
  do conn <- connect defaultConnectInfo {connectDatabase = "haskell-webapps"}
     create_tenant conn get_tenant
     return ()
