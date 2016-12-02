module Main where

import Database.PostgreSQL.Simple

import OpaleyeBm
import PersistenceBm

-- export LC_ALL=C.UTF-8
-- You may have to run this command in your terminal if you get a char encoding/display error
main :: IO ()
main = do
  conn <- connect defaultConnectInfo { connectDatabase = "benchmark"}
  clearTables conn
  opaleye_benchmark conn
  clearTables conn
  persistence_benchmark
  clearTables conn
