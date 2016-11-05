{-# LANGUAGE FlexibleContexts #-}

module Main where

import DataSource
import DBRelations



import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL




main :: IO ()
main = do
    conn    <- getDataSource

    dbUpdate conn updateTenant 1
    results <- dbQuery conn getTenant 1
    mapM_ (BL.putStrLn . encode) results
    --mapM_ print results

    dbDelete conn deleteRole 4 >>= print

    results <- dbQuery conn allRoles ()
    mapM_ print results
