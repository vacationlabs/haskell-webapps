{-# LANGUAGE FlexibleContexts #-}

module Main where

import DataSource
import DBRelations



import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL




main :: IO ()
main = do
    conn    <- getDataSource
    results <- dbQuery conn () allTenants
    mapM_ (BL.putStrLn . encode) results
    --mapM_ print results

    dbDelete conn () (deleteRole 4) >>= print
