{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config where

import Control.Monad.Trans.Except
import Control.Monad.Reader
import Database.HDBC.PostgreSQL
import Servant


type AppM = ReaderT Config (ExceptT ServantErr IO) 

data Config = Config
    { getConnection :: Connection
    , getEnv :: Environment
    }

data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)


