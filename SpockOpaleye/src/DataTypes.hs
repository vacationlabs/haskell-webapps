{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}

module DataTypes where

import           Control.Lens
import qualified Control.Monad.Reader as R
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Writer
import           Data.List.NonEmpty
import           Data.Text
import           Data.Time                  (UTCTime)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Data.Aeson (Value(..))
import           Data.ByteString

import Control.Exception
import Control.Monad.Trans.Except
import           Data.Time
import TenantDefs
import UserDefs
import TH

data ValidationResult = Valid | Invalid String
  deriving (Eq, Show)



type AppM a = WriterT ByteString (ReaderT (Connection, Maybe Tenant, Maybe User) (ExceptT SomeException IO)) a
