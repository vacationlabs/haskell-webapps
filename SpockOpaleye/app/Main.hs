{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Data.Aeson                 (ToJSON (..), Value (..))
import           Database.PostgreSQL.Simple
import           DataTypes
import           JsonInstances              ()
import           TenantApi
import           Validations

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Text                  as T
import           Data.Time
import           Prelude                    hiding (id)
import           TH

--makeAudtableLenses [t| RolePoly RoleId TenantId T.Text (NonEmpty Permission) UTCTime UTCTime |]
makeAudtableLenses ''Role
makeAudtableLenses ''Tenant
makeAudtableLenses ''User

data MySession =
  EmptySession

data MyAppState = DummyAppState

connectDb :: IO Connection
connectDb = connect defaultConnectInfo { connectDatabase = "haskell-webapps" }

main :: IO ()
main = do
  spockCfg <-
    defaultSpockCfg
      EmptySession
      (PCConn $ ConnBuilder connectDb close (PoolCfg 10 10 10))
      DummyAppState
  runSpock 8080 (spock spockCfg app)

runAppM :: Connection -> AppM a -> IO a
runAppM conn x = do
  (item, lg) <- runReaderT (runWriterT x) (conn, Nothing, Nothing)
  putStrLn lg
  return item

getTestTenant :: Tenant
getTestTenant = Tenant (TenantId 1) tz tz "tjhon" "John" "Jacob" "john@gmail.com" "2342424" TenantStatusNew Nothing "Bo domain"
  where
      tz = UTCTime {
        utctDay = ModifiedJulianDay {
          toModifiedJulianDay = 0
          }
        , utctDayTime = secondsToDiffTime 0
      }


app :: SpockM Connection MySession MyAppState ()
app = do
  post ("tenants/new") $
    do maybeTenantIncoming <- jsonBody
       case maybeTenantIncoming of
         Just incomingTenant -> do
           result <- runQuery (\conn -> validateIncomingTenant conn incomingTenant)
           case result of
             Valid -> do
                  newTenant <- runQuery (\conn -> runAppM conn $ createTenant conn incomingTenant)
                  json newTenant
             _ -> json $ T.pack "Validation fail"
         Nothing -> json $ T.pack "Unrecognized input"
