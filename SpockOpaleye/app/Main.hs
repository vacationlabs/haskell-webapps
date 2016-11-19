{-# LANGUAGE OverloadedStrings #-}

module Main where

import           DataTypes
import           JsonInstances              ()
import           TenantApi
import           Validations
import           Control.Lens

import           Web.Spock
import           Web.Spock.Config

import           Control.Monad.Reader
import           Control.Monad.Writer
import           Database.PostgreSQL.Simple 
import qualified Data.Text                  as T
import GHC.Int
import Control.Exception

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

runAppM :: Connection -> String -> TenantId -> Maybe UserId -> AppM a -> IO (Either SomeException a)
runAppM conn summary tid maybe_uid x = do
  _ <- setTxnEnv conn summary tid maybe_uid
  r <- try $ withTransaction conn $ runReaderT (runWriterT x) (conn, Nothing, Nothing)
  case  r of
    Right (item, lg)  -> do
      putStrLn lg
      _ <- execute_ conn $ read $ "discard all"
      return $ Right item
    Left e -> do
      _ <- execute_ conn $ read $ "discard all"
      return $ Left e 

setTxnEnv :: Connection -> String -> TenantId -> Maybe UserId -> IO GHC.Int.Int64
setTxnEnv conn summary tenant_id maybe_uid = do
  let user_id  = case maybe_uid of
                  Just (UserId u_id) -> u_id
                  Nothing -> 0
  _ <- execute_ conn $ read $ "set audit.summary = '" ++ summary ++ "'"
  _ <- execute_ conn $ read $ "set audit.currentuser = " ++ (show user_id)
  execute_ conn $ read $ "set audit.currenttenant = " ++ (show tenant_id)

app :: SpockM Connection MySession MyAppState ()
app = do
  post ("tenants/new") $ do 
      maybeTenantIncoming <- jsonBody
      result <- case maybeTenantIncoming of
         Just incomingTenant -> do
           runQuery (\conn -> do
             runAppM conn "creating tenant" (TenantId 1) Nothing $ do
               result <- liftIO $ validateIncomingTenant conn incomingTenant
               case result of
                 Valid -> do
                      newTenant <- createTenant incomingTenant
                      let modifiedTenant = newTenant & name .~ "Updated name"
                      _ <- updateTenant modifiedTenant
                      return $ Right newTenant
                 _ -> return $ Left $ T.pack "Validation fail"
               )
         Nothing -> return $ Right $ Left $ T.pack "Unrecognized input"
      case result of
        Right (Right nt) -> json nt
        Right (Left msg) -> json msg
        Left _ -> json $ T.pack "An exception occurred"
