
module AppSpec where

import           Control.Exception (throwIO)
import           Control.Monad.Trans.Except
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Test.Hspec

import           App hiding (getItems)

getItems :: Manager -> BaseUrl -> ClientM [Item]
getItem :: Integer -> Manager -> BaseUrl -> ClientM Item
getItems :<|> getItem = client itemApi

spec :: Spec
spec = do
  describe "/item" $ do
    withClient mkApp $ do
      it "lists an example item" $ \ host -> do
        try host getItems `shouldReturn` [Item 0 "example item"]

      it "allows to show items by id" $ \ host -> do
        try host (getItem 0) `shouldReturn` Item 0 "example item"

      it "throws a 404 for missing items" $ \ host -> do
        try host (getItem 42) `shouldThrow` (\ e -> responseStatus e == notFound404)

withClient :: IO Application -> SpecWith Host -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \ action -> \ manager -> do
      testWithApplication x $ \ port -> do
        let baseUrl = BaseUrl Http "localhost" port ""
        action (manager, baseUrl)

type Host = (Manager, BaseUrl)

try :: Host -> (Manager -> BaseUrl -> ClientM a) -> IO a
try (manager, baseUrl) action = either throwIO return =<<
  runExceptT (action manager baseUrl)
