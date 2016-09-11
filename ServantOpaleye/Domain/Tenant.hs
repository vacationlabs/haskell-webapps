{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
module Domain.Tenant where

import Domain.Base
import Opaleye
import Data.Text hiding (head)
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import qualified Database.PostgreSQL.Simple as PGS
import Control.Monad.Reader (runReaderT)
import Data.String.Conv

--
-- Tenant creation
--

data TenantCreationError = NonUniqueBackofficeDomainError


-- NOTE: Going with simple data-types for now (i.e. not creating different
-- data-types for Active/Inactive tenant). Let's see how this pans out.

-- tenantPgToApp :: TenantPGRead -> Tenant
-- tenantPgToApp pgTenant =

createTenant :: NewTenant -> AppM (Either TenantCreationError Tenant)
createTenant newTenant = do
  conn <- askDbConnection
  liftIO $ do
    -- TODO: Handle validation errors
    (Right . head) <$> runInsertManyReturning conn tenantTable [newTenantToPg newTenant] id

-- activateTenant :: TenantId -> AppM (Tenant)
-- activateTenant tenantId@(TenantId tid) = do
--   conn <- askDbConnection
--   liftIO $ do 
--     _ <- runUpdate conn tenantTable
--       (\tenant -> tenant & status .~ TenantActive)
--       (\tenant -> (tenant ^. key .== pgInt8 tid))
--     head <$> runQuery conn (tenantById tenantId)


--
-- main
--
testHarness = do
  conn <- PGS.connect PGS.defaultConnectInfo{
    PGS.connectUser = "servant_opaleye"
    ,PGS.connectPassword = "123"
    ,PGS.connectDatabase = "servant_opaleye"
    }
  let newTenant = Tenant{
        tenantKey = ()
        ,tenantCreatedAt = ()
        ,tenantUpdatedAt = ()
        ,tenantStatus = ()
        ,tenantOwnerId = Nothing
        ,tenantName = toS "Vacation Lab4"
        ,tenantBackofficeDomain = toS "http://app.vacatinlabs.com/vl4"
        }
  runReaderT (createTenant newTenant) AppConfig{appConfigDbPool=conn}
  -- where
  --   action = do
  --     conn <- askDbConnection
  --     liftIO $ (runQuery conn (tenantById $ TenantId 3) :: IO [Tenant])
