{-# LANGUAGE DataKinds, FlexibleContexts, NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings, TypeApplications, TypeOperators #-}

module Main where

import MockAPI
import Servant
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip

import ClassyPrelude
import Control.Lens

data Config = Config { rolesTVar :: TVar Roles }

server :: Config -> Server MockApi
server config
       = enter (runReaderTNat config) removeUser
    :<|> enter (runReaderTNat config) showRoles
    :<|> serveAssets
    :<|> serveJS
  where
    serveAssets = serveDirectory "../mockClient/assets"
    serveJS     = serveDirectory "../mockClient/js/"

showRoles :: (MonadReader Config m, MonadIO m) => m Roles
showRoles = do
  Config { rolesTVar = roles } <- ask
  liftIO $ readTVarIO roles

removeUser :: (MonadReader Config m, MonadIO m) => RoleName -> User -> m NoContent
removeUser rolename user = do
  Config { rolesTVar = roles } <- ask
  liftIO $ atomically $ modifyTVar' roles
    (_Wrapped' . at rolename . _Just . roleAssociatedUsers . contains user .~ False)
  return NoContent

main :: IO ()
main = do
  state <- atomically $ newTVar exRoles
  let config = Config state
  run 8081 (gzip gzipSettings $ serve (Proxy @MockApi) (server config))
  where
    gzipSettings = def { gzipFiles = GzipCompress }

--------------------------------------------------------------------------------
---- Example roles to be served by the server:
--------------------------------------------------------------------------------

exRoles = Roles $ accountAdministrator <> productAdministrator <> productEditor

allPermissions :: [Permission]
allPermissions = concat [ map PP [minBound .. maxBound]
                        , map OP [minBound .. maxBound]
                        , map UP [minBound .. maxBound]
                        ]

accountAdministrator = singletonMap "Account administrator" (RoleAttributes roles users)
  where
    roles = setFromList allPermissions
    users = setFromList [ User "admin@mydomain.com"
                        , User "otheradmin@mydomain.com"
                        , User "yetanotheradmin@mydomain.com"
                        ]

productAdministrator = singletonMap "Product administrator" (RoleAttributes roles users)
  where
    roles = setFromList $ map PP [minBound .. maxBound]
    users = setFromList [ User "user1@mydomain.com"
                        , User "user2@mydomain.com"
                        , User "user3@mydomain.com"
                        ]

productEditor = singletonMap "Product editor" (RoleAttributes roles users)
  where
    roles = setFromList $ map PP [ViewAllProductDetails, EditProdTextualContent, EditProdPhotos]
    users = setFromList [ User "user4@mydomain.com"
                        , User "user5@mydomain.com"
                        , User "user6@mydomain.com"
                        , User "user7@mydomain.com"
                        , User "user8@mydomain.com"
                        , User "user9@mydomain.com"
                        , User "user10@mydomain.com"
                        , User "user11@mydomain.com"
                        ]
