{-# LANGUAGE DataKinds, FlexibleContexts, NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings, TypeApplications, TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
module Main where

import MockAPI
import Servant
import Servant.Router
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Gzip

import ClassyPrelude hiding (Handler, head, span)
import Control.Lens

import Text.Blaze.Html4.Transitional (Html, head, docTypeHtml, body, script, (!), title, meta, link)
import Text.Blaze.Html4.Transitional.Attributes hiding (title)
import Servant.HTML.Blaze

navigationServer :: Server NavigationServer
navigationServer =
  constHandler
    (Proxy @Navigation)
    (Proxy @Handler) initialPage

initialPage :: Html
initialPage = docTypeHtml $ do
    head $ do
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
        title "ui-mockups"
        link ! rel "stylesheet" ! href "assets/bootstrap/css/bootstrap.min.css"
        link ! rel "stylesheet" ! href "assets/fonts/ionicons.min.css"
        link ! rel "stylesheet" ! href "assets/css/Login-Form-Clean.css"
        script ! language "javascript" ! src "all.min.js" $ mempty
        link ! rel "stylesheet" ! href "assets/css/styles.min.css"
    body mempty

data Config = Config { rolesTVar :: TVar Roles }

server :: Config -> Server MockApi
server config
       = enter (runReaderTNat config) removeUser
    :<|> enter (runReaderTNat config) addRole
    :<|> enter (runReaderTNat config) showRoles
    :<|> serveAssets
    :<|> serveJS
  where
    serveAssets = serveDirectory "../mockClient/assets"
    serveJS     = serveDirectory "../mockClient/js/"

wholeServer :: Config -> Server WholeApi
wholeServer config = navigationServer :<|> server config

addRole :: (MonadReader Config m, MonadIO m) => RoleName -> RoleAttributes -> m NoContent
addRole rolename roleAttributes = do
  Config { rolesTVar = roles } <- ask
  liftIO $ atomically $ modifyTVar' roles
    (_Wrapped' . at rolename .~ Just roleAttributes)
  return NoContent

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
  run 8081 (gzip gzipSettings $ serve (Proxy @WholeApi) (wholeServer config))
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

accountAdministrator = singletonMap "AccountAdministrator" (RoleAttributes roles users)
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
