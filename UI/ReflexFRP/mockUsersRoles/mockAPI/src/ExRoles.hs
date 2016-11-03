{-# LANGUAGE OverloadedStrings #-}

module ExRoles where

import MockAPI
import Permissions

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Monoid

exRoles = Roles $ accountAdministrator <> productAdministrator <> productEditor

allPermissions :: [Permission]
allPermissions = concat [ map PP [minBound .. maxBound]
                        , map OP [minBound .. maxBound]
                        , map UP [minBound .. maxBound]
                        ]

accountAdministrator = Map.singleton "Account administrator" (RoleAttributes roles users)
  where
    roles = Set.fromList allPermissions
    users = Set.fromList [ User "admin@mydomain.com"
                         , User "otheradmin@mydomain.com"
                         , User "yetanotheradmin@mydomain.com"
                         ]

productAdministrator = Map.singleton "Product administrator" (RoleAttributes roles users)
  where
    roles = Set.fromList $ map PP [minBound .. maxBound]
    users = Set.fromList [ User "user1@mydomain.com"
                         , User "user2@mydomain.com"
                         , User "user3@mydomain.com"
                         ]

productEditor = Map.singleton "Product editor" (RoleAttributes roles users)
  where
    roles = Set.fromList $ map PP [ViewAllProductDetails, EditProdTextualContent, EditProdPhotos]
    users = Set.fromList [ User "user4@mydomain.com"
                         , User "user5@mydomain.com"
                         , User "user6@mydomain.com"
                         , User "user7@mydomain.com"
                         , User "user8@mydomain.com"
                         , User "user9@mydomain.com"
                         , User "user10@mydomain.com"
                         , User "user11@mydomain.com"
                         ]
