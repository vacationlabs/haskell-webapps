{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module MockAPI.Permission where

import MockAPI.Prelude

-- A permission can be a permission in several ambiti

data Permission = PP ProductPermission | OP OrderPermission | UP UserPermission
                deriving (Show, Read, Eq, Ord, Generic)

data ProductPermission
  = ViewAllProductDetails
  | EditProdTextualContent
  | EditProdPhotos
  | EditProdProperties
  | EditProdPrice
  deriving (Show, Read, Eq, Ord, Generic, Bounded, Enum)

data OrderPermission
  = ViewAllOrderDetails
  | CreateOrder
  | EditOrder
  | ApplyDiscounts
  | CancelOrder
  | ChangeOrderContactDetails
  deriving (Show, Eq, Read, Ord, Generic, Bounded, Enum)

data UserPermission
  = ViewUserDetails
  | EditUserDetails
  | ChangeUserRole
  deriving (Show, Read, Eq, Ord, Generic, Bounded, Enum)

-- Generated JSON instances

instance ToJSON Permission
instance FromJSON Permission

instance ToJSON ProductPermission
instance FromJSON ProductPermission
instance ToJSON OrderPermission
instance FromJSON OrderPermission
instance ToJSON UserPermission
instance FromJSON UserPermission

-- We generate a class used to display a meaningful label to the user:

class UserLabel a where
  toUserLabel :: a -> Text

instance UserLabel ProductPermission where
  toUserLabel ViewAllProductDetails  = "View all product details"
  toUserLabel EditProdTextualContent = "Edit product textual content"
  toUserLabel EditProdPhotos         = "Edit product photos"
  toUserLabel EditProdProperties     = "Edit product properties"
  toUserLabel EditProdPrice          = "Edit product price "

instance UserLabel OrderPermission where
  toUserLabel ViewAllOrderDetails = "View all order details"
  toUserLabel CreateOrder = "Create a new order"
  toUserLabel EditOrder = "Edit an existing order (not created by self)"
  toUserLabel ApplyDiscounts = "Apply special discounts to orders"
  toUserLabel CancelOrder = "Cancel an order"
  toUserLabel ChangeOrderContactDetails = "Change contact details in an order"

instance UserLabel UserPermission where
  toUserLabel ViewUserDetails = "View user details"
  toUserLabel EditUserDetails = "Edit some other user's details"
  toUserLabel ChangeUserRole  = "Change roles assigned to a user"

instance UserLabel Permission where
  toUserLabel (PP p) = toUserLabel p
  toUserLabel (OP p) = toUserLabel p
  toUserLabel (UP p) = toUserLabel p
