{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module GenericStructure where

import Data.Aeson
import Data.Text
import GHC.Generics
import Control.Monad.Identity
import Control.Applicative
import Data.Functor.Classes

newtype Mail = Mail {unMail :: Text} deriving (Eq, Show)

data User' f = User'
  { userName :: f Text
  , userMail :: f Mail
  }

instance (Show1 f) => Show (User' f) where
  show (User' ft fm) = Prelude.unwords ["User'", par $ show1 ft, par $ show1 fm]
    where par x = "(" ++ x ++ ")"
          show1 x = showsPrec1 0 x ""

instance Eq1 f => Eq (User' f) where
  (User' ft1 fm1) == (User' ft2 fm2) = eq1 ft1 ft2

type Simple x    = x Identity
type Validated x = x Validation
type Diff x      = x Difference

newtype Validation a = Validation { unValidation :: Const (a -> Text) a }
newtype Difference a = Difference { unDifference :: Maybe a } deriving (Show1)

-- Let's test some composite structure
data CoupleOfUsers' f = CoupleOfUsers'
                        { firstUser  :: f (User' f)
                        , secondUser :: f (User' f)
                        }

instance (Show1 f) => Show (CoupleOfUsers' f) where
  show (CoupleOfUsers' ft fm) = Prelude.unwords ["CoupleOfUsers'", par $ show1 ft, par $ show1 fm]
    where par x = "(" ++ x ++ ")"
          show1 x = showsPrec1 0 x ""

couple :: Simple User' -> Simple User' -> Simple CoupleOfUsers'
couple u1 u2 = CoupleOfUsers' (Identity u1) (Identity u2)

-- Example Users
john, jack, jill :: Simple User'
john = User' (Identity "john") (Identity (Mail "john@gmail.com"))
jack = User' (Identity "jack") (Identity (Mail "jack@gmail.com"))
jill = User' (Identity "jill") (Identity (Mail "jill@gmail.com"))

-- let's write a diff changing the name of the first user:
exDiff :: Diff CoupleOfUsers'
exDiff = CoupleOfUsers' (dj $ User' (dj "newName") dn) dn
  where
    dj = Difference . Just
    dn = Difference Nothing

-- Could this difference be computed automatically?
diffUser :: Simple User' -> Simple User' -> Diff User'
diffUser (User' (Identity n1) (Identity m1)) (User' (Identity n2) (Identity m2)) =
  User' (if n1 == n2 then Difference Nothing else Difference (Just n2))
        (if m1 == m2 then Difference Nothing else Difference (Just m2))

diffCouple :: Simple CoupleOfUsers' -> Simple CoupleOfUsers' -> Diff CoupleOfUsers'
diffCouple (CoupleOfUsers' (Identity a1) (Identity b1)) (CoupleOfUsers' (Identity a2) (Identity b2)) =
  CoupleOfUsers' (if a1 == a2 then Difference Nothing else Difference (Just $ diffUser a1 a2))
                 (if b1 == b2 then Difference Nothing else Difference (Just $ diffUser b1 b2))

-- This behavior should be abstracted in a typeclass.
