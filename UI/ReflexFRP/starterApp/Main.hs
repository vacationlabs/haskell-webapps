{-# LANGUAGE NoImplicitPrelude, NoMonomorphismRestriction, OverloadedStrings #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, ViewPatterns                  #-}

module Main where

import ClassyPrelude
import Reflex
import Reflex.Dom
import Language.Javascript.JSaddle.Warp (run)

import Data.String.Conv    (toS)
import Text.Email.Validate (toByteString, validate, EmailAddress)

--------------------------------------------------------------------------------

{- Note: The structure of the application

As you can see the structure of the main function is quite linear, and
corresponds to the high level structure of the feature. The most important
functions are validateInput and notifyLogin, defined below.

The htmlHead function provides some styling from a cdn for convenience.
-}

htmlHead :: MonadWidget t m => m ()
htmlHead = do
  styleSheet "https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
  styleSheet "https://cdnjs.cloudflare.com/ajax/libs/milligram/1.1.0/milligram.min.css"
  where
    styleSheet addr = elAttr "link" ("rel"  =: "stylesheet" <> "href" =: addr) (return ())

main :: IO ()
main = run 8081 $ mainWidgetWithHead htmlHead $ do
  el "h1" (text "A validation demo")
  rec firstName <- validateInput "First Name:" nameValidation  signUpButton
      lastName  <- validateInput "Last Name:"  nameValidation  signUpButton
      mail      <- validateInput "Email:"      emailValidation signUpButton
      age       <- validateInput "Age:"        ageValidation   signUpButton
      signUpButton <- button "Sign up"
  let user = (liftM4 . liftM4) User firstName lastName mail age
  notifyLogin user signUpButton

--------------------------------------------------------------------------------
-- Data modeling

data User = User { firstName :: Text
                 , lastName :: Text
                 , email :: EmailAddress
                 , age :: Int
                 } deriving (Eq, Show)

prettyPrint :: User -> Text
prettyPrint u = unwords [ firstName u , lastName u
                        , parenthesized (toS . toByteString $ email u)]
  where
    parenthesized s = "(" <> s <> ")"

--------------------------------------------------------------------------------

{- Note: A general function to construct data validations

The validateInput function takes as parameters: the prompt used to ask the user
for a specific information, a _pure_ validation function and an event to
syncronize the update with (in this example the signUpButton - note that with
less effort the output could be always updated instantly, but in this case it
didn't fell right from an UI perspective).

The function then draws the required elements on the dom and handles internally
all the necessary signal plumbing, only exposing the final, validated signal.

The function is further commented in the code below:
-}

type Prompt = Text

validateInput :: MonadWidget t m
              => Prompt                  -- ^ The text on the label
              -> (Text -> Either Text a) -- ^ A pure validation function
              -> Event t b               -- ^ An event so syncronize the update with
              -> m (Dynamic t (Maybe a)) -- ^ Returns a validated value
validateInput prompt pureValidation event = do
  -- 1) Declaring the graphical interface: the prompt and the input field
  text prompt
  inputField <- textInput def

  -- 2) Using the pure validation function to validate the data construct a
  -- signal for the "hidden" html property of the feedback label and a signal
  -- for an eventual error message, all tied to the sign in button events
  let queryResult = pureValidation <$> _textInput_value inputField
  hiddenAttr <- resampleOn event hidden
                  (either (const mempty) (const hidden) <$> queryResult)
  err        <- resampleOn event ""
                  (either id (const "") <$> queryResult)

  -- 3) Optionally showing a label containing the eventual error, and returning
  -- the validated content of the query for further processing
  elDynAttr "p" hiddenAttr (dynText err)
  return (either (const Nothing) Just <$> queryResult)

--------------------------------------------------------------------------------
-- A function to notify the login of the user, following the same logic of
-- validateInput.

notifyLogin :: MonadWidget t m
            => Dynamic t (Maybe User) -> Event t b -> m ()
notifyLogin user event = do
  hiddenAttr  <- resampleOn event hidden (maybe hidden (const mempty) <$> user)
  loginReport <- resampleOn event ""     (maybe ""     prettyPrint    <$> user)
  elDynAttr "h5" hiddenAttr (do text "User "; dynText loginReport; text " logged in")

--------------------------------------------------------------------------------

{- Note: Validation with pure functions

Down below there are some pure functions used for validation: they all take the
text from the input field as a parameter, and return either an error to be
displayed, or the value correctly parsed. This ensures the complete separation
between the validation logic and the presentation of the application.

For the email validation part, I used the standard implemented in the
email-validate package
-}

ageValidation :: Text -> Either Text Int
ageValidation (readMay -> Just a :: Maybe Int)
  | 18 <= a && a <= 120 = Right a
  | a <= 18             = Left "You must be at least 18 years old."
  | 120 <= a            = Left "No way. Try to pick a reasonable fake age."
ageValidation _         = Left "Please enter your age."

nameValidation :: Text -> Either Text Text
nameValidation "" = Left "Please enter a non-empty name."
nameValidation n  = Right n

emailValidation :: Text -> Either Text EmailAddress
emailValidation ""                             = Left "Please enter your email address"
emailValidation (validate . toS -> Right addr) = Right addr
emailValidation _                              = Left "Your email address seems to be incorrect"

--------------------------------------------------------------------------------
-- Utilities

-- A commonly used property in this demo
hidden :: Map Text Text
hidden = "hidden" =: "true"

-- Given an event, an initial value, and a dynamic, this function creates a
-- dynamic that only changes values when the event fires (starting with the
-- initial value)
resampleOn :: (MonadWidget t m) => Event t b -> a -> Dynamic t a -> m (Dynamic t a)
resampleOn event initial originalDyn = holdDyn initial (tag (current originalDyn) event)
