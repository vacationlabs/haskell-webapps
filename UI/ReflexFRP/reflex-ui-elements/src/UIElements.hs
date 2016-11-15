{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
module UIElements (apiButton, disabledEnabledButton) where

import Reflex
import Reflex.Dom
import Data.Text
import Data.Monoid
import Data.Map (Map)

apiButton :: MonadWidget t m
          => Text                          -- ^ Text of the button
          -> Map AttributeName Text        -- ^ Initial attributes for the button
          -> (Event t () -> m (Event t r)) -- ^ Function transforming a click in an event a la servant-reflex
          -> m (Event t r)                 -- ^ The result value of the api call
apiButton label initialAttr f = do
  rec (e, _) <- element "button" conf (text label)
      let click = domEvent Click e
      apiResponse <- f (() <$ click)
      let conf = def & elementConfig_initialAttributes .~ initialAttr
                     & elementConfig_modifyAttributes  .~ mergeWith (\_ b -> b)
                           [ const disabled <$> click
                           , const enabled  <$> apiResponse ]
  return apiResponse
 where
  disabled = fmap Just initialAttr <> "disabled" =: Just "true"
  enabled  = fmap Just initialAttr <> "disabled" =: Nothing

--------------------------------------------------------------------------------
-- Old code

-- | This function constructs a button that has the enable or disable attribute
--   set when two events fire.
disabledEnabledButton :: DomBuilder t m
                      => Event t ()    -- ^ The event on which the button is enabled
                      -> Event t ()    -- ^ The event on which the button is disabled
                      -> Text          -- ^ Caption for the button
                      -> Map AttributeName Text -- ^ The initial attributes
                      -> m (Event t ())
disabledEnabledButton enable disable text initialAttr = styledButton conf text
  where
    conf = def & elementConfig_initialAttributes .~ initialAttr
               & elementConfig_modifyAttributes  .~ mergeWith (\_ b -> b)
                   [ const disabled <$> disable
                   , const enabled  <$> enable ]
    disabled = fmap Just initialAttr <> "disabled" =: Just "true"
    enabled  = fmap Just initialAttr <> "disabled" =: Nothing

styledButton :: DomBuilder t m => ElementConfig EventResult t m -> Text -> m (Event t ())
styledButton conf t = do
  (e, _) <- element "button" conf (text t)
  return (domEvent Click e)


-- There might be a way to generalize this construction, but I'll check it when
-- I'll be focused on reflex; something along the lines of

-- enableDisable :: Monad m
--               => Event t ()
--               -> Event t ()
--               -> m (Element er (DomBuilderSpace m) t)
--               -> m (Element er (DomBuilderSpace m) t)
-- enableDisable enableEvent disableEvent element = _what $
--   fmap (elementConfig_modifyAttributes .~ _what) element

