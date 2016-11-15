{-# LANGUAGE OverloadedStrings #-}

module UIElements (disabledEnabledButton) where

import Reflex
import Reflex.Dom
import Data.Text
import Data.Monoid
import Data.Map (Map)


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
                   [ const disableAttr <$> disable
                   , const enableAttr  <$> enable ]
    disableAttr = fmap Just initialAttr <> "disabled" =: Just "true"
    enableAttr  = fmap Just initialAttr <> "disabled" =: Nothing


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

