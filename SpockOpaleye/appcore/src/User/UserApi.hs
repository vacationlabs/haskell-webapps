{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module UserApi where

import           Control.Monad.IO.Class
import           Control.Lens
import           UserDefs
import           ApiBase
import           AppM
import           Utils
import           Lenses
import           CryptoDef

createUser :: UserIncoming -> AppM User
createUser user = do
  Just hash <- liftIO $ bcryptPassword $ user ^. password
  let fullUser = user { _userpolyPassword = hash }
  auditable <$> (createRow userTable fullUser)
