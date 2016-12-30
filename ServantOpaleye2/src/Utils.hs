module Utils where

import           AppM
import           Control.Monad.Catch
import           Exceptions
import           AppCore

returnOneIfNE :: (MonadThrow m) => String -> [a] -> m a
returnOneIfNE _ (x:_) = return x
returnOneIfNE msg _   = throwM $ NotFoundException msg

requireRole :: (MonadThrow m) => CookieData -> RoleName -> m ()
requireRole cd rn = return ()
