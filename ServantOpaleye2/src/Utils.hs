module Utils where

import           AppM
import           Control.Monad.Catch
import           Exceptions

returnOneIfNE :: (MonadThrow m) => String -> [a] -> m a
returnOneIfNE _ (x:_) = return x
returnOneIfNE msg _   = throwM $ NotFoundException msg
