module Utils where

import AppM
import Control.Exception
import Exceptions

returnOneIfNE :: (Monad m) => String -> [a] -> m a
returnOneIfNE _ (x:_) = return x
returnOneIfNE msg _ = throw $ NotFoundException msg
