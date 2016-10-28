module Environ
    where

import Control.Monad.Reader
import Types

inEnvironment :: Environment -> App () -> App ()
inEnvironment e m = do
    env <- asks environment
    when (e == env) m

inDevel :: App () -> App ()
inDevel = inEnvironment Devel
