{-# LANGUAGE LambdaCase #-}

{-|
Module      :  Relations.DB
Copyright   :  (c) VacationLabs
Maintainer  :  michaelkarg77@gmail.com

Helper definitions for variadic updates of some data type.
-}

module  Relations.DB where

import  Database.Relational.Query                       (ShowConstantTermsSQL, (<-#), value)
import  Database.Relational.Query.Monad.Trans.Assigning (AssignTarget, Assignings)
import  Data.Default


data VariadicArg a = None | NewVal a

instance Default (VariadicArg a) where
    def = None

varArg :: Eq a => (b -> a) -> b -> b -> VariadicArg a
varArg getter old new =
    let a = getter new
    in if a /= getter old then NewVal a else None

-- add an assigning to the target iff the variadic attribute provides a new value
(<-#?) :: (ShowConstantTermsSQL v, Monad m)
    => AssignTarget r v -> VariadicArg v -> Assignings r m ()
(<-#?) proj = \case
    None        -> return ()
    NewVal val  -> proj <-# value val

infix 4 <-#?
