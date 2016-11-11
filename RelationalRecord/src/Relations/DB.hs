
{-|
Module      :  Relations.DB
Copyright   :  (c) VacationLabs
Maintainer  :  michaelkarg77@gmail.com

Helper definition for variadic updates of some data type.
-}

module  Relations.DB where

import  Database.Relational.Query                       (ShowConstantTermsSQL, (<-#), value)
import  Database.Relational.Query.Monad.Trans.Assigning (AssignTarget, Assignings)

import  Data.Maybe
import  Control.Monad   (when)


-- add an assigning to the target iff the attribute is 'Just _'
(<-#?) :: (ShowConstantTermsSQL v, Monad m)
    => AssignTarget r v -> Maybe v -> Assignings r m ()
(<-#?) proj attr =
    when (isJust attr) $ proj <-# value (fromJust attr)

infix 4 <-#?
