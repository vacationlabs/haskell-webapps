module Domain.User where

import Import
import Domain.Types

createUser :: User -> AppM(User)
createUser user = undefined

activateUser :: User -> Text -> AppM(User)
activateUser user activationKey = undefined

deactivateUser :: User -> AppM(User)
deactivateUser user = undefined

getUser :: UserID -> AppM(User)
getUser uid = undefined

