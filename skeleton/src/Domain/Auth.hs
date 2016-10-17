module Domain.Auth where

import Import
import Domain.Types

authenticateUser :: Text -> Text -> AppM(Bool)
authenticateUser username password = undefined

-- TODO: Should the domain API be dealing with sessions? Or should it be handled
-- by a layer sitting on top of Domain.Auth

createSession :: Text -> Text -> AppM(SessionID)
createSession username password = undefined

destroySession :: SessionID -> AppM()
destroySession sessid = undefined

