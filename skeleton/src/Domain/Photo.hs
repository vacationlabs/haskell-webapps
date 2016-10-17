module Domain.Photo where

import Import
import Domain.Types

createPhoto :: ByteString -> AppM(Photo)
createPhoto bstring = undefined

removePhoto :: PhotoID -> AppM()
remotePhoto pid = undefined

getPhoto :: PhotoID -> Text -> AppM(Photo)
getPhoto pid geometryOrStyle = undefined
