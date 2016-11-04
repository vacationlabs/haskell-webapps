
module  DataSource where

import  Database.HDBC.PostgreSQL    (connectPostgreSQL, Connection)

pgSchemaName :: String
pgSchemaName = "public"

getDataSource :: IO Connection
getDataSource =
    connectPostgreSQL "dbname=vacationlabs"
