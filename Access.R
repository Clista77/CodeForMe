#R(32-bit)
library(RODBC)
data=odbcConnectAccess2007("Database.accdb")
sqlTables(data)$TABLE_NAME
request=sqlFetch(data,"repuest")
support=sqlFetch(data,"support")
