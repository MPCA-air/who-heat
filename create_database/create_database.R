library(RSQLite)
library(dplyr)
drv <- dbDriver("SQLite")
con<-dbConnect(drv, "X:/projects/who_heat/web/who-heat/WHO_HETK/data/HEMTK.db")

dbGetQuery(con, "CREATE TABLE maindata_countries AS SELECT DISTINCT country, iso3, whoreg6, whoreg6_name, wbincome2014_4cat FROM maindata")
dbGetQuery(con, "CREATE TABLE maindata_years AS SELECT DISTINCT country, year, source FROM maindata")

dbGetQuery(con, "DROP TABLE maindata_years")



maindata<-dbGetQuery(con, "SELECT * FROM maindata")
inequal<-dbGetQuery(con, "SELECT * FROM inequals")
nationaldata<-dbGetQuery(con, "SELECT * FROM nationaldata")

countrynames <- distinct(maindata, country, iso3, whoreg6, whoreg6_name, wbincome2014_4cat)
year <- distinct(maindata, country, year)

saveRDS(maindata, "X:/projects/who_heat/web/who-heat/WHO_HETK/data/maindata.RDS")
saveRDS(maindata, "X:/projects/who_heat/web/who-heat/WHO_HETK/data/maindata.RDS")