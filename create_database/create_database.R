drv <- dbDriver("SQLite")
con<-dbConnect(drv, "X:/projects/who_heat/web/who-heat/WHO_HETK/data/HEMTK.db")

dbGetQuery(con, "CREATE TABLE maindata_countries AS SELECT DISTINCT country, iso3, whoreg6, whoreg6_name, wbincome2014_4cat FROM maindata")
dbGetQuery(con, "CREATE TABLE maindata_years AS SELECT DISTINCT country, year, source FROM maindata")

dbGetQuery(con, "DROP TABLE maindata_years")