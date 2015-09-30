library(RSQLite)
library(dplyr)
#drv <- dbDriver("SQLite")
#con<-dbConnect(drv, "X:/projects/who_heat/data/original_data/20150919_standalone_heat/Standalone/shiny/resources/data/HEMTK.db")

#dbGetQuery(con, "CREATE TABLE maindata_countries AS SELECT DISTINCT country, iso3, whoreg6, whoreg6_name, wbincome2014_4cat FROM maindata")
#dbGetQuery(con, "CREATE TABLE maindata_years AS SELECT DISTINCT country, year, source FROM maindata")

#dbGetQuery(con, "DROP TABLE maindata_years")


# 
# maindata<-dbGetQuery(con, "SELECT * FROM maindata")
# inequals<-dbGetQuery(con, "SELECT * FROM inequals")
# nationaldata<-dbGetQuery(con, "SELECT * FROM nationaldata")
# 
# countrynames <- distinct(.rdata[['maindata']], country, iso3, whoreg6, whoreg6_name, wbincome2014_4cat) %>% 
#select(country, iso3, whoreg6, whoreg6_name, wbincome2014_4cat)
# years <- select(maindata, country, year, source) %>%  distinct
# 
# saveRDS(maindata, "X:/projects/who_heat/web/who-heat/WHO_HETK/data/maindata.RDS")
# saveRDS(inequals, "X:/projects/who_heat/web/who-heat/WHO_HETK/data/inequals.RDS")
# saveRDS(nationaldata, "X:/projects/who_heat/web/who-heat/WHO_HETK/data/nationaldata.RDS")
# saveRDS(countrynames, "X:/projects/who_heat/web/who-heat/WHO_HETK/data/countrynames.RDS")
# saveRDS(years, "X:/projects/who_heat/web/who-heat/WHO_HETK/data/years.RDS")


# .rdata<-list()
# .rdata[['maindata']]<-readRDS("X:/projects/who_heat/web/who-heat/WHO_HETK/data/maindata.RDS")
# .rdata[['inequals']]<-readRDS("X:/projects/who_heat/web/who-heat/WHO_HETK/data/inequals.RDS")
# .rdata[['nationaldata']]<-readRDS("X:/projects/who_heat/web/who-heat/WHO_HETK/data/nationaldata.RDS")
# .rdata[['countrynames']]<-readRDS("X:/projects/who_heat/web/who-heat/WHO_HETK/data/countrynames.RDS")
# .rdata[['years']]<-readRDS("X:/projects/who_heat/web/who-heat/WHO_HETK/data/years.RDS")
# 
# library(dplyr)
# xx<-filter(.rdata[['inequals']], country=="Armenia", year==2010, indic=="carep")
# xx<-filter(.rdata[['inequals']], measure %in% c("rci", "mld"))
# 
# 
# .rdata[['focus_country']]<<-"Armenia"
# .rdata[['focus_indicator']]<<-c("carep")
# .rdata[['focus_dimension']]<<-c("Sex")
# .rdata[['focus_year']]<<-c(2010)
# 
# xx<-filter(.rdata[['maindata']], country=="Armenia", indic == "carep", dimension == 'Sex')

filter(.rdata[['countrynames']], TRUE, whoreg6_name=="Africa")


