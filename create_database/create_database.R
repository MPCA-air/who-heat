library(gdata)
library(dplyr)
library(readxl)
path<-"X:/projects/who_heat/data/original_data/20151006_updated_database/HEAT database (national in rows) 2015-10.xlsx"


types<-c("text", "numeric", "text", "text", "text", "text","text",
         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
         "text", "text", "text", "text", "text", "text", 
         "text", "numeric", "numeric", "numeric")
dat<-read_excel(path, col_names=TRUE, col_types = types)


indicpath<-"X:/projects/who_heat/data/original_data/20151006_updated_database/HEAT indicator list 2015-10.xlsx"
indicators <- read_excel(indicpath) %>% 
  filter(show==1)

dat <- semi_join(dat, indicators, by="indic")



maindata <- filter(dat, dimension != "National average")
nationaldata <- filter(dat, dimension == "National average")
countrynames <- distinct(maindata, country, iso3, whoreg6, whoreg6_name, wbincome) %>% 
  select(country, iso3, whoreg6, whoreg6_name, wbincome)
years <- select(maindata, country, year, source) %>%  distinct

# if you want to compare to previous skip part above
#maindata<-readRDS("X:/projects/who_heat/web/who-heat/WHO_HETK/data/maindata.RDS")
#maindata<-semi_join(maindata, indicators, by="indic")
#maindata <- rename(maindata, order = rankorder)
#nationaldata<-readRDS("X:/projects/who_heat/web/who-heat/WHO_HETK/data/nationaldata.RDS")
#nationaldata <- rename(nationaldata, wbincome = wbincome2014_4cat)





nationaldata <- rename(nationaldata,
                       r_national=r,
                       r_national_lower = r_lower,
                       r_national_upper = r_upper,
                       se_national  = se,
                       flag_national = flag)

nationaldata <- nationaldata[, c("country", "year", "source", "indic", "indic_name", 
                 "r_national", "r_national_lower", "r_national_upper",
                 "se_national", "flag_national")]


forInequal <- left_join(maindata, 
                        nationaldata,
                        by=c("country", "year", "source", "indic", "indic_name"))


strata <- filter(forInequal, !grepl("Not available", flag)) %>% 
  distinct(country, year, source, indic, indic_name, dimension)




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
countrynames <- distinct(.rdata[['maindata']], country, iso3, whoreg6, whoreg6_name, wbincome2014_4cat) %>% 
select(country, iso3, whoreg6, whoreg6_name, wbincome2014_4cat)
years <- select(maindata, country, year, source) %>%  distinct
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


