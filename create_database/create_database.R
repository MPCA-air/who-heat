#******************************************************************************
# Script to create the RDS files for the Shiny app
#******************************************************************************


library(dplyr)
library(readxl)

datpath<-"X:/projects/who_heat/data/original_data/20151006_updated_database/HEAT database (national in rows) 2015-10.xlsx"
indicpath<-"X:/projects/who_heat/data/original_data/20151006_updated_database/HEAT indicator list 2015-10.xlsx"
outpath <- "d:/junk/who/"

# ----- here is the raw data. Both national-level and disaggregated 
# ----- data as rows



types<-c("text", "numeric", "text", "text", "text", "text","text",
         "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
         "text", "text", "text", "text", "text", "text", 
         "text", "numeric", "numeric", "numeric")

print("Reading Excel data file")
dat<-read_excel(datpath, col_names=TRUE, col_types = types)


# ----- This is the table of indicators with names and abbreviations


indicators <- read_excel(indicpath) %>% 
  filter(show==1)


# ----- For the moment I'm choosing to restrict the data included in the
# ----- app to the indicators of interest (rather than restricting within
# ----- the app.


dat <- semi_join(dat, indicators, by="indic")



# ----- we can now create several tables


maindata <- filter(dat, dimension != "National average")
nationaldata <- filter(dat, dimension == "National average")
countrynames <- distinct(maindata, country, iso3, whoreg6, whoreg6_name, wbincome) %>% 
  select(country, iso3, whoreg6, whoreg6_name, wbincome)
years <- select(maindata, country, year, source) %>%  distinct
indicators <- select(indicators, -show)
strata <- filter(maindata, !grepl("Not available", flag)) %>% 
  select(country,iso3, year, source, indic, indic_name, dimension) %>% 
  distinct


saveRDS(maindata, paste0(outpath, "maindata.RDS"))
saveRDS(nationaldata, paste0(outpath, "nationaldata.RDS"))
saveRDS(years, paste0(outpath, "years.RDS"))
saveRDS(indicators, paste0(outpath, "indicators.RDS"))
saveRDS(strata, paste0(outpath, "strata.RDS"))

#xx<-readRDS(paste0(outpath, "nationaldata.RDS"))
# ----- For comparing to the original data I received to make sure that
# ----- the inequal calculations are done correctly

# # if you want to compare to previous skip part above
# maindata<-readRDS("X:/projects/who_heat/web/who-heat/WHO_HETK/data/maindata.RDS")
# maindata<-semi_join(maindata, indicators, by="indic")
# maindata <- rename(maindata, order = rankorder)
# nationaldata<-readRDS("X:/projects/who_heat/web/who-heat/WHO_HETK/data/nationaldata.RDS")
# nationaldata <- rename(nationaldata, wbincome = wbincome2014_4cat)
# strata <- filter(maindata, !grepl("Not available", flag)) %>% 
#   select(country, year, source, indic, indic_name, dimension) %>% 
#   distinct


# ----- do some renaming and limiting

nationaldata <- rename(nationaldata,
                       r_national=r,
                       r_national_lower = r_lower,
                       r_national_upper = r_upper,
                       se_national  = se,
                       flag_national = flag)

nationaldata <- nationaldata[, c("country", "year", "source", "indic", "indic_name", 
                 "r_national", "r_national_lower", "r_national_upper",
                 "se_national", "flag_national")]


# ----- we may want to use this dataset as "maindata" and skip nationaldata
# ----- but I think it might be useful to have both. In any case, this is the
# ----- data we will use to do the inequality calculations

forInequal <- left_join(maindata, 
                        nationaldata,
                        by=c("country", "year", "source", "indic", "indic_name"))






#******************************************************************************
# Examples of running
#******************************************************************************  
source("create_database/calcInequal.R", local=TRUE)

inequal.types <- c('aci', 'bgv', 'idis', 'riikm', 'mdb', 'mdm', 'mld', 
                   'paf', 'PAR', 'rci', 'rd', 'rii', 'rr', 'sii', 'ti')

# run the whole thing  
vals<-multiInequalMeasures(inequal.types, strata=strata, disagdata=forInequal, bs=TRUE,
                           outpath = NULL, n=100)

inequal<-do.call("rbind", vals)
#theFinalFiles<-list.files(outpath, full.names=TRUE)
#inequal<-do.call("rbind", lapply(theFinalFiles, read.csv, stringsAsFactors=FALSE))
saveRDS(inequal, paste0(outpath, "inequal.RDS"))














