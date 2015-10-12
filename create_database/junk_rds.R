


# JUNK BELOW THAT I WILL DELETE



newest<-read.csv("X:/projects/who_heat/data/generated_data/bgv.csv")




oldest<-readRDS("X:/projects/who_heat/web/who-heat/WHO_HETK/data/inequals.RDS")

function(inequal_type, olddat){
  
  newest<-read.csv("X:/projects/who_heat/data/generated_data/bgv.csv", stringsAsFactors = FALSE)
  xx<-inner_join(newest, olddat, by=c("country", "year",
                                      "indic", "dimension",  "measure"))
  
  plot(xx$inequal.x, xx$inequal.y, xlab="New", ylab="Old")
  
  dat<-xx[round(xx$inequal.x,1)!=round(xx$inequal.y,1),]
  
  dat$perdiff<-100*(dat$inequal.x-dat$inequal.y)/dat$inequal.y
  
  dat2<-dat[dat$perdiff>2,]
}








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
# maindata <- readRDS("X:/projects/who_heat/web/who-heat/WHO_HETK/data/maindata.RDS")
# strata <- filter(maindata, !grepl("Not available", flag)) %>% 
#   select(country,iso3, year, source, indic, indic_name, dimension) %>% 
#   distinct
#saveRDS(strata, "X:/projects/who_heat/web/who-heat/WHO_HETK/data/strata.RDS")

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



# 
# 
# calcInequal <- function(data_frame,  inequal.types='all'){
#   # df is a dataframe of the health indicator data with the equity dimension
#   # inequal.types is a vector of the inequality measures to be calculated and defaults to all
#   #  print('calcInequal()')  
#   tmpDF <- data_frame
#   
#   if('national' %in% names(tmpDF)){  # Later rely on national average data if available
#     national_flag <- T
#   } else {
#     national_flag <- F
#     national_est <- NULL
#   }
#   
#   if(inequal.types=='all'){
#     inequal.types <- c('aci', 'bgv', 'idis', 'riikm', 'mdb', 'mdm', 'mld', 
#                        'paf', 'par', 'rci', 'rd', 'rii', 'rr', 'sii', 'ti')
#   }
#   
#   ineqDF <- data.frame(country=character(),
#                        year=integer(), 
#                        indic=character(),
#                        dimension=character(),
#                        measure=character(),
#                        inequal=numeric(),
#                        boot.se=numeric(),
#                        se=numeric(),
#                        ccode=character(),
#                        #                       typeordered=logical(),
#                        stringsAsFactors=FALSE)
#   
#   countrylist <- unique(tmpDF$country)
#   yearlist <- unique(tmpDF$year)
#   indiclist <- unique(tmpDF$indic)
#   dimensionlist <- unique(tmpDF$dimension)
#   
#   for(i in countrylist){
#     tmp_ccode <- unique(tmpDF$ccode[tmpDF$country==i])
#     for(j in yearlist){
#       for(k in indiclist){
#         for(l in dimensionlist){
#           for(m in inequal.types){
#             #            print(m)
#             relevant.rows <- which(tmpDF$country==i & tmpDF$year==j & tmpDF$indic==k & tmpDF$dimension==l)
#             ineq.result <- NULL
#             if(length(tmpDF$estimate[relevant.rows]) > 1 & all(!is.na(tmpDF$estimate[relevant.rows]))){  
#               # If there are 1 or 0 rows, there can be no inequality.
#               # If there is missing data it is impossible to estimate the inequality
#               
#               if(c('rankorder') %in% names(tmpDF)){
#                 # If order information is provided about the ranking of equity dimension, record this
#                 rankorder <- tmpDF$rankorder[relevant.rows]
#               }
#               else{
#                 rankorder <- 0
#               }
#               
#               if(national_flag){
#                 national_est <- unique(tmpDF$national[relevant.rows])
#               }
#               
#               maxopt <- tmpDF$maxoptimum[relevant.rows][1]==1
#               
#               # If the equity dimenstion is rankable, note that T/F
#               ranked <- tmpDF$rankable[relevant.rows][1]==1
#               
#               
#               starttime <- proc.time()
#               # Calculate the ACI (Absolute Concentration Index)
#               if((m=='aci') & ranked){
#                 ineq.result <- aci(x=tmpDF$estimate[relevant.rows], 
#                                    w=tmpDF$pop[relevant.rows], 
#                                    se=tmpDF$se[relevant.rows], bs=T, rankorder=rankorder)
#               }
#               
#               
#               
#               # Calculate the BGV (Between Groups Variance)
#               if(m=='bgv'){
#                 ineq.result <- bgv(x=tmpDF$estimate[relevant.rows], 
#                                    w=tmpDF$pop[relevant.rows], 
#                                    se=tmpDF$se[relevant.rows], bs=T)        
#               }
#               
#               
#               
#               # Calculate IDis (Index of Disparity)
#               if(m=='idis'){
#                 ineq.result <- idis(x=tmpDF$estimate[relevant.rows], 
#                                     w=tmpDF$pop[relevant.rows], 
#                                     se=tmpDF$se[relevant.rows], 
#                                     bs=T, 
#                                     national_est=national_est)                
#               }
#               
#               
#               
#               # Calculate KMI (Kunst Mackenbach (Relative) Index)
#               if((m=='riikm') & ranked){
#                 ineq.result <- riikm(y=tmpDF$estimate[relevant.rows], 
#                                      w=tmpDF$pop[relevant.rows], 
#                                      se=tmpDF$se[relevant.rows], 
#                                      bs=T,
#                                      rankorder=rankorder,
#                                      maxopt=maxopt)    
#               }
#               
#               
#               
#               # Calculate mdb (Mean Difference from the Best performing Subgroup)
#               if(m=='mdb'){
#                 ineq.result <- mdb(x=tmpDF$estimate[relevant.rows], 
#                                    w=tmpDF$pop[relevant.rows], 
#                                    se=tmpDF$se[relevant.rows], bs=T, maxopt=maxopt)                
#               }
#               
#               
#               
#               # Calculate MDM (Meand Difference from the Mean)
#               if(m=='mdm'){
#                 ineq.result <- mdm(x=tmpDF$estimate[relevant.rows], 
#                                    w=tmpDF$pop[relevant.rows], 
#                                    se=tmpDF$se[relevant.rows], bs=T,
#                                    national_est=national_est)    
#               }
#               
#               
#               
#               
#               # Calculate MLD (Mean Log Deviation)
#               if(m=='mld'){
#                 ineq.result <- mld(x=tmpDF$estimate[relevant.rows], 
#                                    w=tmpDF$pop[relevant.rows], 
#                                    se=tmpDF$se[relevant.rows], bs=T,
#                                    national_est=national_est)  
#                 
#               }
#               
#               
#               # Calculate PAF (Population Attributable Fraction)
#               if(m=='paf'){
#                 ineq.result <- paf(x=(tmpDF$estimate[relevant.rows]),  # This reverses the percentages on, say ANC visits 
#                                    w=tmpDF$pop[relevant.rows], 
#                                    se=tmpDF$se[relevant.rows],
#                                    bs=T, maxopt=maxopt,
#                                    rankorder = tmpDF$rankorder[relevant.rows],
#                                    national_est = national_est)
#               }
#               
#               
#               
#               
#               # Calculate PAR (Population Attributable Risk)
#               if(m=='par'){
#                 ineq.result <- PAR(x=(tmpDF$estimate[relevant.rows]),  # This reverses the percentages on, say ANC visits 
#                                    w=tmpDF$pop[relevant.rows], 
#                                    se=tmpDF$se[relevant.rows],
#                                    bs=T, maxopt=maxopt,
#                                    rankorder = tmpDF$rankorder[relevant.rows],
#                                    national_est = national_est)
#               }
#               
#               
#               
#               
#               
#               #               # Calculate RCI (Relative Concentration Index)
#               if((m=='rci') & ranked){
#                 ineq.result <- rci(y=tmpDF$estimate[relevant.rows], 
#                                    w=tmpDF$pop[relevant.rows], 
#                                    se=tmpDF$se[relevant.rows], 
#                                    bs=T, 
#                                    rankorder=rankorder)
#                 
#               }
#               
#               
#               
#               # Calculate RD (Rank/Rate Difference) 
#               if(m=='rd'){
#                 ineq.result <- rd(x=tmpDF$estimate[relevant.rows], 
#                                   w=tmpDF$pop[relevant.rows], 
#                                   se=tmpDF$se[relevant.rows], 
#                                   bs=T,
#                                   rankorder=rankorder)               
#               }
#               
#               
#               
#               
#               
#               # Calculate RII (Relative Index of Inequality) 
#               if((m=='rii')  & ranked){
#                 ineq.result <- rii(y=tmpDF$estimate[relevant.rows], 
#                                    w=tmpDF$pop[relevant.rows], 
#                                    se=tmpDF$se[relevant.rows], 
#                                    bs=T, 
#                                    rankorder=rankorder,
#                                    maxopt=maxopt)
#               }
#               
#               
#               
#               # Calculate RR (Rank/Rate Ratio) 
#               if(m=='rr'){
#                 ineq.result <- rr(x=tmpDF$estimate[relevant.rows], 
#                                   se=tmpDF$se[relevant.rows], 
#                                   rankorder=rankorder,
#                                   maxopt=maxopt,
#                                   bs=T)
#                 
#               }
#               
#               
#               
#               # Calculate SII (Slope Index of Inequality)
#               if((m=='sii')  & ranked){  
#                 ineq.result <- sii(y=tmpDF$estimate[relevant.rows], 
#                                    w=tmpDF$pop[relevant.rows], 
#                                    se=tmpDF$se[relevant.rows], 
#                                    bs=T, 
#                                    rankorder=rankorder,
#                                    maxopt=maxopt)
#               }
#               
#               
#               # Calculate TI (Theil Index)
#               if(m=='ti'){
#                 ineq.result <- ti(y=tmpDF$estimate[relevant.rows], 
#                                   w=tmpDF$pop[relevant.rows], 
#                                   se=tmpDF$se[relevant.rows], bs=T)
#               }
#               
#               #print(paste('country=',i,' year=', j, ' indic=', k, ' dimension=', l, ' measure=', m, ' inequal=', ineq.result[[1]],
#               #            ' boot.se=', ineq.result[[2]], ' se=', ineq.result[[3]], tmp_ccode))
#               
#               # Append the last calculated measure to the end of the dataframe 
#               if(!is.null(ineq.result)){
#                 ineqDF <- rbind(ineqDF, data.frame(country=i,
#                                                    year=j, 
#                                                    indic=k,
#                                                    dimension=l,
#                                                    measure=m,
#                                                    inequal=ineq.result[[1]],
#                                                    boot.se=ineq.result[[2]],
#                                                    se=ineq.result[[3]],
#                                                    ccode=tmp_ccode,
#                                                    #                                     typeordered=logical(),
#                                                    stringsAsFactors=FALSE))
#               }
#             }
#           }          
#         }        
#       }  
#     }  
#   }
#   ineqDF <- ineqDF[!duplicated(ineqDF), ]  #*****  For some reason rows are being duplicated.  This is a lazy stop-gap
#   return(ineqDF)
# }
