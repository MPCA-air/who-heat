
#******************************************************************************
# I completely changed the approach to computing the inequality measures
# since it was taking so long
#******************************************************************************  


# 1) oneInequalMeasure is a function that takes a single combination
#    of country, year, source, indic, indic_name, dimension, pulls out
#    the disaggregated data and computes the specified inequality measure

# 2) the returnInequalDF function just applies the oneInequalMeasure to
#    all strata

# 3) Then I have the 


#outpath <- "X:/projects/who_heat/data/generated_data/"

#******************************************************************************
# Load inequality functions and list functions
#******************************************************************************  

thefiles<-list.files("./WHO_HETK/utils/inequal_functions", full.names=TRUE)
for(i in thefiles) source(i)
source("./WHO_HETK/global.R", local=TRUE)



# # run just one strata one measure
# oneInequalMeasure(strata[1,], forInequal, "mdb", bs=TRUE, count=1)
# 
# 
# # choose strata
# 
# onestrata <-filter(strata, country=="Albania", indic=="anc1", year=="2005", dimension=="Economic status")
# onestrata <-filter(strata, country=="Albania", indic=="overwgt5", year=="2005", dimension=="Economic status")
# oneInequalMeasure(onestrata, forInequal, "paf", bs=TRUE, count=1)
# 
# #tmpdata<-filter(maindata, country=="Albania", indic=="anc1", year=="2005", dimension=="Economic status")
# #tmpdata$r<-c(6.8, 7.3, 12.9, 25.8, 56.4)
# #tmpdata$pop <- 1000*c(0.21, 0.22, 0.20, 0.19, 0.18)
# oneInequalMeasure(onestrata, tmpdata, "riikm", bs=TRUE, count=1)



#******************************************************************************
# Functions for inequal database
#******************************************************************************  



multiInequalMeasures <- function(inequal_types, strata, disagdata,  bs, outpath, n=NULL){
  res<-lapply(inequal_types, 
         function(x) returnInequalDF(strata=strata, disagdata=disagdata, 
                                     inequal_type=x, bs=TRUE, outpath=outpath, n=n))
  
  return(res)
}


#returnInequalDF(strata=strata, disagdata=forInequal, inequal_type="riikm", bs=TRUE)
returnInequalDF <- function(strata, disagdata, inequal_type, bs, outpath, n){
  print(paste(Sys.time(), inequal_type))
  
  if(is.null(n) || n=="All") n <- nrow(strata)
  
  dat<-lapply(1:n,  function(x) oneInequalMeasure(strata[x,], 
                                                                  disagdata = disagdata, 
                                                                  inequal_type=inequal_type, 
                                                           bs=TRUE, count=x))
  dat<-do.call("rbind", dat)
  if(!is.null(outpath)) write.csv(dat, paste0(outpath, inequal_type, ".csv"))
  return(dat)
}


# this function focuses on ONE inequality measure and goes through
# all the strata to get results as a list

oneInequalMeasure<-function(strata1, disagdata, inequal_type, bs, count){
  
  if(count%%1000==0) print(count)
  #print(strata1)
  #print(dat)
  #disagdata <- forInequal
  #strata1<-strata[1,]
  #inequal_type <-"aci"
  #bs<-TRUE
  
  #print(strata1)
  disag1strata<-semi_join(disagdata, strata1, by=c("country","iso3", "year", "source", "indic", "indic_name", "dimension"))
  #print(disag1strata)
  
  vals<-do.call(inequal_type, list(dat=disag1strata, bs=bs))
  
  
  uniquestrata <- unique(disag1strata[,c("country","iso3", "year", "source", "indic","indic_name", "dimension")])
  
  whereBS <- grep("boot", names(vals))
  boot.se<-ifelse(length(whereBS)>0, vals[[whereBS]], NA)
  
  whereAnalytic <- grep("formula", names(vals))
  se<-ifelse(length(whereAnalytic)>0, vals[[whereAnalytic]], NA)
  
  whereInequal <- grep("inequal", names(vals))
  inequal<-ifelse(length(whereInequal)>0, vals[[whereInequal]], NA)
  
  
  return(data.frame(country=uniquestrata$country,
                    ccode = uniquestrata$iso3,
                    year = uniquestrata$year,
                    indic = uniquestrata$indic,
                    indic_name = uniquestrata$indic_name,
                    dimension = uniquestrata$dimension,
                    source = uniquestrata$source,
                    measure = inequal_type,
                    inequal = inequal,
                    boot.se = boot.se,
                    se = se,
                    stringsAsFactors = FALSE
  ))
}



