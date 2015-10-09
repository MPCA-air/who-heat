
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
  

  inequal.types <- c('aci', 'bgv', 'idis', 'riikm', 'mdb', 'mdm', 'mld', 
                     'paf', 'PAR', 'rci', 'rd', 'rii', 'rr', 'sii', 'ti')
  

  
vals<-multiInequalMeasures(c("mdm", "ti"), strata=strata, disagdata=forInequal, bs=TRUE)


multiInequalMeasures <- function(inequal_types, strata, disagdata,  bs){
  res<-lapply(inequal_types, 
         function(x) returnInequalDF(strata=strata, disagdata=disagdata, inequal_type=x, bs=TRUE))
  
  return(res)
}


#returnInequalDF(strata=strata, disagdata=forInequal, inequal_type="riikm", bs=TRUE)
returnInequalDF <- function(strata, disagdata, inequal_type, bs){
  print(inequal_type)
  
  dat<-lapply(1:10,  function(x) oneInequalMeasure(strata[x,], 
                                                                  disagdata = disagdata, 
                                                                  inequal_type=inequal_type, 
                                                                  bs=TRUE, count=x))
  return(do.call("rbind", dat))
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
  disag1strata<-semi_join(disagdata, strata1, by=c("country", "year", "source", "indic", "indic_name", "dimension"))
  #print(disag1strata)
  
  vals<-do.call(inequal_type, list(dat=disag1strata, bs=bs))
  
  
  uniquestrata <- unique(disag1strata[,c("country", "year", "source", "indic", "dimension")])
  
  whereBS <- grep("boot", names(vals))
  boot.se<-ifelse(length(whereBS)>0, vals[[whereBS]], NA)
  
  whereAnalytic <- grep("formula", names(vals))
  se<-ifelse(length(whereAnalytic)>0, vals[[whereAnalytic]], NA)
  
  whereInequal <- grep("inequal", names(vals))
  inequal<-ifelse(length(whereInequal)>0, vals[[whereInequal]], NA)
  
  
  return(data.frame(country=uniquestrata$country,
                    year = uniquestrata$year,
                    indic = uniquestrata$indic,
                    dimension = uniquestrata$dimension,
                    source = uniquestrata$source,
                    measure = inequal_type,
                    inequal = inequal,
                    boot.se = boot.se,
                    se = se,
                    ccode = NA,
                    stringsAsFactors = FALSE
  ))
}


  
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

