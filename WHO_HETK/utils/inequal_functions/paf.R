######### Population Attributable Fraction (PAF)
# Population Attributable Risk is a measure of absolute inequality, and is based on the
# premise that inequality could be eliminated by improving the level of a health indicator
# in a population to match the best-performing subgroup. Simply put, population
# attributable risk shows the improvement possible if all subgroups had the same
# rate as a reference subgroup.
# Handbook on Healt Inequality Monitoring (2013)
#########

wrap.paf <- function(x, w, maxopt=F, rankorder, national_est=NULL){
  # The population attributable fraction
  pop.prop <- w/sum(w) 
  if(is.null(national_est)){  # Us the weighted mean of the data if there is no national estimate
    
    w.mean <- weighted.mean(x, pop.prop)
  } else {
    #print(paste0("paf: ", national_est))
    w.mean <- national_est
  }
  
  inequal.par <- wrap.par(x, w, maxopt, rankorder, national_est)
  
  inequal.paf <- (inequal.par/w.mean) * 100
  return(inequal.paf)
}


paf <- function(dat, bs=FALSE){
  x<-dat$r
  w<-dat$pop
  se<-dat$se
  national_est <-unique(dat$r_national)
  maxopt <- unique(dat$maxoptimum)
  rankorder <- dat$order
  # This function returns the percentage of the PAR over the population rate
  # Usage
  # x -- a vector of numbers
  # w -- population weights
  # se -- standard error
  # bs -- whether to use the bootstrap to calculate confidence intervals
  # maxopt -- the highest vakue is the optimum value
  # rankorder -- the rank order of the subgroups (0 mean no rank order)
  #
  # returns the percentage of the PAR over the population rate
  # 
  
  #print(paste0("x is: ", paste(x, collapse=",")))
  #print(paste0("w is: ", paste(w, collapse=",")))
  #print(paste0("se is: ", paste(se, collapse=",")))
  #print(paste0("x is: ", paste(x, collapse=",")))
  
  if(is.na(maxopt)) return(NULL)
  if(all(rankorder>0) & all(!rankorder==1))return(NULL)# The data are ordered by subgroup, but the base subgroup is missing

  
  if(any(is.na(w))) w <- -1
  
  if(any(is.na(se))) se <- -1
  
  if(!is.numeric(x) | !is.numeric(w) | !is.numeric(se)) stop('This function operates on vector of numbers')
  
  if(length(w)==1 & w[1]==-1) w <- rep(1000, length(x))
  
  if(length(se)==1 && se==-1)se <- rep(0, length(x)) # i.e., if there are no standard errors provided, make the se's=0

  
  if( !(length(x) == length(w)) | !(length(x)==length(se)) ) 
    stop('the rates, population-size, and standard errors must all be of the same length')

  
  inequal.paf <- wrap.paf(x, w, maxopt, rankorder, national_est)
  
  # Bootstrap SE
  se.boot <- NA
  if(bs==T){  ## Problem with the bootstrap SE
    paf.boot <- c()  # Start with an empty vector of estimated PARs
    for(i in 1:200){  # Run 200 bootstraps 
      nx <- rnorm(length(x), x, se)  # create a new set of estimates (y) varying as rnorm(mean=x, sd=se)
      if(any(is.nan(nx)) && i==1){
        #print(data.frame(dat))
#         print(paste0("x is: ", paste(x, collapse=",")))
#         print(paste0("w is: ", paste(w, collapse=",")))
#         print(paste0("se is: ", paste(se, collapse=",")))
      }

      paf.boot <- c(paf.boot, wrap.paf(nx, w, maxopt, rankorder, national_est))  # calculate the PAR on the new data
    } 
    se.boot <- sd(paf.boot)  # Estimate the standard error of PAR as the SD of all the bootstrap PARs 
  }
  
  
  
  # this matches up with the Excel spreadsheet I was provided
  # inequality measures(Sam Harper) v1
  mu<-weighted.mean(x, w/sum(w)) #af
  
  co6<-w[which.min(x)] - (min(x)/100)*w[which.min(x)]
  cp6<-(min(x)/100)*w[which.min(x)]
  cq6 <- (mu/100)*sum(w)-cp6
  cr6 <- sum(w)-cq6-cp6-co6
  cv6 <- mu-min(x)
  cx6 <- cv6/mu
  cs6<-sqrt((cr6+cx6*(cq6+co6))/((sum(w)*cp6)))
  
  ct6<-(log(1-cx6))-qnorm(0.975)*cs6
  cu6<-(log(1-cx6))+qnorm(0.975)*cs6
  cy6<-100*abs((1-exp(ct6))-(1-exp(cu6)))/(2*(qnorm(0.975)))
  
  
  if(length(se)==length(x)){
    se.formula <- cy6
  }
  else{
    se.formula <- NA   
  }
  
  
  
  
  # Return the results as a list
  return(list(inequal.paf=inequal.paf, se.paf.boot=se.boot,  se.paf.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}




# dat<-filter(maindata, country=="Albania", indic=="anc1", year=="2005", dimension=="Economic status")
# natl<-data.frame(filter(nationaldata, country=="Albania", indic=="anc1", year=="2005"))
# 
# x<-dat$r
# w<-dat$pop
# se<-dat$se
# national_est <-unique(dat$r_national)
# maxopt <- unique(dat$maxoptimum)
# rankorder <- dat$order
# 
# g=r
# h=se
# i=pop
# 
# mu<-weighted.mean(x, w/sum(w)) #af
# 
# co6<-w[which.min(x)] - (min(x)/100)*w[which.min(x)]
# cp6<-(min(x)/100)*w[which.min(x)]
# cq6 <- (mu/100)*sum(w)-cp6
# cr6 <- sum(w)-cq6-cp6-co6
# cv6 <- mu-min(x)
# cx6 <- cv6/mu
# cs6<-sqrt((cr6+cx6*(cq6+co6))/((sum(w)*cp6)))
# 
# ct6<-(log(1-cx6))-qnorm(0.975)*cs6
# cu6<-(log(1-cx6))+qnorm(0.975)*cs6
# cy6<-100*abs((1-exp(ct6))-(1-exp(cu6)))/(2*(qnorm(0.975)))
# 
# 
# 
# 
# cp6<-(MIN(x)/100)*(OFFSET(x[1],MATCH(MIN(x),x,0)-1,2))
# cq6 <- (AF6/100)*SUM(I2:I6)-CP6
# cr6 <- SUM(I2:I6)-CQ6-CP6-CO6
# cs6<-sqrt((CR6+CX6*(CQ6+CO6))/((sum(w)*CP6)))
# cx6 <- CV6/AF6
# ct6<-(ln(1-CX6))-NORMSINV(0.975)*CS6
# cu6<-(ln(1-CX6))+NORMSINV(0.975)*CS6
# fin<-abs((1-exp(CT6))-(1-exp(CU6)))/(2*(NORMSINV(0.975)))










