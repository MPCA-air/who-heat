######### Kunst Mackenbach (Relative) Index

wrap.riikm <- function(y, w){
  # Relative Index of Inequality wrapper  
 p <- w/sum(w)
#   x <- midPointProp(w)
#   sii_ <- wrap.sii(y, w)
#   denominator <- (sum(p * y)) - (sii_ * (sum(p*x) -1))
  #inequal.riikm <- (sii_/denominator)
  
  #X <- round(cumsum(p) - 0.5*p,2)
  
  #siiNumerator<-round(sum(p*X*y)-(sum(p*X)*sum(p*y)),2)
  #siiDenom <- round(sum(p*X^2)-(sum(p*X)^2),2)
  
  #sii<-round(siiNumerator/siiDenom, 2)
 
  X <- midPointProp(w)
  sii_ <- wrap.sii(y, p)

  b0<-sum(p*y) - sii_*sum(p*X)
  inequal.riikm<-abs((b0+sii_)/b0)
  
  
  return(inequal.riikm)
}


riikm <- function(dat, bs=FALSE){
 # print(data.frame(dat))
  y<-dat$r
  w<-dat$pop
  se<-dat$se
  national_est <-unique(dat$r_national)
  maxopt <- unique(dat$maxoptimum)
  rankorder <- dat$order
  # This function returns the Kunst Mackenbach index of inequality:
  # Usage
  # y -- a vector of numbers
  # w -- the population size.  The default is unit weights, which is suitable for quintiles
  # se -- the vector of standard errors for each estimated y
  # rankorder -- for sii, the subgroups must be oderable (e.g., quintiles of wealth)
  # maxopt -- if higher indicators are better, maxopt is 1, if lower is better, 0
  # returns: riikm and its standard erros.
  #
  
  if(any(is.na(y))) return()
  if(!is.rank(rankorder)) return()  # If these are not rankordered, then RII does not apply
  if(is.na(maxopt)) return()
  if(any(is.na(w))) w <- -1
  if(any(is.na(se)))se <- -1
  

  
  if(!is.numeric(y) | !is.numeric(w) | !is.numeric(se)) stop('This function operates on vector of numbers')

  
  if(length(w)==1 & w[1]==-1) w <- rep(1, length(y))  # i.e., if no population numbers are given assume each group has a weight of 1

  
  if(length(se)==1 & se[1]==-1) se <- rep(0, length(y))

  
  if( !(length(y) == length(w)) | !(length(y)==length(se))) 
    stop('the rates, population-size, and standard errors must all be of the same length')

  
  if(all(w==0)) w <- rep(1, length(w))  # The calculation fails in midproppoint if their are no population numbers / this is a judgement call
    
  
  if(maxopt == 0){  # If decreasing indicator is better (e.g., U5MR)
    decrease <- T
  }
  if(maxopt == 1){  # If increasing indicator is better (e.g., Antenatal visits)
    decrease <- F
  }
  
  # Make sure the vectors are ordered from appropriately, given maxopt
  y <- y[order(rankorder, decreasing=decrease)]
  w <- w[order(rankorder, decreasing=decrease)]
  se <- se[order(rankorder, decreasing=decrease)]
  rankorder <- rankorder[order(rankorder, decreasing=decrease)]
  
  
  inequal.riikm <- wrap.riikm(y, w)
  
  
  ##############  Disable the Bootstrap SE's for RIIKM  
  #   # Bootstrap SE
  #   if(bs==T){
  #     riikm.boot <- c()  # Start with an empty vector of estimated RIIKMs
  #     for(i in 1:2000){  # Run 200 bootstraps 
  #       ny <- rnorm(length(y), y, se)  # create a new set of estimates (y) varying as rnorm(mean=y, sd=se)
  #       riikm.boot <- c(riikm.boot, wrap.riikm(ny, w))  # calculate the RIIKM on the new data
  #     } 
  #     se.boot <- sd(riikm.boot)  # Estimate the standard error of RII as the SD of all the bootstrap RIIs 
  #   }
  #   else{
  #     se.boot <- NA
  #   }
  se.boot <- NA
  
  # Formula-based SE: provided by Sam Harper
  p <- (w/sum(w))
  x <- midPointProp(w)
  sii_ <- wrap.sii(y, w)
  
  px2 <- p * x^2
  px <- p * x
  sumX <- sum(px)
  weighted_mu <- sum( p * y)
  pxy <- p * x * y
  
  ## The variables AT, AS, AR, and AU are column references to the excel spreadsheet containing the formula
  AT <-  (weighted_mu - (sii_ * sumX) +  sii_)^2  * (sum(px2) - sum(px)^2)
  AS <- p *(x - sum(px))*(weighted_mu)
  AR <- (p*sii_)/(((weighted_mu-(sii_* sumX))+sii_))^2
  AU <- ((AR-(AS/AT))^2)*(se^2)
  
  se.formula <- sqrt(sum(AU))
  
  return(list(inequal.riikm=inequal.riikm, se.riikm.boot=se.boot,  se.riikm.formula=se.formula))  # return a list of the inequality measure and the standard error 
  
}



# y <- c(6.8, 7.3, 12.9, 25.8, 56.4)
# p<-c(0.21, 0.22, 0.20, 0.19, 0.18)
# 
# X <- round(cumsum(p) - 0.5*p,2)
# 
# siiNumerator<-round(sum(p*X*y)-(sum(p*X)*sum(p*y)),2)
# siiDenom <- round(sum(p*X^2)-(sum(p*X)^2),2)
# 
# sii<-round(siiNumerator/siiDenom, 2)
# 
# 
# b0<-sum(p*y) - sii*sum(p*X)
# abs((b0+sii)/b0)
# 
