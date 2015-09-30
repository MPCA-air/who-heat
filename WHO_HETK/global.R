focusIndicator_selector <- function(id, multiple = FALSE, core=FALSE){
  
  #if(is.null(.rdata[['focus_indicator']])) return()
  #if(is.null(.rdata[['core_indicators']])) return()
  # TODO: need to unselect non-core if selected in diaggregated
  # and in summary.
  
  if(core)  indicators <- .rdata[['core_indicators']]
  if(!core) indicators <- .rdata[['full_indicators']]
  #countries <- getFilteredCountries()
  #if(is.null(indicators)){ indicators <- c()}
  
  
  ifelse(multiple, focus_indic <- .rdata[['focus_indicator']], focus_indic <- .rdata[['focus_indicator']][1])
  
  #print(.rdata[['focus_indicator']])
  #print(indicators)
  
  selectInput(id, 
              h5("Select health indicators"), 
              choices  = .rdata[['core_indicators']], 
              multiple = multiple, 
              selected = .rdata[['focus_indicator']])
  
  
}



focusInequalType_selector <- function(id, dimension){
  
  
    dimension <- .rdata[['focus_dimension']]
    if(length(dimension)>0){
      if(dimension %in% .rdata[['rankable_dimensions']]){
        selectionOptions <- .rdata[['summary_measures_all']]
      }
      if(!dimension %in% .rdata[['rankable_dimensions']]){
        selectionOptions <- .rdata[['summary_measures_unrank']]
      }
    }
    else{
      selectionOptions <- NULL
    }
  
  selectInput(id, 
              h5("Select summary measure"), 
              choices=.rdata[['summary_measures_all']], 
              selected=c("Range difference" = "rd"), 
              multiple=FALSE)
  
  
  
}


focusDimension_selector <- function(id, multiple = FALSE){
  
  # TODO: need to unselect non-core if selected in diaggregated
  # and in summary.
  

  ifelse(multiple, focus_dimen <- .rdata[['focus_dimension']], focus_dimen <- .rdata[['focus_dimension']][1])
  
  selectInput(inputId = id,
              h5("Select inequality dimensions"),
              choices = .rdata[['equity_dimensions']],
              selected = focus_dimen,
              multiple=multiple)
  
  
}







textInputRow <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: inline-block;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value, class="input-custom"))
}


textPassword <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: inline-block;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "password", value = value, class="input-custom"))
}


myHiddenBoolean <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: none;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "checkbox", value = value, class="input-boolean"))
}

myHiddenText <-function (inputId, label, value = ""){
  # A function to handle the creation of side-by-side numeric input boxes
  div(style="display: none;",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value, class="input-text"))
}


####################
# copied from single files
####################


# *****************************
lappend <- function (lst, ...){
  # Append an item to a list; e.g.:
  # vara <- list("axislimit"=T)
  # vara <- lappend(vara, "print"=F)
  lst <- c(lst, list(...))
  return(lst)
}

# *****************************
is.rank <- function(x){
  ranked <- T
  if(any(x < 1)){
    # The data are not an ordered subgroup
    ranked <- F
  }
  if(all(!x==1)){
    # The data are ordered by subgroup, but the base subgroup is missing
    ranked <- F
  }
  return(ranked)
}

# *****************************
notin <- function (vector1, vector2){
  # Return the elements in vector1 that are not in vector2
  el <- which(!(vector1 %in% vector2))  # svae in 'el' the element-locations on v1 not in v2
  if(length(el)==0){  # If there aren't any, return NULL
    return(NULL)
  }
  else{  # Else return the elements
    return (vector1[el])
  }
} 



# *****************************
sqlIn <- function(a_vector){
  # Take a character or numeric vector and return it in a form suitable for using after the 'IN'
  # subcommand in a SQLite SELECT statement.
  # eg: SELECT a FROM table WHERE c IN ("xx", "yy", "zz");
  
  if(is.numeric(a_vector)){
    num.str <- gsub(", ", ", ", toString(a_vector))
    returnStr <- paste0(' (', num.str, ') ')
  }
  else if(is.character(a_vector)){
    returnStr <- ''
    vlength <- length(a_vector)
    for(i in 1:vlength){
      returnStr <- paste0(returnStr, '"', a_vector[i], '"')
      if(i != vlength){
        returnStr <- paste0(returnStr, ', ')
      }
      else{
        returnStr <- paste0(' (', returnStr, ') ')
      } 
    }
  }
  else{
    returnStr <- NULL
  }
  return(returnStr)
}



my.which.min <- function(lizt, na.rm=F){
  # Which elements of lizt are minimum values
  # The problem with the original which.min() is that it only returns the position
  # of the first occuring minimum value.  so in c(1,2,3,1), which.min returns 1, 
  # my.which.min returns 1 and 4
  # na.rm: remove missing values.  If they exist and na.rm=F, return(NA)
  
  if(na.rm==F & sum(is.na(lizt))!=0){
    return(NA)
  }
  min.lizt <- min(lizt, na.rm=T)
  which(min.lizt==lizt)
}


nearest <- function(anchor, lizt, limit=1, all=F){
  # anchor: the number against which nearness is tested
  # lizt: the vector of candidate numbers
  # limit: the limit on how far away the number can be
  #print('----nearest()-------')
  if(!is.na(anchor) & !is.null(lizt)){
    
    anchor <- as.integer(anchor)
    diff <- abs(anchor - lizt)  # The absolute difference between the anchor and the lizt
    if(min(diff, na.rm=T) > limit){  # If no number in lizt is nearer to anchor than limit ... fail (FALSE)
      return (FALSE)
    }
    else{
      locasi <- my.which.min(diff, na.rm=T)
      if(length(locasi)>1 & all==T){  # If there is more than one candidate and 'all'==T ...
        return(locasi)  # Return all
      }
      else{  # Otherwise ...
        return(locasi[1])  # Return the first
      }
    }

    return(NULL) 
  } else {
    return(NULL)
  }
}

resetAll <- function(){
  
} 


findCommon <- function(vectora, vectorb){
  # Find the health indicators that are common to every year.
  # vectora: the list of heath indicators
  # vectorb: the years in which the indicators occur
  if(length(vectora)!=length(vectorb)){
    stop("Vectors must be of the same length")
  }
  names(which(rowSums(table(vectora, vectorb))==length(unique(vectorb)))) 
}






geoOnly <- function(DF){
  # A function which determines whether geography is on eof the included dimensions of interest
  # returns T/F
  
  the_dimensions <- unique(DF$dimension)
  
  return("Geographic region" %in% the_dimensions & 
           sum(c("Economic status", "Mother's education", "Place of residence", "Sex") %in% the_dimensions)==0)
}


