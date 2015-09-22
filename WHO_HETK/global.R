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

lappend <- function (lst, ...){
  # Append an item to a list; e.g.:
  # vara <- list("axislimit"=T)
  # vara <- lappend(vara, "print"=F)
  lst <- c(lst, list(...))
  return(lst)
}
