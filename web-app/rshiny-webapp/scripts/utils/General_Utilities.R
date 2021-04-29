##GENERAL UTILITIES
#_______________________________________________________________________
#sha512------
#transforms a string in a 512 bytes message digest
#Usefull to hide passwords in DB
#input variables:
#     pass - string with user pass
# returns:
#     message digest
#_______________________________________________________________________


library(shinyjs)

## Modify the CSS style of a given selector
modifyStyle <- function(selector, ...) {
  
  values <- as.list(substitute(list(...)))[-1L]
  parameters <- names(values)
  
  args <- Map(function(p, v) paste0("'", p,"': '", v,"'"), parameters, values)
  jsc <- paste0("$('",selector,"').css({", paste(args, collapse = ", "),"});")
  
  shinyjs::runjs(code = jsc)
  
}



sha512<-function(pass){
  return(digest(pass,"sha512"))
}

## This function, borrowed from http://www.r-bloggers.com/safe-loading-of-rdata-files/, load the Rdata into a new environment to avoid side effects
LoadToEnvironment <- function(RData, env=new.env()) {
  load(RData, env)
  return(env)
}
#_______________________________________________________________________________
#concate_n_strings----
#This function should not exist because paste0 can replace it, but is being used so....
#the it concats n strings in an array: example-> c('Hi','There','!')
#____________________________________________________________________________
concate_n_strings <- function(str_list) {
  return(paste(str_list, sep = '', collapse = ''))
}