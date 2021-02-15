#-------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: fn_plural.R
#- DESCRIPTION: make a word plural
#- AUTHOR: lg
#- DATE: 2021-01-12
#- ------------------------------------------

plural <- function(inString=NULL){
  if ( is.null(inString) ) {
    return(NA)
  }
  if ( ! is.character(inString) ) {
    return(NA)
  }
  #- In case a vector was passed we just want the first element
  inString <- inString[1]
  
  #- we are going to assume that the word being passed in is singular
  if ( grepl('s$', inString, perl = TRUE) )
    return(inString)
  
  if ( grepl('y$', inString, perl=TRUE))
    return(sub('y$', 'ies', inString, perl=TRUE))
  
  if ( grepl('h$|x$|z$', inString, perl=TRUE))
    return(paste0(inString, 'es'))
  
  return (paste0(inString, 's'))
}
