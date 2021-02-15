  #-------------------------------------------
  #- PARENT PACKAGE: Race Analysis Project
  #- MODULE NAME: fn_cleanGenders.R
  #- DESCRIPTION: Here we clean the gender column so that the only
  #-              levels are M and F
  #- AUTHOR: lg
  #- DATE: 2021-01-12
  #- ------------------------------------------

cleanGenders <- function(x){
  
  #- make the gender column a factor
   x$sex <- as.factor(x$sex)
  
   #- convert all genders to either M or F
  levels(x$sex)[levels(x$sex) %in% c('M','MEN','MALE')] <-"M"
  levels(x$sex)[levels(x$sex) %in% c('F','WOMEN','FEMALE')] <-"F"

  #- deduce null genders based on race category
  x[which(x$sex == '' & grepl("Male|M\\d\\d|^MO$|Clyde", x$rcat, perl=TRUE)),'sex'] <- "M"
  x[which(x$sex == '' & grepl("Female|F\\d\\d|^FO$|Athena", x$rcat, perl=TRUE)),'sex'] <- "F"
  
  #- get rid of all the records with unkown gender
  x <- x %>%
    filter(sex %in% c('M', 'F') )
  
  x$sex <- droplevels(x$sex)

  return(x)
} 
