  #-------------------------------------------
  #- PARENT PACKAGE: Race Analysis Project
  #- MODULE NAME: fn_cleanTimes.R
  #- DESCRIPTION: correct time values that are not correct and 
  #-              calculate the sbr time
  #- AUTHOR: lg
  #- DATE: 2021-01-16
  #- ------------------------------------------

cleanTimes <- function(x){
  #- times in the incoming data frame are in seconds. 
  #- overall time must be between 30 minutes and 17 hours
  minTime <- 30 * 60
  maxTime <- 17 * 60 * 60
  x <- x %>%
    mutate(stime = ifelse(sport == 'overall' & !between(stime, minTime, maxTime), 0, stime)) 
  
  return(x)
} 
