- ------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: fn_shapeRawData.R
#- DESCRIPTION: format the raw data and rename columns
#- AUTHOR: lg
#- DATE: 2021-01-12
#- ------------------------------------------

shapeRawData <- function(x = NULL){
  if ( is.null(x) ) {
    return(0)
  }
  
  shaped <- x %>% select(event=EVENT,
                         rdate=DATE,
                         sex=GENDER,
                         name=NAME,
                         loc=CITY_STATE,
                         dist=DISTANCE,
                         venue=VENUE,
                         cat=CAT,
                         ag=AG,
                         age=AGE,
                         swim=SWIM,
                         t1=T1,
                         bike=BIKE,
                         t2=T2,
                         run=RUN,
                         overall=TIME) %>%
    mutate(rdate = mdy(rdate)) %>% 
    gather(sport, stime, swim:overall)

  return(shaped)
} 