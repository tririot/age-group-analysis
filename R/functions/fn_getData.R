  #-------------------------------------------
  #- PARENT PACKAGE: Race Analysis Project
  #- MODULE NAME: fn_shapeRawData.R
  #- DESCRIPTION: format the raw data and rename columns
  #- AUTHOR: lg
  #- DATE: 2021-01-12
  #- ------------------------------------------

getData <- function(filePath = '../data/raw.Rdata'){

  #- the dataframe is called 'races'
  load(file=filePath)
  
  shaped <- races %>% select(event=EVENT,
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
    mutate(sbr = swim + bike + run,
           rdate = mdy(rdate),
           ryear = year(rdate)) %>% 
    gather(sport, stime, swim:sbr)
  

  return(shaped)
} 

#load(file="../data/raw.Rdata")
# races <- read.table(file = "data/new.db", sep='|', header=T,
#                 comment.char = "",
#                 nrows = 95000,
#                 check.names = TRUE,
#                 fill = TRUE,
#                 strip.white = TRUE,
#                 quote=""
# )
# 
# save(races, file="data/raw.Rdata")
