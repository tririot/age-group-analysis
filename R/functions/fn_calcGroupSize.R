  #-------------------------------------------
  #- PARENT PACKAGE: Race Analysis Project
  #- MODULE NAME: fn_calcGroupSize.R
  #- DESCRIPTION: add a column to the races dataframe for rcat size
  #- AUTHOR: lg
  #- DATE: 2021-02-11
  #- ------------------------------------------

calcGroupSize <- function(x){

    #- x = races dataframe
  y <- x %>% 
    group_by(event, rcat, sport) %>%
    summarize (g.count = n(),
               .groups = "keep") %>%
    select(event, rcat, sport, g.count) %>%
    ungroup()

  x <- inner_join(x, y, by=c("event", "rcat", "sport"))
  
  return(x)
} 
