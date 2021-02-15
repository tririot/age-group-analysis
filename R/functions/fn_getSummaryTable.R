#-------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: fn_getSummaryTable.R
#- DESCRIPTION: summarize data
#- AUTHOR: lg
#- DATE: 2021-01-12
#- ------------------------------------------

getSummaryTable <- function(d, c, l=NULL) {
  #- d = dataframe
  #- c = column name
  #- l = column label
  
  if ( is.null(l) )
    l = c

  s <- d %>%
    group_by_at(c) %>%
    summarize(n = n()) %>%
    arrange(n)
  minGroup <- s[1,1]
  maxGroup <- tail(s[,1], n=1)
  
  summaryTable <- data.frame(stat = c(
    sprintf ("%d",sum(s$n)),
    sprintf ("%d",nrow(s)),
    sprintf ("%s (%d)",maxGroup, max(s$n)),
    sprintf ("%s (%d)",minGroup, min(s$n)),
    sprintf ("%7.1f",mean(s$n))
  ))
  row.names(summaryTable) <- c(paste0('Total Records Over All ',plural(l)), 
                               paste0('Number of ', plural(l)),
                               paste0(l, ' with most records'),
                               paste0(l, ' with least records'),
                               paste0('Average # of records per ', l)
  )
  return(summaryTable)
}
