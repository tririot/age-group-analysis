#-------------------------------------------
#- PARENT PACKAGE: Race Analysis Project
#- MODULE NAME: fn_sumData.R
#- DESCRIPTION: summarize data
#- AUTHOR: lg
#- DATE: 2021-01-12
#- ------------------------------------------
races <- getData()
racesOverall <- races %>% 
  filter(sport == 'overall',
         stime != 0)

getSummaryTable <- function(d, c, l=NULL) {
  #- d = dataframe
  #- c = column name
  #- l = column label
  
  if ( is.null(l) )
    l = c

  s <- d %>%
    group_by_at(c) %>%
    summarize(n = n()) %>%
    arrange_at(desc(c))
  maxGroup <- s[1,2]
  minGroup <- tail(s[,2], n=1)
  
  summaryTable <- data.frame(stat = c(
    sprintf ("%d",sum(s$n)),
    sprintf ("%d",nrow(s)),
    sprintf ("%s (%d)",maxGroup, max(s$n)),
    sprintf ("%s (%d)",minGroup, min(s$n)),
    sprintf ("%7.1f",mean(s$n)),
  ))
  row.names(summaryTable) <- c(paste0('Total Records Over All ',plural(l)), 
                               paste0('Number of ', plural(l),
                               paste0(l, ' with most records'),
                               paste0(l, ' with least records'),
                               paste0('Average # of records per ', l)
  )
  return(summaryTable)
}

#- =============== number of unique events and observations per

ggplot (s, aes(n)) +
  geom_histogram()


#- number of years and events per

#- counts per sex
#- number of unique athletes and histogram of counts
#- number of unique locations(one athlete per) and counts
#- number of venues and obs per
#- number of categories (0= age group)
#- number of age groups and observations per
#- age distributions