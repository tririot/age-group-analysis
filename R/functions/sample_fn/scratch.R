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