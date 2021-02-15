library(data.table)
library(tidyverse)
library(lubridate)

setwd('/Users/gould/LX/src/rap2')

#- Read data from the main RAP database
db <- read.table(file="clean_data/temp.db", sep="|",
                 header=T,
                 #row.names=1,
                 na.strings="0",
                 quote="")
db$DATE <- mdy(db$DATE)

#- make db tidy
db.tidy <- db %>% 
  select(DATE, GENDER, PLACE, AGE, NAME, SWIM, T1, BIKE, T2, RUN, TIME, VENUE, DISTANCE, EVENT, AG) %>%
  gather(sport, sport.time, SWIM:TIME)

inputAthleteName = 'LOWELL GOULD'
sportList = c('SWIM', 'T1', 'BIKE', 'T2', 'RUN', 'TIME')
distanceList <- levels(db$DISTANCE)
athleteGender <- unique(db[db$NAME == inputAthleteName, 'GENDER'])

df.pig <- db.tidy %>%
  filter(VENUE == '3PIGS', AG == 'M5054') %>%
  select(PLACE, AGE, NAME, sport, sport.time) %>%
  spread(key = sport, sport.time) %>%
  mutate(time.min = TIME/60) %>%
  arrange(SWIM) 
df.pig
