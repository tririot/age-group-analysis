#Athlete = 'JEFF PLEMMONs'
Athlete = 'LOWELL GOULD'
#Athlete = 'MICHAEL CAVENESS'
#Athlete = 'ERIN GREEN'
#Athlete = 'MARTY DEJOSEPH'
library(data.table)
library(tidyverse)
library(lubridate)

setwd('/Users/gould/LX/src/rap2')

#- Read data from the main RAP database
db <- read.table(file="clean_data/rap.db", sep="|",
                 header=T,
                 row.names=1,
                 na.strings="0",
                 quote="")
db$DATE <- mdy(db$DATE)

inputAthleteName = 'LOWELL GOULD'
inputDistance = 'SPRINT'

#- list the events and age groups of the target athlete
events <- db %>%
  mutate (event.ag = paste0(EVENT,'_', AG)) %>%
  filter(NAME == inputAthleteName &
           DISTANCE == inputDistance &
           year(DATE) >= 2010) %>%
  select(event.ag) 

#- Reduce the data frame to a manageable size
df <- db %>% 
  mutate(event.ag = paste0(EVENT,'_', AG)) %>%
  filter(event.ag %in% events$event.ag) %>%
  select(NAME, EVENT, RACE, AG, TIME, AGE, DISTANCE, DATE, VENUE, event.ag) %>%
  mutate(TIME = TIME,
         rtime = TIME/60,
         rvenue = paste0(year(DATE), sprintf("%03d",yday(DATE)), "_", VENUE)
  )

min.date = min(df$DATE)
#- Calculate mean and SD for race time by venue and ag
plot.df <- df %>%
  group_by(event.ag) %>%
  summarize(m.time = mean(rtime, na.rm=T),
            s.time = sd(rtime, na.rm=T),
            min.time = min(rtime),
            n = n()
            ) %>%
  inner_join(df) %>%
  mutate(centered = rtime-m.time,
         std.time = centered/s.time,
         behind = rtime - min.time,
         event.seq = (DATE-min.date) + 1) %>%
  filter(n >=20) %>%
  select (NAME, EVENT, VENUE, DATE, event.seq, n, rtime, std.time) %>%
  arrange(event.seq)

#- This dataframe can be used to print a table of the athlete's data
athleteData <- plot.df %>%
  filter(NAME == inputAthleteName) %>%
  select(EVENT, VENUE, DATE, event.seq, rtime, std.time)

#- Create a data.frame of race names for labeling the y-axis graphs
Race.Names = tapply(as.numeric(plot.df$event.seq), plot.df$EVENT, mean)
Race.Names = data.frame(rname = names(Race.Names), rday = as.vector(Race.Names))

AthletePointStyle = 15
DataPointStyle = 1

# pdf(file = paste("/Users/gould/LX/src/rap2/reports/",
#                  Athlete,
#                  ".pdf",
#                  sep=''),
#     paper='USr',
#     width=10,
#     height=7.5
# )

#- histograms
ggplot(plot.df) +
  geom_histogram(aes(rtime)) +
  facet_wrap(~DATE)

ggplot(plot.df) +
  geom_point(aes(event.seq, std.time), shape = 21, fill = "white") +
  geom_hline(aes(event.seq, std.time), yintercept = 0) +
  geom_point(data = athleteData, aes(event.seq, std.time),
             color = 'red', size = 3) +
  geom_smooth(data = athleteData, method='lm', se=FALSE,
              aes(event.seq, std.time)) +
  ggtitle("Relative Fitness Plot", subtitle = "Joe Athlete") +
  ylab("Standardized Race Time") +
  scale_x_continuous("Event", breaks=athleteData$event.seq, 
                     labels = athleteData$VENUE)

