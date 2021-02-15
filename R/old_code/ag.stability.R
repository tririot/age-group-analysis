#-
library(tidyverse)

#- Read data from the main RAP database
db <- read.table(file="clean_data/rap.db", sep="|",
                 header=T,
                 row.names=1,
                 na.strings="0",
                 quote="")
db$DATE <- mdy(db$DATE)

#- EVENT == "WILYMCA20120922SPRINT" 
#- reduce data to look at only one event (for simplicity)
main.df <- db %>%
  filter(DISTANCE == "SPRINT" &
           VENUE == "AZALEA" &
           GENDER == "MEN" &
           AGE <=69 &
           AGE >= 20 &
           ! is.na(AG))

event.means <- main.df %>%
  group_by(EVENT) %>%
  summarize(event.mn = mean(TIME, na.rm=T),
            event.sd = sd(TIME, na.rm=T),
            event.n = n() )

ag.means <- main.df %>%
  group_by(EVENT, AG) %>%
  summarize(ag.mn = mean(TIME, na.rm=T),
            ag.sd = sd(TIME, na.rm=T),
            ag.n = n() )

grouped.means <- inner_join(ag.means, event.means, by=c('EVENT') ) %>%
  mutate(ag.dev = ag.mn - event.mn)

ggplot(grouped.means) +
  geom_point(aes(AG, ag.dev, size=ag.n)) +
  geom_hline(aes(AG, ag.dev), yintercept = 0) +
  facet_grid(EVENT ~ .) +
  ggtitle("Relationship of Age Groups to Event Mean")


fit <- aov(ag.dev ~ AG)
#- 