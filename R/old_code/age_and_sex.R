library(tidyverse)

#- Read data from the main RAP database
db <- read.table(file="clean_data/rap.db", sep="|",
                 header=T,
                 row.names=1,
                 na.strings="0",
                 quote="")
db$DATE <- mdy(db$DATE)

#- reduce data to look at only one event (for simplicity)
a.s <- db[db$EVENT == "WILYMCA20120922SPRINT",]
n <- nrow(a.s)

ggplot(a.s) +
  geom_point(aes(AGE, TIME, color=GENDER)) +
  geom_smooth(method='lm', se=TRUE,
              aes(AGE, TIME, color = GENDER)) +
  ggtitle("Relationship of Age and Sex to Race Time", 
          subtitle = paste(n, ' total athletes'))
mod.as <- lm(TIME ~ GENDER + AGE, data=a.s)

#- 