#Athlete = 'JEFF PLEMMONs'
Athlete = 'LOWELL GOULD'
#Athlete = 'MICHAEL CAVENESS'
#Athlete = 'ERIN GREEN'
#Athlete = 'MARTY DEJOSEPH'
library(data.table)
library(lubridate)

dist="sprint"
#dist="intl"
#dist="half"
setwd('/Users/gould/LX/src/rap2')

dt <- fread(input=paste("clean_data/", dist, ".dat", sep=''), 
            sep='|',
            header=T,
            na.strings='0',
            select=c(4,11,29,26,22))

names(dt) <- c('race.date', 'athlete', 'race.time', 'ag', 'event')
setkey(dt, event, ag)

#- drop records that won't be plotted
grps <- dt[athlete == Athlete & ! is.na(ag), .(event, ag)]
setkey(grps, event, ag)
dt <- dt[grps, nomatch=0]

#- Create a relative days field.
rel.date = mdy(dt$race.date)
min.date = min(rel.date) - 1
dt$seq.date <- (rel.date - min.date)

#- Calculate summary stats and standardized measures
#stdz.time <- dt[, .(stdz.time = mean(race.time)), by=.(event, ag)]
dt[, avg.time := mean(race.time, na.rm=T), by = .(event, ag)]
dt[, sd.time := sd(race.time, na.rm=T), by = .(event, ag)]
dt[, rnk := frank(race.time, ties.method='first'), by=.(event, ag)]
dt[, centd := (race.time - avg.time)]
dt[, stdzd := (centd / sd.time)]

#- get rid of outlying observations
good.grps <- dt[stdzd < .2 & athlete==Athlete, .(event)]
if ( length(good.grps) > 0 ) {
  setkey(dt, event)
  setkey(good.grps, event)
  dt <- dt[good.grps, nomatch=0]
}

#- Create a data.frame of race names for labeling the x-axis graphs
Race.Names = tapply(as.numeric(dt$seq.date), dt$event, mean)
Race.Names = data.frame(rname = names(Race.Names), rday = as.vector(Race.Names))
race.names <- dt[, sum(race.time), by=.(seq.date, event)]

AthletePointStyle = 15
DataPointStyle = 1

dt <- dt[order(seq.date),]

fn_plotfit(as.numeric(dt$seq.date), 
           dt$stdzd, 
           race.names$event, 
           race.names$seq.date,
           which(dt$athlete == Athlete)
           )
# pdf(file = paste(Athlete,
#                  ".pdf",
#                  sep=''),
#     paper='USr',
#     width=10,
#     height=7.5
# )

par(mfrow = c(1,1),
    oma = c(0, 0, 0, 0),
    mar = c(5, 2, 0, 0)
    )

plot(dt$centd ~ as.numeric(dt$seq.date),
     axes = FALSE,
     xlab = ' ',
     ylab = 'Centered/Std Times',
     pch = DataPointStyle
     )
axis(2)
abline(h = 0,
       col = 'green')
points(dt[,c(seq.date, avg.time)],
       col = 'red',
       pch = 3)
points(dt[athlete == Athlete, .(seq.date, stdzd)],
       col = 'red',
       pch = AthletePointStyle)
lines(dt[athlete == Athlete, .(seq.date, stdzd)],
      col = 'red',
      pch = AthletePointStyle)

axis(1, las=2, cex.axis=0.5, labels=race.names$event, at=race.names$seq.date)

#- Least squares of standardized race time on race day
lm.df <- dt[athlete == Athlete,.(seq.date, stdzd)]
trend.mod <- lm(lm.df$stdzd ~ lm.df$seq.date)
x.fitted <- lm.df$seq.date

lines(x.fitted, trend.mod$fitted,
      col = 'blue')

#dev.off()

# write.table(df,
#             file = paste(Athlete,
#                          ".csv",
#                          sep=''),
#             sep=",",
#             row.names=F,
#             col.names=T,
#             quote=F,
#             append=F
# )
#         
