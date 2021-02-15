

qryString = "SELECT EVENT AS event,
                 RACE AS race,
                 AG AS ag,
                 AVG([TIME]/60) AS mnTime,
                 MIN([TIME]/60) AS minTime,
                 COUNT(*) AS n 
             FROM RaceData.Import.TriData T1
             WHERE DISTANCE IN ( 'SPRINT', 'INTL')
                AND AGE >= 35
                AND AGE <= 64
                AND AG <> '0'
                AND [TIME]/60 > 43
                AND [TIME] IS NOT NULL
                AND GENDER = 'MEN'
                AND [YEAR] >= 2008
             GROUP BY EVENT, RACE, AG
                HAVING COUNT(*) > 1"

df <- getSQL('RaceData',qryString)

#- Calculate the mean time by race and center the observations
AvgTimes = tapply(df$racetime, df$event, mean)
AvgTimes = data.frame(event = names(AvgTimes), mnTime = as.vector(AvgTimes))
AvgTimes = cbind(AvgTimes, event.code = row.names(AvgTimes))
newdf = merge(df, AvgTimes, by.x = 'event', by.y = 'event')
Centered = newdf$racetime - newdf$mnTime

#- Calclate the sd time by race and standardize the observations
sdTimes = tapply(df$racetime, df$event, sd)
sdTimes = data.frame(event = names(sdTimes), sdTime = as.vector(sdTimes))
newdf = merge(newdf, sdTimes, by.x = 'event', by.y = 'event')
Standardized = Centered / newdf$sdTime

#- Bind the centered and standardized data to the new data frame
newdf = cbind(newdf,
  cent=Centered,
  std=Standardized
  )

AthletePointStyle = 15
DataPointStyle = 1

par(mfrow = c(3,1),
    oma = c(0, 0, 1, 0),
    mar = c(2, 4, 0, 0)
    )
#- Plot racetimes
plot(newdf$racetime ~ as.numeric(newdf$event.code),
     axes = FALSE,
     xlab = ' ',
     ylab = 'Unadjusted Time',
     pch = DataPointStyle
     )
axis(2)
abline(h = mean(newdf$racetime),
       col = 'red')
points(newdf[,c('event.code', 'mnTime')],
       col = 'red',
       pch = 3)
points(newdf[newdf$athlete == "LOWELL GOULD",c('event.code', 'racetime')],
       col = 'red',
       pch =  AthletePointStyle)

#- Plot centered racetimes
plot(newdf$cent ~ as.numeric(newdf$event.code),
     axes = FALSE,
     xlab = ' ',
     ylab = 'Centered Times',
     pch = DataPointStyle
     )
axis(2)
abline(h = mean(newdf$cent),
       col = 'red')
points(newdf[,c('event.code', 'mnTime')],
       col = 'red',
       pch = 3)
points(newdf[newdf$athlete == "LOWELL GOULD",c('event.code', 'cent')],
       col = 'red',
       pch =  AthletePointStyle)

#- Plot centered/standardized racetimes
par (mar = c(4, 4, 0, 0))
plot(newdf$std ~ as.numeric(newdf$event.code),
     axes = FALSE,
     xlab = 'race',
     ylab = 'Centered/Std Times',
     pch = DataPointStyle
     )
axis(2)
abline(h = mean(newdf$cent),
       col = 'red')
points(newdf[,c('event.code', 'mnTime')],
       col = 'red',
       pch = 3)
points(newdf[newdf$athlete == "LOWELL GOULD",c('event.code', 'std')],
       col = 'red',
       pch = AthletePointStyle)

