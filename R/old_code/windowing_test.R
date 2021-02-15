library(data.table)
grp <- c('A','A','A','A','A','B','B','B')
val <- c(1,8,2,25,9,16,32,4)
dt <- data.table(grp=grp, val=val)

dt[,rnk := frank(val, ties.method='first'), by=grp]

