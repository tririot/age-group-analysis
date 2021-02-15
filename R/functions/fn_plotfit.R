fn_plotfit <- function (x=NULL, 
                        y=NULL, 
                        lbl=NULL, 
                        lbl.pos=NULL,
                        mkr=NULL) {
  AthletePointStyle = 15
  DataPointStyle = 1
  
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
  
  plot(y ~ x,
       axes = FALSE,
       xlab = ' ',
       ylab = 'Times',
       pch = DataPointStyle
  )
  axis(2)
  abline(h = 0,
         col = 'green')
  #points(dt[,c(jul.date, avg.time)],
  #       col = 'red',
  #       pch = 3)
  points(x[mkr], y[mkr],
         col = 'red',
         pch = AthletePointStyle)
  lines(x[mkr], y[mkr],
        col = 'red',
        pch = AthletePointStyle)
  
  axis(1, las=2, cex.axis=0.5, labels=lbl, at=lbl.pos)
  
  #- Least squares of standardized race time on race day
  #lm.df <- dt[athlete == Athlete,.(jul.date, stdzd)]
  trend.mod <- lm(y[mkr] ~ x[mkr])
  x.fitted <- x[mkr]
  
  lines(x.fitted, trend.mod$fitted,
        col = 'blue')
  
  #dev.off()
}
