#- R function stdz
#- This function takes an array numbers and returns the standardized (mean, sd) values for each number.
#-
#- INPUTS: x = array of numbers

stdz <- function(x=NULL, in.mean=NULL, in.sd=NULL) {
  
  if ( is.null(x) ) {
    x = 0
    return(9)
  }

  use.mean <- in.mean
  use.sd <- in.sd
  
  if ( is.null(in.mean) ) { use.mean = mean(x, na.rm=T) }
  if ( is.null(in.sd) ) { use.sd = sd(x, na.rm=T) }
  
  if ( use.sd <= 0) { return(9) }
  
  cent <- x - use.mean
  
  #- send back the standardized values
  cent / use.sd
}