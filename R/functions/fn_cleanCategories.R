  #-------------------------------------------
  #- PARENT PACKAGE: Race Analysis Project
  #- MODULE NAME: fn_cleanCategories.R
  #- DESCRIPTION: clean and combine the category levels
  #- AUTHOR: lg
  #- DATE: 2021-01-12
  #- ------------------------------------------

cleanCategories <- function(x){

  x$rcat = x$ag
  x[x$rcat == 0, 'rcat'] <- x[x$rcat == 0, 'cat']
  x$rcat <- as.factor(x$rcat)
  
  levels(x$rcat)[levels(x$rcat) %in% c('EF','FO','OF')] <-"Open Female"
  levels(x$rcat)[levels(x$rcat) %in% c('EM','MO','OM')] <-"Open Male"
  levels(x$rcat)[levels(x$rcat) %in% c('A', 'ATHENA', 'MA', 'MASTER ATHENA')] <-"Athena"
  levels(x$rcat)[levels(x$rcat) %in% c('C', 'CLYDESDALE', 'MC', 'MASTER CLYDESDALE')] <-"Clydesdale"
  levels(x$rcat)[levels(x$rcat) %in% c('PC', 'PHYSICALLY CHALLENGED')] <-"PC"
  levels(x$rcat)[levels(x$rcat) %in% c('NF')] <-"Novice Female"
  levels(x$rcat)[levels(x$rcat) %in% c('NM')] <-"Novice Male"
  levels(x$rcat)[levels(x$rcat) %in% c('F0015', 'F1014', 'F13-', 'F1415')] <-"F1014"
  levels(x$rcat)[levels(x$rcat) %in% c('F1619', 'F15')] <-"F1519"
  levels(x$rcat)[levels(x$rcat) %in% c('M0015', 'M1014', 'M13-', 'M1415')] <-"M1014"
  levels(x$rcat)[levels(x$rcat) %in% c('M1619', 'M15')] <-"M1519"
  levels(x$rcat)[levels(x$rcat) %in% c('MM', 'MMAG')] <-"Masters Male"
  levels(x$rcat)[levels(x$rcat) %in% c('MF', 'FMAG', 'MFAG')] <-"Masters Female"
  
  x <- x %>%
    filter(!rcat %in% c('ZF', 'ZM', 'TOP 3 OVERALL', '0', 'MASTERS AGE GROUP', 'BF', 'BM') )
  
  x$rcat <- droplevels(x$rcat)

  return(x)
} 
