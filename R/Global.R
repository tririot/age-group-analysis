#  ------------------------------------------------------------------------
#
# Title : Global.R
#    By : lg
#  Date : 2021-01-11
#    
#  ------------------------------------------------------------------------
#setwd('/Users/gould/SPG/code_dev/repositories/github/shiny-external_data_portal/R')

# Packages ----------------------------------------------------------------
library("shiny")
library("xlsx")
library('dplyr')
library('lubridate')
library('ggplot2')
library('DT')
library('data.table')
library('tidyr')
library('purrr')
library('tibble')

# Functions ---------------------------------------------------------------
source("functions/fn_getData.R")
source("functions/fn_getSummaryTable.R")
source("functions/fn_plural.R")
source("functions/fn_cleanCategories.R")
source("functions/fn_cleanGenders.R")
source("functions/fn_cleanTimes.R")
source("functions/fn_calcGroupSize.R")
source("functions/fn_calcKmeans.R")

# Modules ----------------------------------------------------------------
source("modules/mod_dataChooser.R")
source("modules/mod_showScatter.R")
source("modules/mod_showBox.R")
source("modules/mod_showKmeans.R")

# Global variables --------------------------------------------------------
races <- getData('/Users/gould/LX/src/repositories/Race-Analysis-Project/data/raw.Rdata')
races <- cleanCategories(races)
races <- cleanGenders(races)
races <- cleanTimes(races)
races <- calcGroupSize(races)

# Import Data --------------------------------------------------------


