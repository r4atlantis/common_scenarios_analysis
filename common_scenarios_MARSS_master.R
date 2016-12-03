###############################################################################
###############################################################################
#-----------------------------------------------------------------------------#
####Author     : Kelli Faye Johnson
####Contact    : kellifayejohnson@gmail.com
####Lastupdate :
####Purpose    : Common scenarios MARSS indicator analysis
####Packages   :
####Inputs     :
####Outputs    :
####Remarks    : Character width = 80
#-----------------------------------------------------------------------------#
###############################################################################
###############################################################################

# Set some initial inputs
dir.main <- "c:/atlantis/common_scenarios_analysis"
dir.data <- "common_scenarios_data"
my.regionnames <- list(
  "CAM_",
  "CalCu_",
  "GoMex_",
  "GOC_",
  "NEUSFixedF_",
  "GuamAtlantis_",
  "AustSE_"
  )
my.year.start <- 40
my.year.end <- 49

# Source the files to run the analysis
source("common_scenarios_MARSS_initialize.R")

temp <- read_data(scenarios = "OA_01",
  regions = list("CalCu_", "GOC_"), dir = dir.data)


