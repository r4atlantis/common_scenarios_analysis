  # HERE IS THE LIST OF POSSIBLE thisRunName
 # "CC_add_25"
 # "CC_add_35"




#  Things to do following June 20:
#   DONE:   Change GulfMexico corals to filter feeders.
#   DONE: Use new NOBA BasicInfo.csv
#   DONE: Replace Guam with May 26 output.
#   DONE: Add in Chesapeake data
#   DONE: #   Use consistent color
#   DONE: Eliminate indicators that just represent Biomass
#   DONE,  Add labels on indictors axes
#   DONE: Update CalCurrent BasicInfo.csv

# Careful to see if color schemes on indicator plots match labels.
#Consider eliminating poorly specified indicators, like Pisciovre or Predatory Fish

#----------------
# DONE: Ln Resposne Ratio:  contract y axis, then rotate axes.
#  ? PCA on Ln Reesponse Rati.
#--------
#  DONE:  Biomass plots:  add pts for individual species.
#  DONE: Biomass plots:  Indicate when bars exceed limits of y axis.
#
#
# check   Fcurr_all_1  -- use this to check base case.
#
#--------------



 # "OA_005"
 # "OA_01"
 # "SB_15"
 # "SB_3"
 # "Fcur_all_05"
 # "Fcur_all_0"
 # "Fcur_all_1"
 # "Fcur_all_2"
 # "Fcur_dem_05"
 # "Fcur_dem_0"
 # "Fcur_dem_2"
 # "Fcur_inV_05"
 # "Fcur_inV_0"
 # "Fcur_inV_2"
 # "Fcur_Lpel_05"
 # "Fcur_Lpel_0"
 # "Fcur_Lpel_2"
 # "Fcur_Spel_05"
 # "Fcur_Spel_0"
 # "Fcur_Spel_2"
  # "mpa_25"
 # "mpa_50"
  # "mpa_10"

ignore <- sapply(dir("R", pattern = "\\.R|\\.r", full.names = TRUE), source)

PlotIndicatorsOneScenarioVsBaseCaseGOMEX(thisRunName = "Fcur_all_2", regionNames = list("GoMex_"),yr.start=10,yr.end=20)
