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

# PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "OA_01", regionNames = list("CalCu_", "GOC_"),yr.start=40,yr.end=49)
#  PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "OA_01", regionNames = list("CalCu_", "GOC_"),yr.start=40,yr.end=49)


#-------------------
# ocean acidification
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "OA_01", regionNames = list("CAM_", "CalCu_","GoMex_","GOC_","NOBA_", "NEUSFixedF_","NEUSDyn_","GuamAtlantis_","AEEC_", "AustSE_","AustSE_DynEffort_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "OA_005", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_", "NEUSFixedF_", "NEUSDyn_","GuamAtlantis_","AEEC_", "AustSE_","AustSE_DynEffort_"))

#
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "OA_01", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_","NEUSFixedF_","NEUSDyn_", "GuamAtlantis_", "AEEC_", "AustSE_","AustSE_DynEffort_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "OA_005", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_","NEUSFixedF_","NEUSDyn_", "GuamAtlantis_", "AEEC_", "AustSE_","AustSE_DynEffort_"),yr.start=40,yr.end=49)


#-----------------
# Seabirds: looks like NEUS and AustSE might not be driving birds to decline:
# CAM chesapeake and AEEC adn  Guam does not include SB seabird runs.

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "SB_15", regionNames = list("CalCu_","GoMex_","GOC_","NOBA_", "NEUSFixedF_", "AustSE_","AustSE_DynEffort_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "SB_3", regionNames = list("CalCu_","GoMex_","GOC_","NOBA_", "NEUSFixedF_",  "AustSE_","AustSE_DynEffort_"))

# NOBa works now
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "SB_15", regionNames = list("CalCu_","GoMex_","GOC_","NOBA_", "NEUSFixedF_",  "AustSE_","AustSE_DynEffort_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "SB_3", regionNames = list("CalCu_","GoMex_","GOC_","NOBA_", "NEUSFixedF_", "AustSE_","AustSE_DynEffort_"),yr.start=40,yr.end=49)


#--------------------------------
# Fishign relative to Current day:
# Fcurr_all
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_all_05", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_all_0", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_all_1", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_all_2", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

#
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_all_05", regionNames = list("CAM_", "CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_","AustSE_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_all_0", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_","GuamAtlantis_","AustSE_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_all_1", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_","GuamAtlantis_", "AustSE_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_all_2", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_","GuamAtlantis_", "AustSE_"),yr.start=40,yr.end=49)






# FCUR DEMERSAL.
# Note:  CAM_Fcur_dem_05_BiomIndx.txt': No such file or directory
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_dem_05", regionNames = list("CalCu_","GoMex_","GOC_","NEUSFixedF_", "GuamAtlantis_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_dem_0", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_dem_2", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

# # FCUR DEMERSAL.
# Note 'CAM_Fcur_dem_05_BiomIndx.txt': No such file or directory
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_dem_05", regionNames = list("CalCu_","GoMex_","GOC_","NEUSFixedF_", "GuamAtlantis_","AustSE_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_dem_0", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_","AustSE_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_dem_2", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_","AustSE_"),yr.start=40,yr.end=49)





# FCUR LARGE PELAGIC, guam does not have
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Lpel_05", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Lpel_0", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Lpel_2", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "AustSE_"))

PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_Lpel_05", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "AustSE_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_Lpel_0", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "AustSE_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_Lpel_2", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "AustSE_"),yr.start=40,yr.end=49)

# just testing with Gavin PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_Lpel_2", regionNames = list("CalCu_"),yr.start=40,yr.end=49)



# FCUR SMALL PELAGIC
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Spel_05", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Spel_0", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Spel_2", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

# SKIPPING guam FOR NOW, SOME FORMATTIN PROBLEM FOR guam :
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_Spel_05", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_","AustSE_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_Spel_0", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_","AustSE_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_Spel_2", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_","AustSE_"),yr.start=40,yr.end=49)



# FCUR INVERTEBRATE

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_inV_05", regionNames = list("CAM_","CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_inV_0", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_inV_2", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

#
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_inV_05", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_","GuamAtlantis_", "AustSE_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_inV_0", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_","AustSE_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fcur_inV_2", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSFixedF_", "GuamAtlantis_","AustSE_"),yr.start=40,yr.end=49)



#--------------------------------------
# MPAs

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "mpa_10", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_", "NEUSDyn_", "GuamAtlantis_","AEEC_", "AustSE_","AustSE_DynEffort_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "mpa_25", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_", "NEUSDyn_", "GuamAtlantis_","AEEC_", "AustSE_","AustSE_DynEffort_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "mpa_50", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_", "NEUSDyn_", "GuamAtlantis_","AEEC_", "AustSE_","AustSE_DynEffort_"))

# SKIPPING GUAM, NOBA, AND AEEC FOR NOW FOR SOME FORMATTING REASON:

PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "mpa_10", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_", "NEUSDyn_", "GuamAtlantis_","AEEC_", "AustSE_","AustSE_DynEffort_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "mpa_25", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_", "NEUSDyn_", "GuamAtlantis_","AEEC_", "AustSE_","AustSE_DynEffort_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "mpa_50", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_", "NEUSDyn_", "GuamAtlantis_","AEEC_", "AustSE_","AustSE_DynEffort_"),yr.start=40,yr.end=49)
#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "mpa_50", regionNames = list("CAM_","CalCu_","GoMex_","GOC_", "NEUSDyn_", "AustSE_","AustSE_DynEffort_"),yr.start=40,yr.end=49)

#-------------------------

# Climate change

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "CC_add_2or25", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_", "NEUSDyn_", "GuamAtlantis_", "AustSE_","AustSE_DynEffort_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "CC_add_3or35", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_", "NEUSDyn_", "GuamAtlantis_", "AustSE_","AustSE_DynEffort_"))

# skipping guam and noba for now:
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "CC_add_2or25", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_", "NEUSDyn_", "GuamAtlantis_", "AustSE_","AustSE_DynEffort_"),yr.start=40,yr.end=49)
PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "CC_add_3or35", regionNames = list("CAM_","CalCu_","GoMex_","GOC_","NOBA_", "NEUSDyn_", "GuamAtlantis_", "AustSE_","AustSE_DynEffort_"),yr.start=40,yr.end=49)
#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "CC_add_3or35", regionNames = list("CalCu_","GoMex_","GOC_", "NEUSDyn_", "AustSE_","AustSE_DynEffort_"),yr.start=40,yr.end=49)




#----------------------------------
#MYS for NOBA Australia and AAEC

#PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_all_05", regionNames = list("NOBA_","AEEC_", "AustSE_"))
#PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_all_1", regionNames = list("NOBA_","AEEC_", "AustSE_"))
#PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_all_2", regionNames = list("NOBA_","AEEC_", "AustSE_"))



#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fmsy_all_05", regionNames = list( "NOBA_","AEEC_", "AustSE_"),yr.start=40,yr.end=49)
#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fmsy_all_1", regionNames = list("NOBA_","AEEC_", "AustSE_"),yr.start=40,yr.end=49)
#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fmsy_all_2", regionNames = list("NOBA_","AEEC_", "AustSE_"),yr.start=40,yr.end=49)



#---------------------------------
#MYS for specific groups, Australia and AAEC

#PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_dem_05", regionNames = list("AEEC_", "AustSE_"))
#PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_dem_2", regionNames = list("AEEC_", "AustSE_"))
#PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_Spel_05", regionNames = list("AEEC_", "AustSE_"))
#PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_Spel_2", regionNames = list("AEEC_", "AustSE_"))
#PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_Lpel_05", regionNames = list("AustSE_"))
#PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_Lpel_2", regionNames = list("AustSE_"))
#PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_inV_05", regionNames = list("AEEC_", "AustSE_"))
#PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_inV_2", regionNames = list("AEEC_", "AustSE_"))


#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fmsy_dem_05", regionNames = list("AEEC_","AustSE_"),yr.start=40,yr.end=49)
#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fmsy_dem_2", regionNames = list("AEEC_","AustSE_"),yr.start=40,yr.end=49)
#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fmsy_Spel_05", regionNames = list("AEEC_","AustSE_"),yr.start=40,yr.end=49)
#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fmsy_Spel_2", regionNames = list( "AEEC_","AustSE_"),yr.start=40,yr.end=49)
#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fmsy_Lpel_05", regionNames = list("AEEC_","AustSE_"),yr.start=40,yr.end=49)
#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fmsy_Lpel_2", regionNames = list("AEEC_","AustSE_"),yr.start=40,yr.end=49)
#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fmsy_inV_05", regionNames = list("AEEC_","AustSE_"),yr.start=40,yr.end=49)
#PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "Fmsy_inV_2", regionNames = list( "AEEC_","AustSE_"),yr.start=40,yr.end=49)
