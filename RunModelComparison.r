  # HERE IS THE LIST OF POSSIBLE thisRunName
 # "CC_add_25"
 # "CC_add_35"





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


PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "OA_01", regionNames = list("CalCu_","GoMex_","NOBAtestONLY_", "NEUSFixedF_", "GuamAtlantis_","AEEC_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "OA_005", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_","AEEC_", "AustSE_"))


# Seabirds: looks like NEUS and AustSE might not be driving birds to decline:

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "SB_15", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "SB_3", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "AustSE_"))


# Fcurr_all
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_all_05", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_all_0", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_all_1", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_all_2", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))



# FCUR DEMERSAL. NEUS lacks 05
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_dem_05", regionNames = list("CalCu_","GoMex_","GuamAtlantis_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_dem_0", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_dem_2", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))




# FCUR LARGE PELAGIC, guam does not have
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Lpel_05", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Lpel_0", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Lpel_2", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "AustSE_"))




# FCUR SMALL PELAGIC
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Spel_05", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Spel_0", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_Spel_2", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))



# FCUR INVERTEBRATE

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_inV_05", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_inV_0", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_inV_2", regionNames = list("CalCu_","GoMex_", "NEUSFixedF_", "GuamAtlantis_", "AustSE_"))


PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "mpa_10", regionNames = list("CalCu_","GoMex_", "GuamAtlantis_","AEEC_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "mpa_25", regionNames = list("CalCu_","GoMex_", "GuamAtlantis_","AEEC_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "mpa_50", regionNames = list("CalCu_","GoMex_", "GuamAtlantis_","AEEC_", "AustSE_"))

# Climate change

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "CC_add_2or25", regionNames = list("CalCu_","GoMex_", "GuamAtlantis_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "CC_add_3or35", regionNames = list("CalCu_","GoMex_", "GuamAtlantis_", "AustSE_"))

#MYS for Australia and AAEC

PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_all_05", regionNames = list("AEEC_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_all_1", regionNames = list("AEEC_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_all_2", regionNames = list("AEEC_", "AustSE_"))


PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_dem_05", regionNames = list("AEEC_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_dem_2", regionNames = list("AEEC_", "AustSE_"))



PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_Spel_05", regionNames = list("AEEC_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_Spel_2", regionNames = list("AEEC_", "AustSE_"))



PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_Lpel_05", regionNames = list("AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_Lpel_2", regionNames = list("AustSE_"))



PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_inV_05", regionNames = list("AEEC_", "AustSE_"))
PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fmsy_inV_2", regionNames = list("AEEC_", "AustSE_"))
