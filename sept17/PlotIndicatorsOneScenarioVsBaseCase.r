PlotIndicatorsOneScenarioVsBaseCase <- function(thisRunName = "OA_01", regionNames = list("CalCu_","GoMex_","NOBAtestONLY_", "NEUSFixedF_", "GuamAtlantis_","AEEC_", "AustSE_"),
  dir = "C:/Users/Isaac.Kaplan/Documents/Atlantis/AtlantisSummit/CommonScenarios/CalCurrentSUMMITallcommonscenarios/ModelComparison/ModelComparison/CommonScenarios", yr.start= 40,yr.end=49,autodetectAxisLimits=FALSE)

{

## ============================================================================
##  Plotting Guild-level summary for across model comparison
## Emma Hodgson
##   December 4, 2015
# Minor modifications to file naming and directory structure,
#   Isaac.Kaplan@noaa.gov
#   Feb 26 2016
# THIS IS A  Function, but you must specify some things in  "FINAL BLOCK OF HARD CODED THINGS" BELOW.
#
# TO RUN:  PlotIndicatorsOneScenarioVsBaseCase(thisRunName = "OA_01", regionNames = list("CalCu_", "GOC_"),yr.start=40,yr.end=49)
# #
#  Warnings like this occur if some models lack species in every guild: 1: In max(group.temp) : no non-missing arguments to max; returning -Inf
# 2: In min(group.temp) : no non-missing arguments to min; returning Inf
## ============================================================================
#
# BEFORE RUNNING,  SET  thisRunName, one of the common scenario run names in the standardized nomenclature, without region or file extension.
#   should be on of the following:



#--------------------
  # HERE IS THE LIST OF POSSIBLE thisRunName
 # "CC_add_25"
 # "CC_add_35"
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
 # "mpa_10"
 # "mpa_25"
 # "mpa_50"
 # "OA_005"
 # "OA_01"
 # "SB_15"
 # "SB_3"
 # Note these can be generated at command line DOS prompt by:  dir > myFileDirectoryList.txt


# Get colors for consistent color schemes: 
allregionNames <- list("CalCu_","GoMex_","GOC_","NOBA_", "NEUSFixedF_", "NEUSDyn_","GuamAtlantis_","AEEC_", "AustSE_","AustSE_DynEffort_","CAM_")
#simpleregionNames <- list("California Current","Gulf Mexico","Gulf California","Nordic and Barents Sea", "NE USA fixed F", "NE USA Dyn.F","Guam","English Channel", "SE Australia","SE Australia DynEffort","Chesapeake Bay")
simpleregionNames <- list("Cal. Current","Gulf Mexico","Gulf Calif.","Nordic Barents", "NE USA fixed", "NE USA Dyn","Guam","English Channel", "SE Australia","SE Aus. Dyn","Chesapeake")


colorsToUse <- NA
simpleNamesToUse <-NA

for (regionIndex in 1:length(regionNames))
{
   # If this is a region for which we have a matching color: 
  if ( any( which(allregionNames==regionNames[[regionIndex]] )))
   { 
     colorsToUse[regionIndex] <- which(allregionNames==regionNames[[regionIndex]])
     simpleNamesToUse[regionIndex] <- which(allregionNames==regionNames[[regionIndex]])
   } 
  else
   {
   print("Region not recognized, so using default colors")
   colorsToUse[regionIndex] <- regionIndex
   simpleNamesToUse[regionIndex] <- regionNames[regionIndex]
   
   }


}

simpleregionNamesForTheseModels <- simpleregionNames[simpleNamesToUse]



#----------------------------
# Note on plottign to PDFs if you use ADOBE:  Delete old PDFs and shut down new versions of Adobe acrobat DC completely before running this,
# otherwise you get  stuff like this:
#    Error in pdf(paste("../PanelPlot_", scenario.name, ".pdf", sep = ""),  :
#    cannot open file '../PanelPlot_scenarioX.pdf'
#--------------------------

#------------------------------
# FINAL BLOCK OF HARD CODED THINGS HERE: DEFINE Regions, DIRECTORIES, NUMBER OF GUILDS, AND SOURCE THE HELPER FUNCTION.
#-------------------------------------
# In order for this to run, all of the scenarios need to be put into one folder
# e.g. "CommonScenarios"
currentdirectory <- getwd()
on.exit(setwd(currentdirectory))
setwd(currentdirectory) # was set to dir, don't use that now

num.guilds <- 11 # This I set manually

BaseCaseName <- "BC"

maxNumIndicators <- 100 # just hard coding a max of 100 indicators, many of which can be left blank. 



#-------------
# SOME ADDITIONAL CALCULATIONS

files <- list.files()
num.models <- length(regionNames)

models.list <- list()



#-----------
# LOOP THROUGH EACH MODEL, 

#-------------------


indicators <-  NULL #as.data.frame(matrix(nrow=num.models, ncol=maxNumIndicators))  # GAVIN I WAS TRYING TO INITIALIZE AN EMPTY MATRIX TO HOLD NUM.MODELS X NUMINDICATORS 


for(i in 1:num.models) {
  
  print(i)
  print(regionNames[i])
  indicatorsThisModel <- NULL
  
  
  regionName<- regionNames[i]
  
  baseOutBiomIndxFileName <-  paste(regionName,BaseCaseName, "_BiomIndx.txt", sep="")
  scenarioOutBiomIndxFileName <-paste(regionName,thisRunName, "_BiomIndx.txt", sep="")
  baseOutCatchFileName <-  paste(regionName,BaseCaseName, "_Catch.txt", sep="")
  scenarioOutCatchFileName <-paste(regionName,thisRunName, "_Catch.txt", sep="")
  
  
  print(" DO READ_LOOKUP")
  lookup <- read_lookup(paste(regionName,"BasicInfo.csv",sep=""))
  
  base_bio <- read.table(baseOutBiomIndxFileName,header=TRUE)
  scenario_bio <- read.table(scenarioOutBiomIndxFileName,header=TRUE)
  
  #-----------------------
    # Biomasses -- Make sure all time columns begin with 0. This is to deal with NE USA Fixed F; it is applied to all other models but shouldnt change them. 
  
  base_bio[,1] <- base_bio[,1] - base_bio[1,1] 
  scenario_bio[,1] <- scenario_bio[,1]- scenario_bio[1,1]

  
  
  #---------------
  print(" CHECK NUMBER OF YEARS TO MAKE SURE NOT TOO SHORT ")
  
  #--------------
  # take averages over the sets of years desired -- specified by yearA and yearB
  max.years <- base_bio[nrow(base_bio),1]/365
  
  if(max.years < yr.end) stop("Year range to average over exceeds years in output")
  #------------
  
  #--------
  print(" NOW DEAL WITH PROBLEM THAT NOT ALL MODELS HAVE 365 DAY OUTPUTS. NEED TO FIND ROWS CORRESPODNGING TO RIGHT YEARS.")
  #--------
  row.start.base.bio <- min(which(base_bio[,1]/365>=yr.start))
  row.end.base.bio <- min(which(base_bio[,1]/365>=yr.end))
  
  row.start.scenario.bio <- min(which(scenario_bio[,1]/365>=yr.start))
  row.end.scenario.bio <- min(which(scenario_bio[,1]/365>=yr.end))
  
  
  
  #--------------
  print(" AVERAGE biomass OVER YEARS 40-49 OR WHATEVER IS APPROPRIATE")
  
  # biomass
  base.vec <- apply(base_bio[row.start.base.bio:row.end.base.bio, ], 2, mean)
  scenario.vec <- apply(scenario_bio[row.start.scenario.bio:row.end.scenario.bio, ], 2, mean)
  bio <- as.data.frame(rbind( base.vec,scenario.vec))   #  GAVIN , HERE I AM JUST TRYING TO BIND TWO BIOMASS VECTORS INTO A DATA FRAME I CAN PASS TO GET_INDICATORS
  #bio<-base_bio[1:2,] 
  bio$Time <- 365*floor(bio$Time/365)
  
 # LOAD CATCHES 
 
  base_catch <- read.table(baseOutCatchFileName,header=TRUE)
  scenario_catch <- read.table(scenarioOutCatchFileName,header=TRUE)
  
   #-----------------------
      # Catches -- Make sure all time columns begin with 0. This is to deal with NE USA Fixed F; it is applied to all other models but shouldnt change them. 
    
    base_catch[,1] <- base_catch[,1] - base_catch[1,1]
    scenario_catch[,1] <- scenario_catch[,1]- scenario_catch[1,1]
  
 
 #----------------
  print(" DEAL WITH PROBLEM THAT NOT ALL CATCH FILES HAVE SAME 365 DAY OUTPUT. ASSUME CATCH HAS SAME TIME INTERVALS AS BIOMASS .NEED TO CHECK THIS.") 
   
  
  row.start.base.catch <- min(which(base_catch[,1]/365>=yr.start))
  row.end.base.catch <- min(which(base_catch[,1]/365>=yr.end))
  
  row.start.scenario.catch <- min(which(scenario_catch[,1]/365>=yr.start))
  row.end.scenario.catch <- min(which(scenario_catch[,1]/365>=yr.end))
  
  
  
  print("CHECK START AND END YEARS")
  print( scenario_bio[min(which(scenario_bio[,1]/365>=yr.start)),1])
  print(scenario_bio[min(which(scenario_bio[,1]/365>=yr.end)),1] )
  print( scenario_catch[min(which(scenario_catch[,1]/365>=yr.start)),1])
  print(scenario_catch[min(which(scenario_catch[,1]/365>=yr.end)),1])
  
  
  
  
  base.cat <- apply(base_catch[row.start.base.catch:row.end.base.catch, ], 2, mean)
  scenario.cat <- apply(scenario_catch[row.start.scenario.catch:row.end.scenario.catch, ], 2, mean)
  cat <- as.data.frame(rbind( base.cat,scenario.cat))   #  #  GAVIN , HERE I AM JUST TRYING TO BIND TWO BIOMASS VECTORS INTO A DATA FRAME I CAN PASS TO GET_INDICATORS
  cat$Time<- 365*floor(cat$Time/365)
  # cat <- base_catch[1:2,]
  
  
  
  rownames(bio) <- NULL
  rownames(cat)<- NULL
  
  test <-get_indicators(bio[1,],cat[1,],lookup)
  length(test)
  
  print("GET INDICATORS")
  indicatorsThisModel <- rbind ( indicatorsThisModel ,get_indicators(bio[1,],cat[1,],lookup))
  indicatorsThisModel <- rbind ( indicatorsThisModel ,get_indicators(bio[2,],cat[2,],lookup))
  print("PRINTING BIO AND THEN CAT")
  print(bio)
  print(cat)
  #print("printing original base_catch")
  #print(base_catch)
  #print(scenario_catch)
  
  
  print("START ASSIGNING TO INDICATORS MATRIX")
  print("check here")
  print( indicatorsThisModel[1,] )
  print(indicatorsThisModel[2,])
  
  indicators <- rbind(indicators, indicatorsThisModel[2,]/indicatorsThisModel[1,])
  
  #indicators <- rbind(rep(-1 ,times= length(test)  ), rep(1,times= length(test)), (indicatorsThisModel[2,]/indicatorsThisModel[1,]-1))
  
  #indicators[i,]<- indicatorsThisModel[2,]/indicatorsThisModel[1,]    # GAVIN, I AM JUST TRYING TO ADD A ROW TO INDICATORS[ ], AFTER NORMALIZING BY INDICATOR SCORES FROM THE BASE CASE RUN. 
  print("DONE ASSIGNING TO INDICATORS MATRIX")
  
}  # end loop over models





print(indicators)


# QUESTIONS IS WHETHER INDICAOTR SCORES WILL BE NORMALIZED BY PLOT_INDICATORS IN RADAR PLOT, I DONT WANT THIS TO HAPPEN SINCE I HAVE ALREADY NORMALIZED. 

#----------------
print(" PREPARING TO MAKE THE PLOT: ")

#--------------

coloursAvailableForAllModels <- colorschemes$SteppedSequential.5[c(1,3,5,7,9,11,13,15,17,19,21,23)]  #colorschemes$SteppedSequential.5[seq(1,23,by=2)]   #colorschemes$SteppedSequential.5  # from Dichromat package, colorblind colour blind
#coloursAvailableForAllModels = colorRampPalette(c("#8B1A1A", "#CD6600", "#EEB422", "#228B22","#104E8B", "#68228B"), space ="rgb")(12)
coloursForTheseModels <- coloursAvailableForAllModels[colorsToUse]
coloursForTheseModels.transparent <- paste(coloursForTheseModels, 90, sep="")




# PLOT COMBINED INDICATORS ECOLOGICAL AND ECONOMIC 
ind_choose6 <- c("Pelbio_pp","Bio_pp","Mtlbio","Predfish_prop","Dempelfish","Dempel","Dembio_pp","Demcat","Fishcat","Value","Exprate","Fish_exprate","Mtlcat","Totcat","Pelcat")
plot_indicators(ind=indicators[,c("Time",ind_choose6)],
                axis_labels= c("Pel bio/PP","Bio/PP","MTL bio","Predfish prop","Dem/ pel fish","Dem/pelagic ","Dem bio/ PP","Dem cat","Fish cat","Value","Exp rate","Fish exp rate","MTL catch","Total catch","Pel catch"),
                legend_labels = simpleregionNamesForTheseModels,plotfile=paste("IndicatorPlotEconEcol_", thisRunName, ".png", sep=""), standardized = TRUE, colvec = coloursForTheseModels,autodetectAxisLimits=autodetectAxisLimits)





} # end function
