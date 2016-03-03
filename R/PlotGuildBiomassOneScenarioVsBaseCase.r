PlotGuildBiomassOneScenarioVsBaseCase <- function(thisRunName = "OA_01", regionNames = list("CalCu_","GoMex_","NOBAtestONLY_", "NEUSFixedF_", "GuamAtlantis_","AEEC_", "AustSE_"),
  dir = "C:/Users/Isaac.Kaplan/Documents/Atlantis/AtlantisSummit/CommonScenarios/CalCurrentSUMMITallcommonscenarios/ModelComparison/ModelComparison/CommonScenarios")

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
# TO RUN:  PlotGuildBiomassOneScenarioVsBaseCase() or for example PlotGuildBiomassOneScenarioVsBaseCase(thisRunName = "Fcur_all_2")
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
setwd(dir)

source("Functions_ScenarioEffect_2.03.2016.r")  # soure the main function that calculates impacts that will be plotted here.

#regionNames <- list("CalCu_","GoMex_","NOBAtestONLY_", "NEUSFixedF_", "GuamAtlantis_","AEEC_", "ams71SPF_")  # GuamAtlantis_ NOBAtestONLY is real output but wrong scenario, just testing formats here.

num.guilds <- 11 # This I set manually

BaseCaseName <- "BC"



#-------------
# SOME ADDITIONAL CALCULATIONS

files <- list.files()
num.models <- length(regionNames)

models.list <- list()




#-----------
# LOOP THROUGH EACH MODEL, CALCULATE THE EFFECT SIZE.
# All I can think of for now is to just loop through each model, I am not saving
# anything for each for now
#-------------------
for(i in 1:num.models) {

  print(i)

  regionName<- regionNames[i]

  baseOutBiomIndxFileName <-  paste(regionName,BaseCaseName, "_BiomIndx.txt", sep="")
  scenarioOutBiomIndxFileName <-paste(regionName,thisRunName, "_BiomIndx.txt", sep="")

  print(regionNames[i])
  temp.dat<- calc.pH.effect(baseOutBiomIndxFileName, scenarioOutBiomIndxFileName, yr.start = 40, yr.end = 49, scenarioNameString="GiveName", CSV.file.name = paste(regionName,"BasicInfo.csv",sep=""), ifplot=F)


  models.list[[i]] <- temp.dat$dataframe.effect

  # CAN PROBABLY ADD THIS BACK. names(models.list[[i]]) <- regionNames[i] #files[1]

}


#----------------
# PREPARING TO MAKE THE PLOT: SET UP SPECIFICATION AND GUILDS

#--------------

names <- c("Mammal", "Seabird", "Shark", "Demersal Fish",
           "Pelagic Fish", "Squid", "Filter Feeder", "Epibenthos",
           "Zooplankton", "Primary Producer", "Infauna", "Detritus")
x.locations <- c(1:num.guilds)*num.models
colours = colorRampPalette(c("#8B1A1A", "#CD6600", "#EEB422", "#228B22",
                             "#104E8B", "#68228B"), space ="rgb")(num.models)
col.trans <- paste(colours, 90, sep="")
xmax <- num.guilds*num.models
ymin <- -1 # NEED TO FIX THIS AT SOME POINT AS WELL.
ymax <- 1
# Order that we want to cycle through the groups
group.order <- c("IsMammal", "IsBird", "IsShark", "IsDemersal", "IsPelagic",
                 "IsSquid", "IsFilterFeeder", "IsEpibenthos", "IsZooplankton",
                 "IsPrimaryProducer", "IsInfauna")
#"IsJellyfish", "IsForageFish", "IsKrill", "IsPiscivore", "1"))


#----------------------------
# Note on plottign to PDFs:  Delete old PDFs and shut down new versions of Adobe acrobat completely before running this,
# otherwise get  stuff like this:
#    Error in pdf(paste("../PanelPlot_", scenario.name, ".pdf", sep = ""),  :
#    cannot open file '../PanelPlot_scenarioX.pdf'
#--------------------------



#----------------------------
# Writing plots to PDFs :   All in One Plot
#
#------------------




pdf(paste("AllInOnePlot_", thisRunName, ".pdf", sep=""), width=20, height=8.5)
par(oma=c(4,0,0,0))
par(mar=c(5.1, 4.1, 1, 2.1))

plot(x=1, y=1, col="white", axes=F, xlim=c(1, xmax), ylim=c(ymin, ymax),
     xlab="", ylab="")
axis(2, las=1)
text(x=x.locations-(num.models/2), y=ymin-.1, lab=names[1:11], xpd=T, srt=45, adj=.9)
lines(x=c(0.5,xmax), y=c(0,0))
abline(v=x.locations+.5)

counter=0
for(j in 1:length(group.order)) {
  for(i in 1:num.models) {
    counter=counter+1
    temp.dat <- models.list[[i]]
    group.temp <- temp.dat[1, temp.dat[rownames(temp.dat) == group.order[j],]==1]
    group.temp <- group.temp[!is.na(group.temp)]  #   *rnorm(1, 1)
    points(x=counter, y=mean(unlist(group.temp)), col=colours[i], pch=16)
    lines(x=rep(counter, 2), y=c(max(group.temp), min(group.temp)), lwd=6, col=col.trans[i])
  }
}
legend("topleft", col=colours[1:num.models], bty="n", pch=c(19,19),
       legend=regionNames, cex=1)


dev.off()


#----------------------------
# Writing plots to PDFs :   Panel Plot
#--------------------------

pdf(paste("PanelPlot_", thisRunName, ".pdf", sep=""), width=10, height=8)
par(oma=c(0,0,0,0))
par(mar=c(1.1, 3.1, 1, 2.1))
#"IsJellyfish", "IsForageFish", "IsKrill", "IsPiscivore", "1"))
par(mfrow=c(3,4))
counter=0
for(j in 1:length(group.order)) {
  plot(x=1, y=1, col="white", axes=F, xlim=c(1, length(group.order)),
       ylim=c(ymin, ymax), xlab="", ylab="")
  axis(2, las=1)
  text(x=3, y=ymax, lab=names[j], xpd=T)
  # this works well when we have lots of models : text(x=(num.models/2), y=ymax, lab=names[j], xpd=T)
  lines(x=c(0.5,xmax), y=c(0,0))

  for(i in 1:num.models) {
    counter=counter+1
    temp.dat <- models.list[[i]]
    group.temp <- temp.dat[1, temp.dat[rownames(temp.dat) == group.order[j],]==1]
    group.temp <- group.temp[!is.na(group.temp)]   # *rnorm(1, 1)
    points(x=i, y=mean(unlist(group.temp)), col=colours[i], pch=16)
    lines(x=rep(i, 2), y=c(max(group.temp), min(group.temp)), lwd=6, col=col.trans[i])
  }
}

plot(x=1, y=1, col="white", axes=F, xlim=c(1, length(group.order)),
     ylim=c(ymin, ymax), xlab="", ylab="")
legend("topleft", col=colours, bty="n", pch=rep(16, num.models),
       legend=regionNames, cex=1.2)

dev.off()

} # end function
