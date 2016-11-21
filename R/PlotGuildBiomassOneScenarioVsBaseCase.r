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
setwd(currentdirectory) # was set to dir, don't use that now

# check to see if "calc.pH.effect" exists in the working directory
# "calc.pH.effect" calculates the impacts that will be plotted here.
#if (!"calc.pH.effect" %in% ls()) {
#  stop(paste("\nThe function calc.pH.effect does not exist in your\n",
#    "global environment. Please source the function from the\n",
#    "r4atlantis\\common_scenarios_analysis\\R folder"))
#}

#regionNames <- list("CalCu_","GoMex_","NOBAtestONLY_", "NEUSFixedF_", "GuamAtlantis_","AEEC_", "ams71SPF_")  # GuamAtlantis_ NOBAtestONLY is real output but wrong scenario, just testing formats here.

num.guilds <- 11 # This I set manually

BaseCaseName <- "BC"



#-------------
# SOME ADDITIONAL CALCULATIONS

files <- list.files()
num.models <- length(regionNames)

models.list <- list()

#-------------

# Setting up color scheme, to be conistent across plots and alos consistent with the indicators plots: 

allregionNames <- list("CalCu_","GoMex_","GOC_","NOBA_", "NEUSFixedF_", "NEUSDyn_","GuamAtlantis_","AEEC_", "AustSE_","AustSE_DynEffort_","CAM_")
simpleregionNames <- list("California Current","Gulf Mexico","Gulf California","Nordic and Barents Sea", "NE USA fixed F", "NE USA Dyn.F","Guam","English Channel", "SE Australia","SE Australia DynEffort","Chesapeake Bay")


colorsToUse <- NA
simpleNamesToUse <-NA

for (regionIndex in 1:length(regionNames))
{
colorsToUse[regionIndex] <- which(allregionNames==regionNames[[regionIndex]])
simpleNamesToUse[regionIndex] <- which(allregionNames==regionNames[[regionIndex]])
}

print("colorsToUse")
print(colorsToUse)

coloursAvailableForAllModels = colorRampPalette(c("#8B1A1A", "#CD6600", "#EEB422", "#228B22","#104E8B", "#68228B"), space ="rgb")(12)
coloursForTheseModels <- coloursAvailableForAllModels[colorsToUse]
coloursForTheseModels.transparent <- paste(coloursForTheseModels, 90, sep="")
simpleregionNamesForTheseModels <- simpleregionNames[simpleNamesToUse]


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


  # NEED TO GO IN RIGHT HERE AND CALL GETINDICATORS, FOR EACH MODEL AND ALSO FOR ITS BASE CASE. 
  # THEN DOWN BELOW PLOT INDICAOTRS (NORMALIZED BY BASE CASE) EITHER USING EMMA STYLE PLOTS OR  PLOT_INDICATORS

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
#colours = colorRampPalette(c("#8B1A1A", "#CD6600", "#EEB422", "#228B22",
#                            "#104E8B", "#68228B"), space ="rgb")(num.models)
#col.trans <- paste(colours, 90, sep="")
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



png(paste("AllInOnePlot_", thisRunName, ".png", sep=""),width=20,height=8.5,units="in",res=72)   #,width=10, height = 4.5 ) # 20 8.5
#jpeg(paste("AllInOnePlot_", thisRunName, ".jpeg", sep=""),units = "in", width=20 , height=8.5) # 20 8.5
#par(oma=c(2,0,0,0))
par(oma=c(4,0,0,0))
#par(mar=c(2.6, 2.1, 0.5, 1.1))
par(mar=c(5.1, 4.1, 1, 2.1))


#Isaac output text for Gavin
meanResponsePerModel <- matrix(nrow = num.models, ncol = length(group.order)) 
cvResponsePerModel <- matrix(nrow = num.models, ncol = length(group.order)) 
geommeanResponseOverModels <- matrix(nrow = 1, ncol = length(group.order)) 



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
    points(x=counter, y=mean(unlist(group.temp)), col="black", pch=16,cex=1.5)
    numSppAsPoints <- length(unlist(group.temp))
    points(x=rep(counter,numSppAsPoints),y=unlist(group.temp),col=coloursForTheseModels.transparent[i],pch= 17)
    lines(x=rep(counter, 2), y=c(max(group.temp), min(group.temp)), lwd=6, col=coloursForTheseModels.transparent[i])
    
        if (!is.na( unlist(group.temp)) && ( min(group.temp) < ymin))
             {
              text(x = counter  , y = 0.9*ymin, lab = toString(round(min(group.temp),digits=1)), col="black",cex=1.1, srt=90 )
             }
           if (!is.na( unlist(group.temp)) && (max(group.temp) > ymax))
             {
             text(x = counter  , y = 0.9*ymax, lab = toString( round(max(group.temp),digits=1)), col="black",cex=1.1, srt=90 )
             }
    
    #Isaac output text for Gavin
    meanResponsePerModel[i,j] <- mean(unlist(group.temp))
    cvResponsePerModel[i,j] <- sd(unlist(group.temp))/mean(unlist(group.temp))
    
    
  }
}
legend("topleft", col=coloursForTheseModels[1:num.models], bty="n", pch=c(19,19),
       legend = simpleregionNamesForTheseModels, cex=1)



dev.off()


#----------------------------
# Writing plots to PDFs :   Panel Plot
#--------------------------

png(paste("PanelPlot_", thisRunName, ".png", sep=""), width=10, height=8,units="in",res=72)
par(oma=c(0,0,0,0),width=10,height=8)
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
    points(x=i, y=mean(unlist(group.temp)), col=coloursForTheseModels[i], pch=16)
    lines(x=rep(i, 2), y=c(max(group.temp), min(group.temp)), lwd=6, col=coloursForTheseModels.transparent[i])
  }
}

plot(x=1, y=1, col="white", axes=F, xlim=c(1, length(group.order)),
     ylim=c(ymin, ymax), xlab="", ylab="")
legend("topleft", col=coloursForTheseModels, bty="n", pch=rep(16, num.models),
       legend = simpleregionNamesForTheseModels, cex=1.2)

dev.off()


#-------------
#  Effect Size Plot
#----------

yminEffectSize <- (- 0.5)
ymaxEffectSize <- 0.5

ResponseRatioPerModel <- matrix(nrow=1,ncol = num.models)
LnResponseRatioPerModel <- matrix(nrow=1,ncol = num.models)

png(paste("EffectSizePlot_", thisRunName, ".png", sep=""), width=20, height=8.5,units="in",res=72)
par(oma=c(4,0,0,0))
par(mar=c(6.1, 5.1, 1, 2.1))

plot(x=1, y=1, col="white", axes=F, xlim=c(1, xmax),ylim=c(yminEffectSize,ymaxEffectSize), xlab="", ylab="Biomass response",cex.lab =2 )
axis(2, las=1)
text(x=x.locations-(num.models/2), y= yminEffectSize-0.1, lab=names[1:11], xpd=T, srt=25, adj=.9,cex=1.5)
lines(x=c(0.5,xmax), y=c(0,0))
abline(v=x.locations+.5)
abline(h=1)


xLocationsForBar <- (x.locations-(num.models/2))

counter=0
for(j in 1:length(group.order)) {
  for(i in 1:num.models) {
    counter=counter+1
    temp.dat <- models.list[[i]]
    group.temp <- temp.dat[1, temp.dat[rownames(temp.dat) == group.order[j],]==1]
    group.temp <- group.temp[!is.na(group.temp)]  #   *rnorm(1, 1)
    
    ResponseRatioPerModel[i] <- mean(unlist(group.temp))+1  # MUST ADD ONE BECAUSE A VALUE OF 0 FROM OTHER PLOTS MEANS NO EFFECT, WHICH IS A RATIO OF 1
    LnResponseRatioPerModel[i] <- log(mean(unlist(group.temp))+1)  # MUST ADD ONE BECAUSE A VALUE OF 0 FROM OTHER PLOTS MEANS NO EFFECT, WHICH IS A RATIO OF 1
    
    print(regionNames[i])
    print(LnResponseRatioPerModel[i])
    
    points(x= xLocationsForBar[j], y= ResponseRatioPerModel[i], col=coloursForTheseModels[i], pch=16,cex=2.5)
     # lines(x=rep(counter, 2), y=c(max(group.temp), min(group.temp)), lwd=6, col=coloursForTheseModels.transparent[i])
     
     print((ResponseRatioPerModel[i] < yminEffectSize) )
     print((ResponseRatioPerModel[i] ) )
     print(( yminEffectSize) )
     
    if (!is.na( ResponseRatioPerModel[i] ) && (ResponseRatioPerModel[i] < yminEffectSize))
    {
     text(x = counter  , y = 1.1*yminEffectSize, lab = toString(round(ResponseRatioPerModel[i],digits=1)), col=coloursForTheseModels[i],cex=1.1,srt=90 )
    }
       if (!is.na( ResponseRatioPerModel[i] ) && (ResponseRatioPerModel[i] > ymaxEffectSize))
        {
         text(x = counter  , y = 0.9*ymaxEffectSize, lab = toString( round(ResponseRatioPerModel[i],digits=1)), col=coloursForTheseModels[i],cex=1.1,srt=90 )
       }
    
  } # end loop over models 
  
    # Add square point for geometric mean
   points(x= xLocationsForBar[j], y= exp(mean(log(ResponseRatioPerModel),na.rm=TRUE)),  pch=0,col = "black",cex=3.5)
    
   #points(x= xLocationsForBar[j], y= mean(ResponseRatioPerModel,na.rm = TRUE),  pch=0,col = "black",cex=3.5)
   
   geommeanResponseOverModels[j] <- exp(mean(log(ResponseRatioPerModel),na.rm=TRUE))
   
  
} # end loopover groups

legend("topleft", col=coloursForTheseModels[1:num.models], bty="n", pch=c(19,19),
       legend = simpleregionNamesForTheseModels, cex=1)

dev.off()

  
#   meanLnResponseRatioAllModels <- mean(LnResponseRatioPerModel,na.rm=TRUE)
#   points(x=counter, y=  meanLnResponseRatioAllModels, pch=16)
#   #lines(x=rep(j, 2), y=c(max(LnResponseRatioPerModel,na.rm=TRUE), min(LnResponseRatioPerModel,na.rm=TRUE)), lwd=6, col=col.trans[i])



#Isaac output text for Gavin


write.csv(meanResponsePerModel, file = paste("meanResponsePerModel_", thisRunName, ".csv", sep=""))   
write.csv(cvResponsePerModel, file = paste("cvResponsePerModel_", thisRunName, ".csv", sep=""))   
write.csv(geommeanResponseOverModels, file = paste("geommeanResponsePerModel_", thisRunName, ".csv", sep=""))   





} # end function
