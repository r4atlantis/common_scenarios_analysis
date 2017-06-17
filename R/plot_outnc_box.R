###########################################################################
## Code to extract biomasses by box for each functional group from the
## output.nc file
###########################################################################

#' @author Emma Hodgson
# At the end of this code the function is called automatically -- so just
# make sure that the working directory (below) is set correctly to a folder
# that contains the files sourced in, and also the output.nc and biomindx.txt

# This is modified from code I received from both Isaac and Kristin

PlotOutNC_NandRNandNums_PerBox <- function(avgOverYrs, makePlot=F,
  dir = "~/AtlantisLibraries/Model_Plotting/SpatialPlots/pH2013_AllOn_mort0.1"){
#---------------
# Inputs include:
# runName -- this is something like "base2013" as it will name the csv that is made
# avgOverYrs -- tells R how many years at the end of the simulation to average biomass

# First set the working directory -- to the place with the nc files you want to
# read in and where you want to spit out the outputs
setwd(dir)

library(ncdf)
library(sm)  # for pause()
library(reshape2) # for melt() -- to make 3D array into 2D for csv export
library(plyr) # for col sums by region -- ddply

#---------------
# Puling in necessary files from the working directory
FuncGroupNamesInPlotOrder<-read.table("FuncGroupNamesInPlotOrder.csv",as.is = TRUE,header=TRUE,sep=",") # For the Biomass vs. Time plots here. Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank.
AssessOrSurveyData<-read.table("AssessOrSurveyData.csv",as.is = TRUE,header=TRUE,sep=",") # Assessment or survey data. Column 1 is year, other cols are MT biomass per year
VertNamesInPlotOrder<-read.table("VertNamesInPlotOrder.csv",as.is = TRUE,header=FALSE,col.names=c("NetCDFVertName","commonName"),sep=",") # For the rN vs. time plots below. It should have # rows=10*numVertebrates (one per age class). Any order is ok, but that order will specify plot order. "NA" will leave a subplot blank.
print('***** OPENING THE NETCDF FILE CAN TAKE 5 MINUTES ***** ')

ThisNC.nc<-open.ncdf("outputCCV3.nc") #EmoccOut169HistoricalIdentical EmoccOutHistoricalIdentical F2MSY EmoccOutF2MSY.nc EmoccOut169longer EmoccOutRun1  EmoccOutSept29a.nc NewCodeStatQuoMortC 18quadB.nc   EmoccOutSept29aItQStatusQuoLowRec.nc Oct3b EmoccOutOct3bPart EmoccOutOct2a  moccOutOct1aPart EmoccOutSept29a EmoccOutSept28a EmoccOut40yrSpinupNoFish.nc EmoccOutHydro3.nc") #) EmoccOutAug28.nc")  #AMS: amsFCoutput12Mar.nc "EmoccOutJan30bNoFish.nc EmoccOutFeb2BF.nc EmoccOutJan24a.nc Open NetCDF file
save(ThisNC.nc,file="outputCCV3.Rdata")

#---------------
# Getting first set of info from the nc file
volumeData <- get.var.ncdf( ThisNC.nc,"volume") # extract the data from the variable. The variable contains lots of other metainfo like units, name, etc.
volDims<-dim(volumeData)  # Just use volume to see how many time steps are in the data

## NOT SURE IF THE BELOW CALLS ARE NEEDED
volBottomCells<-volumeData[volDims[1],,] # Because dz (height) of lowest boxes is 1m,I can say that Area is equal to the volume of the lowest boxes
dz <- get.var.ncdf( ThisNC.nc,"dz")
numDepths<-dim(dz)[1]
zBottomCell<- dz[numDepths,1,1]
areaData<-volBottomCells/zBottomCell

#---------------
# Determining dimensions
numTimeSteps<-volDims[3]
rowscols<-dim(FuncGroupNamesInPlotOrder) # count how many species are in this data
numFuncGroups<-rowscols[1]
numBoxes <- volDims[2]

startYrAvg <- numTimeSteps-avgOverYrs
endYrAvg <- numTimeSteps

#---------------
# Regions info -- created by me, not elsewhere
region.vec <- c("7_Outside", rep("1_VIs", 12), rep("2_WA", 6), rep("3_OR_NCA", 12),
                rep("4_CenCA", 12), rep("5_SCA", 19), rep("6_Mexico", 13),
                rep("7_Outside", 14))
numRegions <- length(unique(region.vec))

#---------------
# Create blank matrix that will be filled in
outputMat <- matrix(nrow=numBoxes, ncol=numFuncGroups+1)
outputMat[,1] <- c(0:88)
outputArray <- array(dim=c(numBoxes, numTimeSteps, numFuncGroups))
regionArray <- array(dim=c(numRegions, numTimeSteps, numFuncGroups))

#---------------
# Do the looping -- by box and by species functional grou
# averaging over the last "avOverYrs" of the dataset
# Don't need to loop over boxes! Do all at once, like Kristin's code

for (funcGroup in 1:numFuncGroups) {
    #print(FuncGroupNamesInPlotOrder$commonName[funcGroup])

    # Now extract biomass time series for the functional group and box,
    # method depends on the type of functional group.
    if (!is.na(FuncGroupNamesInPlotOrder$NetCDFName[funcGroup]))
    {
      thisData <- get.var.ncdf( ThisNC.nc,FuncGroupNamesInPlotOrder$NetCDFName[funcGroup])  # Extract data from variable
      # IF WE HAVE A 2D SPECIES LIKE AN INVERT....
      if (length(dim(thisData))==2)
      {
        thisY<-(thisData*areaData)*(5.7*20/10^9)
        #yLabString<-'Biomass, metric tons'
      }
      # IF WE HAVE A 3D THING THAT IS NH3, N03, Chla, Carrion, we leave it in mg N/m^3
      if( FuncGroupNamesInPlotOrder$NetCDFName[funcGroup]=='NH3'| FuncGroupNamesInPlotOrder[funcGroup,1]=='NO3'| FuncGroupNamesInPlotOrder[funcGroup,1]=='Chl_a' | FuncGroupNamesInPlotOrder[funcGroup,1]=='Carrion_N' | FuncGroupNamesInPlotOrder[funcGroup,1]=='Si')
      {
        thisY<-apply(thisData*volumeData,c(2,3),sum)/apply(volumeData,c(2,3),sum)
        #yLabString<-'mg N/m^3'
      }
      # IF WE HAVE A 3D SPECIES LIKE A FISH (not Nuts,Chla, or Carrion)...
      if( length(dim(thisData))==3 & (FuncGroupNamesInPlotOrder$IsVertebrate[funcGroup]==1)   )
      {
        #thisY<-apply(thisData[,boxesToUse,]*volumeData[,boxesToUse,],3,sum)  #*(5.7*20/10^9) NOTE RECENT VERSION OF ATLANTIS HAVE VERTEBRTES_N AS WET WEIGH MT/M^3

        # the below wants a '2' not a '3' in the apply() function
        thisY<-apply(thisData*volumeData,c(2,3),sum)
        #thisY<-apply(thisData[,boxesToUse,]*volumeData[,boxesToUse,],2,sum)  #*(5.7*20/10^9) NOTE RECENT VERSION OF ATLANTIS HAVE VERTEBRTES_N AS WET WEIGH MT/M^3
        #yLabString<-'Biomass, metric tons'
        print(FuncGroupNamesInPlotOrder$NetCDFName[funcGroup])
      }
      # IF WE HAVE 3D THING THAT IS LIKE PLANTKON, NOT VERTEBRATE
      if( length(dim(thisData))==3 & !(FuncGroupNamesInPlotOrder$IsVertebrate[funcGroup]==1) &  !FuncGroupNamesInPlotOrder$NetCDFName[funcGroup]=='NH3' &  !FuncGroupNamesInPlotOrder[funcGroup,1]=='NO3' &  !FuncGroupNamesInPlotOrder[funcGroup,1]=='Chl_a' &  !FuncGroupNamesInPlotOrder[funcGroup,1]=='Carrion_N' & ! FuncGroupNamesInPlotOrder[funcGroup,1]=='Si' )
      {
        #thisY<-apply(thisData[,boxesToUse,]*volumeData[,boxesToUse,],3,sum)*(5.7*20/10^9) #NOTE RECENT VERSION OF ATLANTIS HAVE VERTEBRTES_N AS WET WEIGH MT/M^3

        # the below wants a '2' not a '3' in the apply() function
        thisY<-apply(thisData*volumeData,c(2,3),sum)*(5.7*20/10^9) #NOTE RECENT VERSION OF ATLANTIS HAVE VERTEBRTES_N AS WET WEIGH MT/M^3
        #yLabString<-'Biomass, metric tons'
        print(FuncGroupNamesInPlotOrder$NetCDFName[funcGroup])
      }
    }
    # Average over the number years of interest:
    BiomassSubset <- thisY[,(startYrAvg+1):endYrAvg]
    BiomassAvg <- apply(BiomassSubset,1,mean)
    outputMat[,funcGroup+1] <- BiomassAvg
    outputArray[,,funcGroup] <- thisY

    temp.data <- data.frame(thisY, region.vec)

    # Calculating by region
    region.time <- ddply(.data=temp.data[,1:101],
                         .variables='region.vec', .fun=colSums)
    regionArray[,,funcGroup] <- as.matrix(region.time[,-1])
}


#--------------
# Make 3D array outputs into 2D format to export -- using reshape2 package
outputTimeSpace <- melt(outputArray)
colnames(outputTimeSpace) <- c("Box", "TimeStep", "FuncGroup", "Value")

regionTimeSpace <- melt(regionArray)
colnames(outputTimeSpace) <- c("Region", "TimeStep", "FuncGroup", "Value")

#--------------
# Calculate BiomassAveraged by region
outputDataFrame <- data.frame(outputMat, region.vec)
regionAveraged <- ddply(.data=outputDataFrame[,2:92],
                         .variables='region.vec', .fun=colSums)

#--------------
# Calculating initial biomasses -- but really they should be the same across
# all runs
initialBiomass <- outputArray[,1,]
colnames(initialBiomass) <- FuncGroupNamesInPlotOrder$CODE
initialMat <- data.frame(box=c(0:88), initialBiomass, region.vec)
initialAveraged <- ddply(.data=initialMat[,2:92],
                        .variables='region.vec', .fun=colSums)

###############################################################################
###############################################################################
### Doing some comparisons between box outputs and the BiomIndx.txt file
### since I am worried they do not really match up!
###############################################################################
###############################################################################
initialBiomass <- outputArray[-c(1,76:89),1,]
colnames(initialBiomass) <- FuncGroupNamesInPlotOrder$CODE
yearOne <- outputArray[-c(1,76:89),2,]
colnames(yearOne) <- FuncGroupNamesInPlotOrder$CODE

# vector of biomasses in years zero and one from the output.nc file
nc.year.zero <- apply(initialBiomass, 2, sum)[1:87]
nc.year.one <- apply(yearOne, 2, sum)[1:87]

# vectors of biomasses in years zero and one from the biomindx.txt file
txt.bio <- read.table("outputCCV3BiomIndx.txt", sep=" ", header=T)
txt.year.zero <- txt.bio[1,2:87]
txt.year.one <- txt.bio[2,2:87]

# fixing the double printing of PWN as PWN1 and PWN2, so that the txt and nc
# vectors are directly comparable
nc.year.zero.final <- c(nc.year.zero[1:60], sum(nc.year.zero[61:62]), nc.year.zero[63:87])
nc.year.one.final <- c(nc.year.one[1:60], sum(nc.year.one[61:62]), nc.year.one[63:87])

compare <- rbind(txt.year.zero, nc.year.zero.final, txt.year.one, nc.year.one.final)
rownames(compare) <- c("BiomIndx_yearZERO", "NC_yearZERO", "BiomIndx_yearONE", "NC_yearONE")
write.csv(compare, "Comparison_BiomIndxAndoutputNC.csv")

###############################################################################
###############################################################################

#--------------
# Col names for the matrices
colnames(outputMat) <- c("Box", FuncGroupNamesInPlotOrder$CODE)
colnames(regionAveraged) <- c("Region", FuncGroupNamesInPlotOrder$CODE)

#--------------
# Write the output to CSV
write.csv(outputMat, file="BoxBiomass_timeAveraged.csv")
write.csv(outputTimeSpace, file="BoxBiomass_timeAll.csv")
write.csv(regionTimeSpace, file="RegionBiomass_timeAll.csv")
write.csv(regionAveraged, file="RegionBiomass_timeAveraged.csv")
write.csv(initialMat, file="BoxBiomass_timeZero.csv")
write.csv(initialAveraged, file="RegionalBiomass_timeZero.csv")

#--------------
# This lists all of the variables in the NC file and makes a csv file -- for reference
numVarsInNC<-length(ThisNC.nc$var)
ListOfVarsFromNC<-matrix(numVarsInNC,1)

for (groupIndex in 1:numVarsInNC )
{
  thisVar <- ThisNC.nc$var[[groupIndex]]
  #print(thisVar$name)
  ListOfVarsFromNC[groupIndex]<-thisVar$name
}
write.csv(ListOfVarsFromNC,file="ListOfVarsFromNC.csv")


#--------------
# PLOTTING
# makes pdfs of different plots -- biomass by box (for each functional group)
# and biomass by region (to make it less messy)

  if(makePlot==T) {
    print("**Making Figures**")
    # making onshore to offshore diff colours, below is if only 74 dimensions
    boxColor <- c(rep(1:6, 8), 1,2,3, rep(4, 9), 6, 1:6, 1:6, 4)
    # boxColors if all boxes are included:
    boxColor <- c(7, rep(1:6, 8), 1,2,3, rep(4, 9), 6, 1:6, 1:6, 4, rep(7,14))
    #cols <- c("#8B1A1A", "#CD6600", "#EEB422", "#228B22", "#104E8B", "#68228B", "black")
    cols <- c("red4", "orangered3", "orange3", "seagreen4", "royalblue3", "palevioletred4", "black")

    pdf("BiomassTime_byBox.pdf", width=8.5, height=11)
    par(mfrow=c(3,2))
    for(i in 1:numFuncGroups) {
      plot(x=1, y=1, xlim=c(0,numTimeSteps), ylim=c(min(outputArray[,,i]), max(outputArray[,,i])),
           col="white", xlab="TimeStep", ylab="Biomass", main=FuncGroupNamesInPlotOrder$commonName[i])
      for(j in 1:numBoxes) {
        lines(x=c(0:(numTimeSteps-1)), y=outputArray[j,,i], col=cols[boxColor[j]])
      }
      legend("topleft", col=cols[c(1,6)], lty=c(1,7), lwd=1.5,
             legend=c("Coastal box", "Offshore box"))
    }
    dev.off()

    pdf("BiomassTime_byRegion.pdf", width=8.5, height=11)
    par(mfrow=c(3,2))
    for(i in 1:numFuncGroups) {
      plot(x=1, y=1, xlim=c(0,numTimeSteps), ylim=c(min(regionArray[,-1,i]), max(regionArray[,-1,i])),
           col="white", xlab="TimeStep", ylab="Biomass", main=FuncGroupNamesInPlotOrder$commonName[i])
      for(j in 1:numRegions) {
        lines(x=c(0:(numTimeSteps-1)), y=regionArray[j,,i], col=cols[j])
      }
      legend("topleft", col=cols, lty=c(1,7), lwd=1.5,
             legend=c("Van Is", "WA", "OR & NCA", "Cent CA", "SCA", "Mexico", "Outside"))
    }
    dev.off()
  }

close.ncdf(ThisNC.nc)

} #END OF FUNCTION



# Run the function
# PlotOutNC_NandRNandNums_PerBox(avgOverYrs=10, makePlot=T)



