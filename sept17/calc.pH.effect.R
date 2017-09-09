## ============================================================================
## Functions for making outputs useful from Atlantis
## July 24, 2015 Emma Hodgson.
## Modified Feb 2016 by Isaac.Kaplan@noaa.gov to allow differnt file name and directory structure
#  THIS IS JUST A HELPER FUNCTION FOR SCRIPT PlotGuildBiomassOneScenarioVsBase.r
## ============================================================================

# -----------------------------------------------------------------------------
# Calculating the effect of pH on biomass and catch
# for whatever run is specified (specified by the working directory)
# -----------------------------------------------------------------------------
calc.pH.effect <- function(baseOutBiomIndxFileName, scenarioOutBiomIndxFileName, yr.start = 40, yr.end = 49, scenarioNameString="GiveName", CSV.file.name = "CalCu_BasicInfo.csv", ifplot=F)

{
  # BIOMASS
  # read in all of the text files
  base <- read.delim(baseOutBiomIndxFileName,header=T, sep=" ")
  scenario <- read.delim(scenarioOutBiomIndxFileName,header=T, sep=" ")

  guild.dat <- read.csv(CSV.file.name, skip=14, header=F, row.names=1) # read guild data in here becuase use it to identify which colss of BiomIndx we want.
  numColsToUseInBiomIndx <-dim(guild.dat)[2] # count number of cols in guild data, which is num of cols to use in BiomIndx.

  base.sub <- base[1:numColsToUseInBiomIndx]
  scenario.sub <- scenario[1:numColsToUseInBiomIndx]

#-----------------------
  # Make sure all time columns begin with 0. This is to deal with NE USA Fixed F; it is applied to all other models but shouldnt change them. 

base.sub[,1] <- base.sub[,1] - base.sub[1,1] 
scenario.sub[,1] <- scenario.sub[,1]- scenario.sub[1,1]


  #---------------
  # CHECK NUMBER OF YEARS TO MAKE SURE NOT TOO SHORT

  #--------------
  # take averages over the sets of years desired -- specified by yearA and yearB
  max.years <- base.sub[nrow(base.sub),1]/365

  if(max.years < yr.end) stop("Year range to average over exceeds years in output")
  #------------

  #--------
  # NOW DEAL WITH PROBLEM THAT NOT ALL MODELS HAVE 365 DAY OUTPUTS. NEED TO FIND ROWS CORRESPODNGING TO RIGHT YEARS.
  #--------
  row.start.base <- min(which(base.sub[,1]/365>yr.start))
  row.end.base <- min(which(base.sub[,1]/365>yr.end))

  row.start.scenario <- min(which(scenario.sub[,1]/365>yr.start))
  row.end.scenario <- min(which(scenario.sub[,1]/365>yr.end))

  #--------------
  # AVERAGE OVER YEARS 40-49 OR WHATEVER IS APPROPRIATE

  # biomass
  base.vec <- apply(base.sub[row.start.base:row.end.base, ], 2, mean)
  scenario.vec <- apply(scenario.sub[row.start.scenario:row.end.scenario, ], 2, mean)

  scenario.effect <- (scenario.vec - base.vec) / base.vec

  # Check if the average value over the set years has a biomass < 0.02 the initial
  # value. If so, then we will exclude this functional group from the analysis

  for(i in 1:length(base.vec))
  {

    if(base.sub[1,i] == 0)
    {
    scenario.effect[i] <- NA
    }

    else
     {

      if(base.vec[i]/base.sub[1,i] < 0.02)
      {
      scenario.effect[i] <- NA
      }

     } #end else
    } # end for

  # EXTRA BIT on plotting the time series for visualization
  if(ifplot==T) {
    cols <- c("red", "blue")

    pdf(paste("BiomassTS", scenarioNameString,".pdf", sep=""), width=8.5, height=11)
    par(mfrow=c(3,1))
    for(i in 2:ncol(scenario.sub)) {
      ymax = max(base.sub[,i], scenario.sub[,i])
      ymin = 0
      plot(base.sub[,i], main=colnames(base.sub)[i], ylim=c(ymin, ymax),
           xlab="Year", ylab="Biomass", type="l", lwd=1, col=cols[1])
      points(scenario.sub[,i], ylim=c(ymin, ymax), xlab="Year", ylab="Biomass",
             col=cols[2], type="l")
      legend("topleft", legend=c("Base run", scenario),
             col=cols, lty=c(1,1), bty="n")
    }
    dev.off()
  }

  # Now we also want to assign each of the functional groups their guild
  #files <- list.files(base.wd)
  #file.name <- file.path(base.wd, files[which(grepl("csv", files)=="TRUE")])

  combo.dat <- rbind(scenario.effect[-1], guild.dat[,-8])
  colnames(combo.dat) <- names(scenario.effect[-1])

  return(list(scenario.effect=scenario.effect[-1], dataframe.effect=combo.dat))
}
