#' Calculate ecosystem indicators based on Atlantis biomass and catch text file output
#'
#' Takes time series of biomass and catch (by species) and returns a 
#' list of ecosystem indicators, given flags for species groups as detailed
#' in the return from \code{get_lookup()}.

#' @author Gavin Fay


#' @param bio A dataframe of biomass time series, such as that 
#' obtained from reading in \code{BiomIndx.txt}.
#' @param cat A dataframe of catch time series, such as that 
#' obtained from reading in \code{Catch.txt}. 
#' @param lookup A list of flags and summary stats by species. 
#' This is the list returned from \code{get_lookup()}.
#' 
#' @return Returns a list of time series of ecosystem indicators.
#'
#' @examples
#' #example using Isaac's example lookup table with an old NEUS text output
#' lookupfile <- file.path("~","Atlantis","r4atlantis","common_scenarios_analysis","CalCu_BasicInfo.csv")
#' lookup <- read_lookup(lookupfile)
#' full_bio <- read.table('~/Dropbox/ATLANTIS/Scenarios/Levels\ of\ Tuning/Base\ Effort/neusDynEffort_Base_Effort_BiomIndx.txt',header=TRUE)
#' bio <- full_bio[1:2,1:38]
#' full_cat <- read.table('~/Dropbox/ATLANTIS/Scenarios/Levels\ of\ Tuning/Base\ Effort/neusDynEffort_Base_Effort_Catch.txt',header=TRUE)
#' cat <- full_cat[1:2,1:38]
#' indicators <- get_indicators(bio,cat,lookup)

get_indicators <- function(bio,cat,lookup)
{
  
  #turn biomass and catch data into long form
  bio_use <- reshape(bio,direction="long",varying=list(2:ncol(bio)),idvar="Time")
  bio_use$time <- names(bio[,-1])[bio_use$time]    
  rownames(bio_use) <- NULL
  names(bio_use) <- c("Time","Code","Biomass")

  cat_use <- reshape(cat,direction="long",varying=list(2:ncol(cat)),idvar="Time")
  cat_use$time <- names(cat[,-1])[cat_use$time]    
  rownames(cat_use) <- NULL
  names(cat_use) <- c("Time","Code","Catch")
  
  #merge dataframes
  results <- merge(bio_use,cat_use,by=c("Time","Code"),all.x=TRUE)
  results <- merge(results,lookup,by.x="Code",by.y="Atlantis species code",all.x=TRUE)
  #make some variables easier to reference
  names(results)[7] <- "Bzero"
  names(results)[8] <- "Btarget"
  
  #isfish <- 2:20
  #isTEP <- c(19,21:25)
  #isBio <- c(2:28,30:32)
  #istargBio <- c(2:20,26:28,30:32)
  #isCatch <- isBio
  #isDemBio <- c(4,6,10,12:20,27:28,30:31)
  #isPelBio <- c(2,3,4,7:9,11,19)
  #isDemBio <- c(4,6,10,12:18,20,27:28,30:31)
  #isPelBio <- c(2,3,4,7:9,11,19,26,32)
  #isDemCatch <- isDemBio
  #isPelCatch <- isPelBio
  #isPP <- 37:39
  #isPelFish <- c(2,3,4,7:9,11,19)
  #isDemFish <- c(4,6,10,12:20)
  
  #if(Sys.info()[['sysname']]=="Linux") 
  #  TL <- read.csv("/media/My\ Passport/NEFSC/ATLANTIS/2013/OA/Trophiclevels.csv",header=TRUE)
  #else
  #  TL <- read.csv("J:/NEFSC/ATLANTIS/2013/OA/trophiclevels.csv",header=TRUE)  
  #trophic.levels <- read.csv("H:/NEFSC/ATLANTIS/2013/OA/compare_trophiclevels.csv")
  #trophic.levels <- read.csv("/Volumes/MyPassport/NEFSC/ATLANTIS/2013/OA/compare_trophiclevels.csv")
  #trophic.levels <- read.csv(paste(path,"/compare_trophiclevels.csv",sep="")
  
  #TL <- trophic.levels$New.ATL.TL

  #start building indicator data frame
  ind <- data.frame(Time=sort(unique(results$Time)))
  ind$Totbio <- tapply(results$Biomass,results$Time,sum,na.rm=TRUE)
  ind$Totcat <- tapply(results$Catch,results$Time,sum,na.rm=TRUE)
  ind$Exprate <- ind$Totcat/tapply(results$Biomass[results$IsTarget==1],results$Time[results$IsTarget==1],sum,na.rm=TRUE)
  ind$Fishbio <- tapply(results$Biomass[results$IsFish==1],results$Time[results$IsFish==1],sum,na.rm=TRUE)
  ind$Dempelfish <- tapply(results$Biomass[results$IsFish==1 & results$IsDemersal==1],
                           results$Time[results$IsFish==1 & results$IsDemersal==1],
                           sum,na.rm=TRUE)/
    tapply(results$Biomass[results$IsFish==1 & results$IsPelagic==1],
              results$Time[results$IsFish==1 & results$IsPelagic==1],sum,na.rm=TRUE)
  ind$Dempel <- tapply(results$Biomass[results$IsDemersal==1],
                           results$Time[results$IsDemersal==1],
                           sum,na.rm=TRUE)/
    tapply(results$Biomass[results$IsPelagic==1],
           results$Time[results$IsPelagic==1],sum,na.rm=TRUE)
  
  ind$Bird <- rep(0,nrow(ind))
  if (length(which(results$IsBird==1))>0)
   ind$Bird <- tapply(results$Biomass[results$IsBird==1],
                      results$Time[results$IsBird==1],sum,na.rm=TRUE)

  ind$Mammal <- rep(0,nrow(ind))
  if (length(which(results$IsMammal==1))>0)
   ind$Mammal <- tapply(results$Biomass[results$IsMammal==1],
                        results$Time[results$IsMammal==1],sum,na.rm=TRUE)
  
  # might need some additional customization here, perhaps a new flag, 
  # some will have some shark species or possibly even endangered fish
  ind$Teps <- ind$Bird+ind$Mammal
  if (length(which(results$Code=='REP'))>0)
    ind$Teps <- ind$Teps + tapply(results$Biomass[results$Code=='REP'],
                                  results$Time[results$Code=='REP'],sum,na.rm=TRUE)
  
  ind$Demcat <- tapply(results$Catch[results$IsDemersal==1],
                       results$Time[results$IsDemersal==1],sum,na.rm=TRUE)
  
  ind$Pelcat <- tapply(results$Catch[results$IsPelagic==1],
                       results$Time[results$IsPelagic==1],sum,na.rm=TRUE)
  
  ind$Mtlbio <- tapply(results$Biomass*results$TrophicLevel,
                       results$Time,sum,na.rm=TRUE)/ind$Totbio

  ind$Mtlcat <- tapply(results$Catch*results$TrophicLevel,
                       results$Time,sum,na.rm=TRUE)/ind$Totcat
  
  ind$Bio_pp <- ind$Totbio/tapply(results$Biomass[results$IsPrimaryProducer==1],
                       results$Time[results$IsPrimaryProducer==1],sum,na.rm=TRUE)
  
  ind$Dembio_pp <- tapply(results$Biomass[results$IsDemersal==1],
                       results$Time[results$IsDemersal==1],sum,na.rm=TRUE)/
                tapply(results$Biomass[results$IsPrimaryProducer==1],
                       results$Time[results$IsPrimaryProducer==1],sum,na.rm=TRUE)
  
  ind$Pelbio_pp <- tapply(results$Biomass[results$IsPelagic==1],
                          results$Time[results$IsPelagic==1],sum,na.rm=TRUE)/
    tapply(results$Biomass[results$IsPrimaryProducer==1],
           results$Time[results$IsPrimaryProducer==1],sum,na.rm=TRUE)
  
  ind$Cat_pp <- ind$Totcat/tapply(results$Biomass[results$IsPrimaryProducer==1],
                               results$Time[results$IsPrimaryProducer==1],sum,na.rm=TRUE)
  
  ind$Demcat_pp <- ind$Demcat/tapply(results$Biomass[results$IsPrimaryProducer==1],
                                  results$Time[results$IsPrimaryProducer==1],sum,na.rm=TRUE)
  
  ind$Pelcat_pp <- ind$Pelcat/tapply(results$Biomass[results$IsPrimaryProducer==1],
                                  results$Time[results$IsPrimaryProducer==1],sum,na.rm=TRUE)
  
  ind$Fishcat <- tapply(results$Catch[results$IsFish==1],results$Time[results$IsFish==1],
                        sum,na.rm=TRUE)
  
  ind$Fish_exprate <- ind$Fishcat / ind$Fishbio
  
  ind$Predfish_prop <- tapply(results$Biomass[results$IsPredatoryFish==1],
                              results$Time[results$IsPredatoryFish==1],
                              sum,na.rm=TRUE)/ind$Fishbio
  
  results$Depletion <- results$Biomass / results$Bzero
  results$IsBelowTarget <- rep(0,nrow(results))
  results$IsOverfished <- rep(0,nrow(results))
  results$IsBelowTarget[results$Depletion<results$Btarget] <- 1 
  results$IsOverfished[results$Depletion<(0.5*results$Btarget)] <- 1 
  
  ind$Prop_of <- tapply(results$IsOverfished[results$IsAssessedByFisheriesAssessment==1],
                       results$Time[results$IsAssessedByFisheriesAssessment==1],sum,na.rm=TRUE) /
                 sum(lookup$IsAssessedByFisheriesAssessment)
  
  ind$Value <- tapply(results$Catch*results$USDollarsPerTon, results$Time,
                      sum, na.rm=TRUE)
  
  #return data frame with time series of indicators
  return(ind)
  
}
#example using Isaac's example lookup table with an old NEUS text output
lookupfile <- file.path("~","Atlantis","r4atlantis","common_scenarios_analysis","CalCu_BasicInfo.csv")
lookup <- read_lookup(lookupfile)
full_bio <- read.table('~/Dropbox/ATLANTIS/Scenarios/Levels\ of\ Tuning/Base\ Effort/neusDynEffort_Base_Effort_BiomIndx.txt',header=TRUE)
bio <- full_bio[1:2,1:38]
full_cat <- read.table('~/Dropbox/ATLANTIS/Scenarios/Levels\ of\ Tuning/Base\ Effort/neusDynEffort_Base_Effort_Catch.txt',header=TRUE)
cat <- full_cat[1:2,1:38]
indicators <- get_indicators(bio,cat,lookup)

  
  



