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

    #------------------------------------
  #start building indicator data frame
  ind <- data.frame(Time=sort(unique(results$Time)))
   #--------------
   #Initialize indicators as NA, especially useful in cases where indicators cant be calcualted because some charatacteristic applies to NO species in the model. 
   # For instance No species is assessed, so indicators with IsAssessed in denominator will not be possible to calculate: 
   
    ind$Totbio <- NA
    ind$Totcat <- NA
    ind$Exprate <- NA
    ind$Fishbio <- NA
    ind$Dempelfish <- NA
    ind$Dempel <- NA
    ind$Bird <- NA
    ind$Mammal <- NA
    ind$Teps <- NA
    ind$Demcat <- NA
    ind$Pelcat <- NA
    ind$Mtlbio <- NA
    ind$Mtlcat <- NA
    ind$Bio_pp <- NA
    ind$Dembio_pp <- NA
    ind$Pelbio_pp <- NA
    ind$Cat_pp <- NA
    ind$Demcat_pp <- NA
    ind$Pelcat_pp <- NA
    ind$Fishcat <- NA
    ind$Fish_exprate <- NA
    ind$Predfish_prop <- NA
    ind$Prop_of <- NA
    ind$Value <- NA
    ind$Prop_belowtarget <- NA  # Isaac added
    ind$Foragefish <- NA  # Isaac added
    #ind$Sardine <- NA  # Isaac added
    ind$Prop_notoverfished <- NA
 
  #----------------
 # Now fill in indicators
 
 

 
 
 ind$Totbio <- tapply(results$Biomass,results$Time,sum,na.rm=TRUE)
  ind$Totcat <- tapply(results$Catch,results$Time,sum,na.rm=TRUE)
  
 if ( sum(lookup$IsTarget) > 0)
{ 
  ind$Exprate <- ind$Totcat/tapply(results$Biomass[results$IsTarget==1],results$Time[results$IsTarget==1],sum,na.rm=TRUE)
}  

  
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
                      
 # ind$Sardine <- rep(0,nrow(ind))
 # if (length(which(results$IsSardine==1))>0)
 #     ind$Sardine <- tapply(results$Biomass[results$IsSardine==1],results$Time[results$IsSardine==1],sum,na.rm=TRUE)

  ind$Foragefish <- rep(0,nrow(ind))
  if (length(which(results$IsForageFish==1))>0)
   ind$Foragefish <- tapply(results$Biomass[results$IsForageFish==1],
                      results$Time[results$IsForageFish==1],sum,na.rm=TRUE)


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
  
  if(ind$Fishbio == 0)
  {
   stop("Looks like Fishbio == 0, do you really have no fish in your model?")
  }
  
  #----------
 
 
 #print("restuls$TrophicLevel IS : ")
 #print(results)
  
  
 #if (length(which(results$TrophicLevel>=3.5))>0)
 #  {
 #   ind$Predfish_prop <- tapply(results$Biomass[results$TrophicLevel>=3.5  ],results$Time[results$TrophicLevel>=3.5],sum,na.rm=TRUE)/ind$FishBio
 # }
 # print("Predfish_prop  IS THIS: " )
 # print(ind$Predfish_prop)
  
  #----------
  
  
if (length(which(results$TrophicLevel>=3.5))>0)
{
  ind$Predfish_prop <- tapply(results$Biomass[results$TrophicLevel>=3.5 & results$IsFish==1],
    results$Time[results$TrophicLevel>=3.5 & results$IsFish==1],
    sum,na.rm=TRUE)/ind$Fishbio
}

  
  #old
  
  #ind$Predfish_prop <- tapply(results$Biomass[results$IsPredatoryFish==1],
  #results$Time[results$IsPredatoryFish==1],
  #sum,na.rm=TRUE)/ind$Fishbio
  
  #-------------
  results$Depletion <- results$Biomass / results$Bzero
  results$IsBelowTarget <- rep(0,nrow(results))
  results$IsOverfished <- rep(0,nrow(results))
  print("deplteion then btarget")
  print(results$Depletion )  # 0.85, 0.99, etc.
  print(results$Btarget )  # 0.6 etc. 
  results$IsBelowTarget[results$Depletion< 0.4] <- 1   # Feb 20 was results$Btarget
  results$IsOverfished[results$Depletion<(0.5*results$Btarget)] <- 1 
  
 # print(sum(lookup$IsAssessedByFisheriesAssessment))
 #print( tapply(results$IsOverfished[results$IsAssessedByFisheriesAssessment==1],
 #                      results$Time[results$IsAssessedByFisheriesAssessment==1],sum,na.rm=TRUE) )
 
 
print(  " sum(lookup$IsAssessedByFisheriesAssessment) ")
print(sum(lookup$IsAssessedByFisheriesAssessment) )

if ( sum(lookup$IsAssessedByFisheriesAssessment) > 0)
{
  ind$Prop_of <- tapply(results$IsOverfished[results$IsAssessedByFisheriesAssessment==1],
                       results$Time[results$IsAssessedByFisheriesAssessment==1],sum,na.rm=TRUE) /
                      sum(lookup$IsAssessedByFisheriesAssessment)
              print("ind$Prop_of is  ")
              print(ind$Prop_of)
}

# Isaac added: 
ind$Prop_belowtarget <-  tapply(results$IsBelowTarget[results$IsFish==1],
                       results$Time[results$IsFish==1],sum,na.rm=TRUE) /
                      sum(lookup$IsFish)   # Feb 20 was sum(results$IsBelowTarget)/length( results$IsBelowTarget)


 
  ind$Value <- tapply(results$Catch*results$USDollarsPerTon, results$Time,
                      sum, na.rm=TRUE)
  
 
# Isaac added
ind$Prop_abovetarget <- (1- ind$Prop_belowtarget)

 #return data frame with time series of indicators
  return(ind)  
  
}
#example using Isaac's example lookup table with an old NEUS text output
#lookupfile <- file.path("~","Atlantis","r4atlantis","common_scenarios_analysis","CalCu_BasicInfo.csv")
#lookup <- read_lookup(lookupfile)
#full_bio <- read.table('~/Dropbox/ATLANTIS/Scenarios/Levels\ of\ Tuning/Base\ Effort/neusDynEffort_Base_Effort_BiomIndx.txt',header=TRUE)
#bio <- full_bio[1:2,1:38]
#full_cat <- read.table('~/Dropbox/ATLANTIS/Scenarios/Levels\ of\ Tuning/Base\ Effort/neusDynEffort_Base_Effort_Catch.txt',header=TRUE)
#cat <- full_cat[1:2,1:38]
#indicators <- get_indicators(bio,cat,lookup)

  
  




#NOT USED: L_infinity
#NOT USED: MaxAge
#NOT USED: Intrinsic Vulnerability Index ("vulnerability" from Fishbase)



