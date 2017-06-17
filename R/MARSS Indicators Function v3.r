# 
# Title: MARSS Indicators Function
# Author: Tyler Rothnie
# Date: Feb 24 2014
# 
# +30,000 MARSS comparisons
# 24 Hours to complete all comparison
#
#

#-----------------------------------------------------------------------
#####Known Errors
#
# 1. No problem because it returns NA values for low/upCI values which we
#     handle
# Error in solve.default(MLEobj.hessian$Hessian) : 
# Lapack routine dgesv: system is exactly singular: U[3,3] = 0
# In addition: Warning message:
#   In MARSShessian(MLEobj) :
#   MARSShessian: Hessian could not be inverted to compute the parameter var-cov matrix
#
# 2.
#
#-----------------------------------------------------------------------



#----------------------------------------------------------------------------
#DATA STRUCTURE
#-----------------------------------------------------------------------------
# Outer Structure - List
#-----------------------------------------------------------------------------
# There are two data structure used in this program
# The outer data structure is a list to incapsulate the different types
# of scenarios ex(Fishing-( fishing0pt5,fishing1pt5,fishing2,fishing2xtrawl) )
#   List Example
#   To get a the array with the list
#    pathNamesNuts=c(
#     "/Nuts5xBox3N/" ,   #1
#     "/Nuts10xBox3N/" ,  #2
#     "/NutsMajor/" ,     #3
#     "/NutsMedium/" ,    #4
#     "/NutsMinor/"       #5
#   )
#   To get list element "/Nuts10xBox3N/" array is listName[[2]]
#
# Note a Helpful command to find the order of the files in the list is names(list)
#
#-------------------------------------------------
# IMPORTANT MIX TABLES ARE A LITTLE DIFFERENT 
#-------------------------------------------------
# For the mix scenarios list the structure is a bit different ex mixlist[[1:5]][[1]]
# The mix list has a 1 extra list wrapped around the outer list
# so to access the the 3d arrary you have to go mixlist[[#]][[1]]
#
#------------------------------------------------------------------------------
# Inner Structure - 3d Array
#------------------------------------------------------------------------------
# The inner structure is a 3d array. With x = row = attributes(1-11) and 
# y = columns = indicators(1-59) and z = Data(11)
# Data z 1-3 = B values 1=1v1 2=2v1 3=2v2
# 4-6 = lowCI 4=1v1 5=2v1 6=2v2
# 7-9 = upCI 7=1v18=2v1 9=2v2
# 10 = converge - 0=converged, 1-52=Did not converge
# 11 = Error - If there is a error with this data then dont tally but avg
#   Array Example
#   To get data attribute 1 vs indicator 2 and B value 2v2 is array[1,2,3]
#   To get data attribute 3 vs indicator 41 and upCI 1v1 is array[3,41,7]
# 
#   Both structure Example
#   To get list element 3 attribute 5 indicator 11 and error list[[3]][5,11,10]
#
#------------------------------------------------------------------------




#-----------------------------------------------------------------------
#
#MAIN FUNCTION
#
# Input - indicatorFolder - Is the file path to your indicator folder on 
#                           your computer
#
# Progression - Will loop through every section of scenario and call 
#               readAnalysisWrite who will call the correct single or double
#               readAnalysisWrite function. If it is a single it will do
#               checks using checkForNA and then do work and then 
#               use exportToXlsx function to export to excel and return. 
#               If it was a double then it will slice up the data into 
#               2 new tables and usethe single table function to do the 
#               work and return.
#
#-----------------------------------------------------------------------
Mainfunction = function( 
  indicatorFolder="C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs"){
  
  # Address locations of all of the different scenarios
  # with full paths to each directory broken down by
  # type of scenario
  # combine with the pathtoindicators & filename to get 
  # the compelete address to each file added element numbers
  # to help reference when they are inside of thier list
  
  require(MARSS)
  require(xlsx)
  
  #List of files/scenarios
  #Each List contains a bunch of file
  #The list are broken down by type of
  #File/scenario
  
  pathNamesFishing=c(
    "/Fishing0pt5/" ,           #1
    "/Fishing1pt5/" ,           #2
    "/Fishing2/" ,              #3
    "/Fishing5/" ,              #4
    "/Fishing2xTrawl/" ,        #5
    "/Fishing4xDemersal/" ,     #6
    "/Fishing4xPelagic/" ,      #7
    "/Fishing10xForageKrill/" , #8
    "/FishingStatusQuo/"        #9 
  )
  
  pathNamesPulseFishing=c(
    "/FishingPulse1pt5long/" ,  #1
    "/FishingPulse1pt5short/" , #2
    "/FishingPulse4long/" ,     #3
    "/FishingPulse4short/"      #4
  )
  
  pathNamesNuts=c(
    "/Nuts5xBox3N/" ,   #1
    "/Nuts10xBox3N/" ,  #2
    "/NutsMajor/" ,     #3
    "/NutsMedium/" ,    #4
    "/NutsMinor/"       #5
  )
  
  pathNamesHotspots=c(
    "/SpatialFishing1pt5/" ,  #1
    "/SpatialFishing2/"       #2
  )
  
  pathNamesMPA=c(
    "/SpatialFishing50ptNearshore/" ,   #1
    "/SpatialFishing100pct3Regions/" ,  #2
    "/SpatialFishing100pctNMS/"         #3
  )
  
  pathNamesNutsLocal=c(
    "/Nuts5xBox3NLocal/" ,    #1
    "/Nuts10xBox3NLocal/" ,   #2
    "/NutsMajorLocal/" ,      #3
    "/NutsMediumLocal/" ,     #4
    "/NutsMinorLocal/"        #5
  )
  
  pathNamesHotspotsLocal=c(
    "/SpatialFishing1pt5Local/" ,   #1
    "/SpatialFishing2Local/"        #2
  )
  
  pathNamesMPALocal=c(
    "/SpatialFishing50ptNearshoreLocal/" ,    #1
    "/SpatialFishing100pct3RegionsLocal/" ,   #2
    "/SpatialFishing100pctNMSLocal/"          #3
  )
  
  #List of the Attributes names that are in each file
  listAttrib = c("NPPtoBiomass","meanTrophicLevelBiomass","shannonIndex",
                 "numNonAssessedBelowB40","numNonAssessedBelowB25",
                 "targetGroupBiomass","totalCatch","rockfish","NPP",
                 "totalLivingBiomass","groundfishMeanPropMature")
  
  #List of the Indicators names that are in each file
  listIndic = c("meanTrophicLevelCatch","phytoplankton","numAssessedBelowB40",
                "numAssessedBelowB25","totalCatch.1","nonCommercialSpecies",
                "bottomfish","scavengers","flatfish","benthicInverts","herbivores",
                "roundfish","rockfish.1","rockfishToFlatfish","Invertivores",
                "habitatStructure","Kelp","SeastarAbaloneUrchin","Lingcod",
                "ShallowLargeMidwaterShortbelly","DungenessCrabSeastar",
                "HalibutSmallFlatfish","ShallowLarge","ShallowSmall",
                "ShallowSmallShallowLargeMidwaterCanary","DungenessAndCrabs",                     
                "LingcodYelloweyeMidwaterLargeShallow","gelatinousZoops",
                "forageFishToJellyfish","zooplanktivorousFish","zooplankton",
                "zooplanktonToPhyto","benthicToPelTSMatrix","piscivToPlankTSMatrix",                 
                "piscivToScavengerTSMatrix","piscivores","InvertivoresToHerbivores",
                "marineMammalsAndBirds","Seabirds","marineMammals",
                "finfishToCrustacean","cetacean","sablefish","lingcodPropMature",
                "shallowLargePropMature","midwaterPropMature","shortbellyPropMature",
                "immatureSumGroundfish","immatureSumAssessed",
                "groundfishMeanWtMaturity","assessedMeanWtMaturity",
                "reefTopInvertebrates","forageFish","krill","salmon",
                "divingAndMigratoryBirds","baleenWhales","pinnipeds","seaOtters" )
  
  # this is the path from your drive to your Indicators folder that contains
  # all of you the scenarios
  # example = "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs"
  # combine with the pathtofolder & filename to get the compelete address to each file
  pathtoIndicators=indicatorFolder
  
  # The 2 different file name that are used in each folder
  # combine with the path to get the compelete address to each file
  fileName="attributeTSMatrixHeaderStandard"
  
  # Will create if needed a new folder to store all of the output files 
  # from the MARSS function
  dir.create(paste0(pathtoIndicators,"/MARSS Output/"), showWarnings= F)
  #saves your current working dir to reset later
  currentwd=getwd()
  #sets your work dir to your pathtoIndicators
  setwd(paste0(pathtoIndicators,"/MARSS Output/"))
  
  
  
  #creats lists for storing the output
  listFish= list()
  listPulseFish= list()
  listNutsFish= list()
  listMPAFish= list()
  listHotsFish= list()
  listNutsLocalFish= list()
  listMPALocalFish= list()
  listHotsLocalFish= list() 
  listNutsMixFish= list()
  listMPAMixFish= list()
  listHotsMixFish= list()
  
  
     #----------------------------------------------------------------------
     #Fishing
  
  cat("\nSection Progress Fishing:\n")
  numRuns=length(pathNamesFishing)
  for(runNum in 1:numRuns){
    
    cat(pathNamesFishing[runNum]," Progress: ",runNum," of ",numRuns)
    
    path = paste0( pathtoIndicators, pathNamesFishing[runNum], fileName)
    out = readAnalysisWrite( path ,"", listAttrib, listIndic)
    listFish[[pathNamesFishing[runNum]]] = out
    cat("\nComplete \n \n")
  }
  
    
    #PulseFishing
    cat("\nSection Progress Pulse Fishing:\n")
    numRuns=length(pathNamesPulseFishing)
    for (runNum in 1:numRuns){
      
      cat(pathNamesPulseFishing[runNum]," Progress: ",runNum," of ",numRuns)
      
      path = paste0( pathtoIndicators, pathNamesPulseFishing[runNum], fileName)
      out = readAnalysisWrite( path ,"", listAttrib, listIndic)
      listPulseFish[[pathNamesPulseFishing[runNum]]] = out
      cat("\nComplete \n \n")
    }
    
    #NuTS
    cat("\nSection Progress Nuts Fishing:\n")
    numRuns=length(pathNamesNuts)
    for (runNum in 1:numRuns){
      
      cat(pathNamesNuts[runNum]," Progress: ",runNum," of ",numRuns)
      
      path = paste0( pathtoIndicators, pathNamesNuts[runNum], fileName)
      out = readAnalysisWrite( path ,"", listAttrib, listIndic)
      listNutsFish[[pathNamesNuts[runNum]]] = out
      cat("\nComplete \n \n")
    }
    
    #MPA
    cat("\nSection Progress MPA Fishing:\n")
    numRuns=length(pathNamesMPA)
    for (runNum in 1:numRuns){
      
      cat(pathNamesMPA[runNum]," Progress: ",runNum," of ",numRuns)
      
      path = paste0( pathtoIndicators, pathNamesMPA[runNum], fileName)
      out = readAnalysisWrite( path ,"", listAttrib, listIndic)
      listMPAFish[[pathNamesMPA[runNum]]] = out
      cat("\nComplete \n \n")
    }
    
    #Hotspots
    cat("\nSection Progress Hotspots Fishing:\n")
    numRuns=length(pathNamesHotspots)
    for (runNum in 1:numRuns){
      
      cat(pathNamesHotspots[runNum]," Progress: ",runNum," of ",numRuns)
      
      path = paste0( pathtoIndicators, pathNamesHotspots[runNum], fileName)
      out = readAnalysisWrite( path ,"", listAttrib, listIndic)
      listHotsFish[[pathNamesHotspots[runNum]]] = out
      cat("\nComplete \n \n")
    }
    
    
    #-------------------------------------------------------------------
    # LOCAL RUNS
    
    # NuTS
    cat("\nSection Progress Nuts Local Fishing:\n")
    numRuns=length(pathNamesNutsLocal)
    for (runNum in 1:numRuns){
      
      cat(pathNamesNutsLocal[runNum]," Progress: ",runNum," of ",numRuns)
      
      path = paste0( pathtoIndicators, pathNamesNutsLocal[runNum], fileName)
      out = readAnalysisWrite( path ,"", listAttrib, listIndic)
      listNutsLocalFish[[pathNamesNutsLocal[runNum]]] = out
      cat("\nComplete \n \n")
    }
    
    # MPA
    cat("\nSection Progress MPA Local Fishing:\n")
    numRuns=length(pathNamesMPALocal)
    for (runNum in 1:numRuns){
      
      cat(pathNamesMPALocal[runNum]," Progress: ",runNum," of ",numRuns)
      
      path = paste0( pathtoIndicators, pathNamesMPALocal[runNum], fileName)
      out = readAnalysisWrite( path ,"", listAttrib, listIndic)
      listMPALocalFish[[pathNamesMPALocal[runNum]]] = out
      cat("\nComplete \n \n")
    }
    
    # Hotspots
    cat("\nSection Progress Hotspots Local Fishing:\n")
    numRuns=length(pathNamesHotspotsLocal)
    for (runNum in 1:numRuns){
      
      cat(pathNamesHotspotsLocal[runNum]," Progress: ",runNum," of ",numRuns)
      
      path = paste0( pathtoIndicators, pathNamesHotspotsLocal[runNum], fileName)
      out = readAnalysisWrite( path ,"", listAttrib, listIndic)
      listHotsLocalFish[[pathNamesHotspotsLocal[runNum]]] = out
      cat("\nComplete \n \n")
    }
    
    
    
    #-------------------------------------------------------------------
    # Mixed Local vs. Regional, both ind-att and att-ind:  Hotspots
    cat("\nSection Progress Hotspots Local VS Regional Fishing:\n")
    for (runNum in 1:length(pathNamesHotspots)){
      
      cat(pathNamesHotspots[runNum]," Progress: ",runNum," of ",length(pathNamesHotspots))
      
      path1 = paste0( pathtoIndicators, pathNamesHotspots[runNum], fileName)
      path2 = paste0( pathtoIndicators, pathNamesHotspotsLocal[runNum], fileName)
      out = readAnalysisWrite(path1, path2, listAttrib, listIndic)
      #name1= regional(attrib) vs local(indic)
      #name2= local vs regional
      name = paste0( pathNamesHotspots[runNum]," vs ",pathNamesHotspotsLocal[runNum])
      name2 = paste0( pathNamesHotspotsLocal[runNum]," vs ",pathNamesHotspots[runNum])
      listHotsMixFish[[name]] = out[1]
      listHotsMixFish[[name2]] = out[2]
      cat("\nComplete \n \n")
    }
    
    
   cat("\nSection Progress Nuts Local VS Regional Fishing:\n")
   # Mixed Local vs. Regional, both ind-att and att-ind:  Nuts
    for (runNum in 1:length(pathNamesNuts)){
      
      cat(pathNamesNuts[runNum]," Progress: ",runNum," of ",length(pathNamesNuts))
      
      path1 = paste0( pathtoIndicators, pathNamesNuts[runNum], fileName)
      path2 = paste0( pathtoIndicators, pathNamesNutsLocal[runNum], fileName)
      out = readAnalysisWrite(path1, path2, listAttrib, listIndic)
      name = paste0( pathNamesNuts[runNum]," vs ",pathNamesNutsLocal[runNum])
      name2 = paste0( pathNamesNutsLocal[runNum]," vs ",pathNamesNuts[runNum])
      listNutsMixFish[[name]] = out[1]
      listNutsMixFish[[name2]] = out[2]
      cat("\nComplete \n \n")
    }
    
    
    cat("\nSection Progress MPA Local VS Regional Fishing:\n")
    # Mixed Local vs. Regional, both ind-att and att-ind:  MPA
    for (runNum in 1:length(pathNamesMPA)){
      
      cat(pathNamesMPA[runNum]," Progress: ",runNum," of ",length(pathNamesMPA))
      
      path1 = paste0( pathtoIndicators, pathNamesMPA[runNum], fileName)
      path2 = paste0( pathtoIndicators, pathNamesMPALocal[runNum], fileName)
      out = readAnalysisWrite(path1, path2, listAttrib, listIndic)
      name = paste0( pathNamesMPA[runNum]," vs ",pathNamesMPALocal[runNum])
      name2 = paste0( pathNamesMPALocal[runNum]," vs ",pathNamesMPA[runNum])
      listMPAMixFish[[name]] = out[1]
      listMPAMixFish[[name2]] = out[2]
      cat("\nComplete \n \n")
    }
    
  
  #Save the List output in a RData file
  save(
    "listNutsMixFish",
    "listMPAMixFish",
    "listHotsMixFish",
    "listNutsLocalFish",
    "listMPALocalFish",
    "listHotsLocalFish",
    "listFish",
    "listPulseFish",
    "listNutsFish",
    "listMPAFish",
    "listHotsFish", file="MARSS_Data_Output.RData")
  
  #resets your working directory to what is was
  setwd(currentwd)
  
}



#-----------------------------------------------------------
#
#Helper Read Write Function
#-Sorts whether this is a single or double table
#
#--------------------------------------------------------------

readAnalysisWrite = function(fileName1, fileName2="", listAttrib, listIndic){
  
  #initalize the return value
  tableout="NA"
  #call the correct functin depending on whether you get 1 or 2 files
  if(!fileName2==""){#Mix table call
    tableout=readAnalysisWriteDouble(fileName1, fileName2, listAttrib, listIndic) 
  }else{#Single table call
    tableout=readAnalysisWriteSingle(fileName1, listAttrib, listIndic) 
  }
  
  return(tableout)
}


#-----------------------------------------------------------------------
#
#DOUBLE FILE FUNCTION
#
# Input - fileName1 - full path to regional table
#         fileName2 - full path to local table
#         listAttrib -  full list of attribute names
#         listIndic - full list of indicator names
#
# Output - Return a list of the 2 3d array for Regional vs Local and vis versa
#
# Description - This function will take 2 file paths and read them in
#               check there row/col to make sure they are large enough
#               will then handoff the params to myMARSS2.
#               The function is wrapped in a trycatch for io errors
#
#-----------------------------------------------------------------------
readAnalysisWriteDouble = function( fileName1, fileName2, listAttrib, listIndic){
  
  tryCatch({
    # read in table/s
    table1= read.table(fileName1, header=T) # read in table1
    folderName1= strsplit(fileName1, "/")[[1]]# element 6 is the folder name
    
    table2= read.table(fileName2, header=T)# read in table2
    folderName2= strsplit(fileName2, "/")[[1]] # element 6 is the folder name
    
    #Checks for proper table formating
    # Find the smallest number of rows and columns from the two table to make 
    # sure that when you proccess it, it will not break the function 
    # by going beyond the bounds of the matrix
    smallestRows= ifelse((nrow(table1) > nrow(table2) ),
                         nrow(table2), nrow(table1))
    #if true then table2 if false then table1
    
    smallestCols= ifelse((ncol(table1) > ncol(table2)), 
                         ncol(table2), ncol(table1) )
    #if true then table2 if false then table1
    
    # if there are not 75 columns the error and return NA
    if(!ncol(table1) == 75 && !ncol(table2) == 75){
      cat("Error: Table1=",ncol(table1)," or Table2=",
          ncol(table2)," does not have enough columns\n")
      return(NA)
    }
    
    #if there are not enough rows/years to proccess then return NULL
    if(smallestRows<35){
      cat("Error: Max Rows of ",smallestRows,
          "is too small on:\n","File1 = ",fileName1,"\nFile2 = ",fileName2,"\n")
      return(NA)
    }
    
    #do work and return the work
    listout=myMARSS2(table1, table2, folderName1[6], folderName2[6], listAttrib, listIndic)
    return (listout)
    
    #ERROR something went wrong either reading or writing
    # return NULL
  }, error= function(ex){
    cat("Error was detected\n")
    print(ex)
  })
  
}



#-----------------------------------------------------------------------
#
#Single FILE FUNCTION
#
# Input - fileName1 - full path to regional table
#         listAttrib -  full list of attribute names
#         listIndic - full list of indicator names
#
# Output - Return a 3d array of marss data
#
# Description - This function will take a file paths and read it in
#               check there row/col to make sure they are large enough
#               will then handoff the params to myMARSS1.
#               The function is wrapped in a trycatch for io errors
#
#-----------------------------------------------------------------------
readAnalysisWriteSingle = function( fileName1, listAttrib, listIndic){
  
  
  tryCatch({
    
    #cat("Test in readAnalysisWriteSingle\n")
    
    # read in table/s
    table1= read.table(fileName1, header=T) # read in table1
    folderName1= strsplit(fileName1, "/")[[1]]# element 6 is the folder name
    
    #Check for proper formating of tables
    # if there are not 75 columns the error and return NA
    if(!ncol(table1) == 75){
      cat("Error: Table 1 ",ncol(table1)," does not have enough columns\n")
      return(NA)
    }
    
    #if there are not enough rows/years to proccess then return NULL
    if(nrow(table1)<35){
      cat("Error: Max Rows of ",nrow(table1),"is too small on:\n","File1 = ",fileName1,"\n")
      return (NA)
    }
    
    # do work and return work
    array3d=myMARSS1(table1, folderName1[6], listAttrib, listIndic)
    return (array3d)
    
    #ERROR something went wrong either reading or writing
    # return NULL
  }, error= function(ex){
    cat("Error was detected\n")
    print(ex)
  })
  
}


#--------------------------------------------------------
#
#Single MARSS Function
#
# Input - table1 - The current table to do MARSS comparison
#         folderName - Name of the current scenario
#         listAttrib - List of Attribute Names  
#         listIndic - List of Indicator Names
#
# Output - Return a 3d array of marss data
#
# Description - This function will create a 3d array for
#               for storing the output. Then will transform 
#               the data and loop through all of the comparisons
#               storing them in the 3d array. When done will
#               save in a Excel file and return the array
#
#--------------------------------------------------------
myMARSS1 = function( table1, folderName, listAttrib, listIndic){
  
  #create the return list
  marss3d = array(0,dim=c(11,59,11))
  colnames(marss3d)=listIndic
  rownames(marss3d)=listAttrib
  #convert the data frames matrixs and flips the col and rows
  table1=t(as.matrix(table1))
  
  #remove duplicate indicators
  table1=table1[-64:-66,]
  table1=table1[-68,]
  table1=table1[-70,]
  
  #model
  model=list(Z="identity", B=matrix(list("B:1,1","B:2,1",0,"B:2,2"),2,2),
             Q="diagonal and unequal", R="zero")
  
  #vars for the loops
  attrib=11
  indic=59
  
  #tests
  cat("\n-0% ")
  cat("\n")
  
  #Should go through all attributes 1-11
  for(x in 1:attrib ){
    
    #Should go through all Indicators 12-70
    for(y in 1:indic ){
      
      #cat(paste0(x,"vs",y," \n") )
      a=table1[x,]
      i=table1[(y+11),]
      #if the attribute row == indicator row then NA and next
      if(identical(a,i) || checkNARows(table1[x,] , table1[y+11,]) || checkRows(x,y) ){
        marss3d[x,y,1]=NA
        marss3d[x,y,2]=NA
        marss3d[x,y,3]=NA
        marss3d[x,y,4]=NA
        marss3d[x,y,5]=NA
        marss3d[x,y,6]=NA
        marss3d[x,y,7]=NA
        marss3d[x,y,8]=NA
        marss3d[x,y,9]=NA
        marss3d[x,y,10]=T
        marss3d[x,y,11]=T
        #skip to interation
        next
      }
      
      tryCatch({
        #Process the data with MARSS
        marss = MARSS(table1[c(x,y+11),], model=model, silent=T, 
                      control=list(safe=T) )
        
        # If there are no error then error=F and add the B, low/upCI
        marssb = MARSSparamCIs(marss)
        marss3d[x,y,1]=coef(marssb)$B[1]
        marss3d[x,y,2]=coef(marssb)$B[2]
        marss3d[x,y,3]=coef(marssb)$B[3]
        marss3d[x,y,4]=coef(marssb, what="par.lowCI")$B[1]
        marss3d[x,y,5]=coef(marssb, what="par.lowCI")$B[2]
        marss3d[x,y,6]=coef(marssb, what="par.lowCI")$B[3]
        marss3d[x,y,7]=coef(marssb, what="par.upCI")$B[1]
        marss3d[x,y,8]=coef(marssb, what="par.upCI")$B[2]
        marss3d[x,y,9]=coef(marssb, what="par.upCI")$B[3]
        
        if( is.na(marss3d[x,y,5]) || is.na(marss3d[x,y,8]) ){
          marss3d[x,y,10]=T
          marss3d[x,y,11]=T
        }else{
          marss3d[x,y,10]=marssb$convergence
          marss3d[x,y,11]=F
        }
        
      },error= function(ex){
        #If there are any Errors then NA
        cat(paste0("ERROR: ",x,"vs",y," \n") )
        marss3d[x,y,1]=NA
        marss3d[x,y,2]=NA
        marss3d[x,y,3]=NA
        marss3d[x,y,4]=NA
        marss3d[x,y,5]=NA
        marss3d[x,y,6]=NA
        marss3d[x,y,7]=NA
        marss3d[x,y,8]=NA
        marss3d[x,y,9]=NA
        marss3d[x,y,10]=T
        marss3d[x,y,11]=T
      })
      
      
      #cat("After\n")
    }#End of the Indicator Loop
    
    #Add a progress percentage to the console
    o=x/attrib
    o=o*100
    cat( paste0( "\n- ", formatC(o,digits=0,format="d"), "% \n") )
    
  }#End of the Attribute Loop
  
  #export 3d array to excel, also return the 3d array
  exportToXlsx(marss3d, folderName)
  return (marss3d)
}


#--------------------------------------------------------
#
#Single MARSS Function
#
# Input - table1 - The regional table to do MARSS comparison
#         table2 - The local table to do MARSS comparison
#         folderName1 - Name of the regional scenario
#         folderName2 - Name of the local scenario
#         listAttrib - List of Attribute Names  
#         listIndic - List of Indicator Names
#
# Output - Return a list of 2 3d array of marss data
#
# Description - This function will create a 2 new tables
#               one with regional attributes and local indicators
#               and the other one with local attribute
#               and region indicators. Will then feed the tables
#               back to myMARSS1 and return them
#
#--------------------------------------------------------
myMARSS2 = function(table1, table2, folderName1, folderName2, listAttrib, listIndic){
  
  #table1 = Regional
  #table2 = Local
  
  #Add Names for file export
  name1 = paste0(folderName1,"_Attrib=Regional_Indic=Local")
  name2 = paste0(folderName1,"_Attrib=Local_Indic=Regional")
  
  #Create two new temp table for
  #Regional vs Local table = RvL
  #Local vs Regional table = LvR
  RvL=table1
  LvR=table1
  
  #Add attribute from Regional table
  #Add indicator from Local table
  RvL[,1:11]=table1[,1:11]
  RvL[,12:75]=table2[,12:75]
  
  #Add attribute from Local table
  #Add indicator from Regional table
  LvR[,1:11]=table2[,1:11]
  LvR[,12:75]=table1[,12:75]
  
  #Creates list for returning both of the RvL , LVR
  marssList=list()
  marssList[[1]] = myMARSS1(RvL, name1, listAttrib, listIndic)
  marssList[[2]] = myMARSS1(LvR, name2, listAttrib, listIndic)
  
  #Return the list
  return(marssList)
  
}

#-----------------------------------------------------------
#
#CHECKNAROWS -Helper
#
# Description - Helper function that check the 2 rows if all
#               values are NA then return False otherwise True
#
#----------------------------------------------------------
checkNARows = function(row1, row2){
  
  #transforms to matrixs and finds the total 
  #numbers of NA values in the rows
  row1=as.matrix(row1)
  row2=as.matrix(row2)
  rt1=sum(is.na(row1))
  rt2=sum(is.na(row2))
  
  #if either row1 or row2 have ALL NA then return True
  #other wise return False
  if( (rt1==nrow(row1)) || (rt2==nrow(row2)) ){
    return(T)
  }
  return(F)
}


#-----------------------------------------------------------
#
#CHECKROWS -Helper
#
# Description - Checks the Attrib and Indic against
#               hardcoded values that are not suppose to be run
#
#----------------------------------------------------------
checkRows = function(attrib, indic){
  if(attrib == 7 && indic == 5){
    return(T)
  }else if(attrib == 8 && indic == 13){
    return(T)
  }else{
    return(F)
  }
    
}



#-----------------------------------------------------
#
#EXPORTTOXLSX -Helper
#
# Description - Takes in marss 3d array output and Name
#               will output data into excel file with Name
#
#------------------------------------------------------
exportToXlsx = function(marss3d, folderName){
  
  #Create a workbook
  wb = createWorkbook()
  
  #Create the 10 sheets for the Excel with names
  sheet1 = createSheet(wb, sheetName = "B Value(1,1)")
  sheet2 = createSheet(wb, sheetName = "B Value(2,1)")
  sheet3 = createSheet(wb, sheetName = "B Value(2,2)")
  sheet4 = createSheet(wb, sheetName = "lowCI Value(1,1)")
  sheet5 = createSheet(wb, sheetName = "lowCI Value(2,1)")
  sheet6 = createSheet(wb, sheetName = "lowCI Value(2,2)")
  sheet7 = createSheet(wb, sheetName = "upCI Value(1,1)")
  sheet8 = createSheet(wb, sheetName = "upCI Value(2,1)")
  sheet9 = createSheet(wb, sheetName = "upCI Value(2,2)")
  sheet10 = createSheet(wb, sheetName = "Convergence")
  sheet11 = createSheet(wb, sheetName = "Error")
  
  #Add Data the each sheet
  addDataFrame(marss3d[,,1] , sheet1)
  addDataFrame(marss3d[,,2] , sheet2)
  addDataFrame(marss3d[,,3] , sheet3)
  addDataFrame(marss3d[,,4] , sheet4)
  addDataFrame(marss3d[,,5] , sheet5)
  addDataFrame(marss3d[,,6] , sheet6)
  addDataFrame(marss3d[,,7] , sheet7)
  addDataFrame(marss3d[,,8] , sheet8)
  addDataFrame(marss3d[,,9] , sheet9)
  addDataFrame(marss3d[,,10] , sheet10)
  addDataFrame(marss3d[,,11] , sheet11)
  
  #Save the Excel file with the given Name
  saveWorkbook(wb, paste0(folderName,".xlsx"))
  
}





##SAVING JUST INCASE I NEED IT AGAIN

# pathNamesFishing=c(
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Fishing0pt5/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Fishing1pt5/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Fishing2/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Fishing5/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Fishing2xTrawl/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Fishing4xDemersal/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Fishing4xPelagic/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Fishing10xForageKrill/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/FishingStatusQuo/" )
# 
# pathNamesPulseFishing=c(
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/FishingPulse1pt5long/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/FishingPulse1pt5short/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/FishingPulse4long/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/FishingPulse4short/"  )
# 
# pathNamesNuts=c(
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Nuts5xBox3N/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Nuts10xBox3N/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/NutsMajor/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/NutsMedium/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/NutsMinor/" )
# 
# pathNamesHotspots=c(
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/SpatialFishing1pt5/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/SpatialFishing2/")
# 
# pathNamesMPA=c(
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/SpatialFishing50ptNearshore/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/SpatialFishing100pct3Regions/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/SpatialFishing100pctNMS/")
# 
# pathNamesNutsLocal=c(
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Nuts5xBox3NLocal/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/Nuts10xBox3NLocal/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/NutsMajorLocal/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/NutsMediumLocal/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/NutsMinorLocal/" )
# 
# pathNamesHotspotsLocal=c(
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/SpatialFishing1pt5Local/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/SpatialFishing2Local/")
# 
# pathNamesMPALocal=c(
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/SpatialFishing50ptNearshoreLocal/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/SpatialFishing100pct3RegionsLocal/" ,
#   "C:/Users/Tyler.Rothnie/Documents/IndicatorsIEAoutputs/SpatialFishing100pctNMSLocal/")