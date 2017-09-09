#' Plot radarcharts of ecosystem indicators for multiple scenarios/timesteps
#'
#' Takes a set of vectors of ecosystem indicators (each row representing either
#' a unique scenario or time step) and plots a radar chart \code{package 'fmsb'}
#' or 'kite diagram' of the values for the indicators.
#' 
#' @author Gavin Fay
#' 
#' @param ind A dataframe of sets of indicators. First column should be a 
#' scenario identifier. Each subsequent column is an indicator. Rows are 
#' scenarios (or time steps).
#' @param colvec A vector of colors to plot the charts for each scenario.
#' @param legend_labels A vector of labels for the scenarios to be included in 
#' the legend. Could be the values in the first column of \code{ind}. 
#' @param axis_labels A vector of names for the indicator labels to be included
#' on the plot. 
#' @param plotfile A string detailing the filename of the pdf to contain the plot.
#' @param standardized Logical indicating whether the indicators have been 
#' standardized to a base scenario before calling the plotting function.
#' Defaults to FALSE.
#' 
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
#' #choose some example indicators to do a quick test plot.
#' ind_choose <- c("Totbio","Totcat","Exprate","Fishbio","Dempelfish","Bird","Mammal","Mtlbio","Mtlcat","Value")
#' plot_indicators(ind=indicators[,c("Time",ind_choose)],
#'                plotfile='~/sandbox/indicator_plot.pdf',
#'                axis_labels=ind_choose,
#'                legend_labels=indicators$Time)

plot_indicators <- function(ind,
                            colvec=c("#66c2a5","#fc8d62","#8da0cb"),
                            legend_labels=paste("Scenario ",1:3,sep=""),
                            axis_labels=1:8,
                            plotfile='indicator_plot.pdf', 
                            standardized=FALSE, autodetectAxisLimits=FALSE...) {
                            
#throw warning if Isaac forgets to use a color vector the length of his scenarios
  if (length(colvec)<nrow(ind)) {
    print("YOU SUPPLIED A COLOR VECTOR THAT IS SHORTER THAN THE NUMBER OF SCNEARIOS")
    print("COLORS WILL BE RECYCLED AND YOU WON'T BE ABLE TO TELL SCENARIOS APART")
    stop("Stopped function plot_indicators for your own sanity")
  }                           
                            
                            
  aa2 <- ind[,-1]
  #make all in zero to max(ind), can add code here to retain 0->1 for proportion indicators.
  aa1 <- rep(1,ncol(aa2))
  aa1 <- apply(aa2,2,max,na.rm=TRUE)
  aa0 <- aa1
  aa0[] <-0
  #make MTL indicators minimum of 3
  pick <- grep("Mtl",names(ind)[-1])
  aa0[pick] <- 2
  
  # IK hack:
  numOf0pt5Segments <- ceiling(max(ind,na.rm=TRUE)/0.5)
  
  if (standardized) {

    aa1[] <- 2.0 
    aa0[] <- 0
    aa3 <- rep(1,length(aa1))
  }
  
  if (autodetectAxisLimits)  #IK HACK to make it automatically fit plot to axis limits of radar
  {
     aa1[] <- 0.5*numOf0pt5Segments   
  }
 
  
  
  #combine to 
  new_dat <- as.data.frame(rbind(aa1,aa0,aa2))
  lwd_use <- rep(4,nrow(ind))
  col_use <- colvec
  if (standardized) {
    new_dat <- as.data.frame(rbind(aa1,aa0,aa3,aa2))
    col_use <- c(gray(0.1),col_use)
    lwd_use <- c(0.5,lwd_use)
  }
  
  #colnames(new_dat) <- c("Biomass","Catch","Cat/Bio","Dem:Pel","Birds & Seals","MTL Catch","Catch/PP","PropOF","Landed Value")
  #axis_labels <- names(new_dat)
  axis_labels <- rep(axis_labels,length=ncol(new_dat))
  #legend_labels <- ind[,1]
  legend_labels <- rep(legend_labels,length=nrow(ind))
  
   if (autodetectAxisLimits)
   {
     plotfile <- sub("IndicatorPlot","IndicatorPlotAutoAxis", plotfile)
   }
  
  png(file=plotfile,width = 750, height = 750, units = "px")  # 480 by 480 is default
  #png(file=plotfile,width = 750, height = 750, units = "cm",res=300)  # 480 by 480 is default
  
  par(mar=c(0,0,3,0),oma=c(0,0,0,0))



if (autodetectAxisLimits)
{
    radarchartspecifypointsize(new_dat,plwd=2,cglcol=gray(0.1),xlim=c(-1.5,2),
                   ylim=c(-1.5,1.5),pcol=col_use,plty=c(1,3),vlabels=axis_labels,axistype=4,seg= numOf0pt5Segments , caxislabels = as.character(seq(0, 0.5*numOf0pt5Segments ,0.5)),pty=c(18,15,16,17),pointsize=2,vlcex=1.7,calcex=1.7,palcex=1.7)  #seg=5) #
} else  
{     # IF want All axes set to 2 as limit

   radarchartspecifypointsize(new_dat,plwd=2,cglcol=gray(0.1),xlim=c(-1.5,2),
               ylim=c(-1.5,1.5),pcol=col_use,plty=c(1,3),vlabels=axis_labels,axistype=4,seg=4, caxislabels=c("0","0.5","1","1.5","2"),pty=c(18,15,16,17),pointsize=2,vlcex=1.7,calcex=1.7,palcex=1.7)


}
                   
     write.csv(new_dat, file = paste(plotfile, ".csv"))              
             
#text(0.07,0.065,"-1",cex=2)
#text(0.07,0.57,"0",cex=2)
#text(0.07,1.035,"1",cex=2)      
                   
                   

                   
  # playing with raphaelcode: fmsb::radarchart(new_dat,pty=32,plwd=lwd_use,cglcol=gray(0.1),
     #            pcol=col_use,plty=1,vlabels=axis_labels,axistype=0, seg=2,centerzero=T ,caxislabels=c("-1","0","1"))                   
  
  legend(0.9,1.7,legend=legend_labels,lwd=2,col=colvec,cex=1.25,bty='n',lty=c(3,1),pch=c(15,16,17,18),pt.cex = 1.5)  #0.7 1.6
  #mtext(text="(a) Mortality scenarios",side=3,adj=0,line=0,cex=1.5)
  dev.off()
}
