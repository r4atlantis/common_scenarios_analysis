#' Convert Isaac's basicinfo.csv to a lookup table with species in rows rather than columns
#'
#' Takes a filename containing comma separated 'basicinfo' and converts to a 
#' lookup table for use by \code{get_indicators()}.
#' 
#' @author Gavin Fay 
#'
#' @param lookupfile A string containing the filename of the basicinfo.csv
#' 
#'
#' @examples
#' #example using Isaac's example lookup table with an old NEUS text output
#' lookupfile <- file.path("~","Atlantis","r4atlantis","common_scenarios_analysis","CalCu_BasicInfo.csv")
#' lookup <- read_lookup(lookupfile)

read_lookup <- function(lookupfile)
{
  x <- read.csv(lookupfile,header=FALSE,stringsAsFactors=FALSE)
  labels <- x[,1]
  read.lookup <- as.data.frame(t(x[,-1]))
  names(read.lookup) <- labels
  for (icol in 3:ncol(read.lookup))
    read.lookup[,icol] <- as.numeric(as.character(read.lookup[,icol]))
  return(read.lookup)
}

#Example
#lookupfile <- file.path("~","Atlantis","r4atlantis","common_scenarios_analysis","CalCu_BasicInfo.csv")
#lookup <- read_lookup(lookupfile)



