#' Validate the region name
#'
#' Check against available region names
#'
#' @param region
#' @author Kelli Faye Johnson
#'
validate_regionname <- function(region = NULL) {
  all <- c("CalCu_", "GoMex_",
    "GOC_", "NOBA_",
    "NEUSFixedF_", "NEUSDyn_", "GuamAtlantis_",
    "AEEC_", "AustSE_", "AustSE_DynEffort_",
    "CAM_")
  long <- c("California Current", "Gulf Mexico",
    "Gulf California", "Nordic and Barents Sea",
    "NE USA fixed F", "NE USA dyn. F", "Guam",
    "English Channel", "SE Australia", "SE Australia dyn. F",
    "Chesapeake Bay")

  if (is.null(region)) {
    return(data.frame("short" = all, "long" = long))
  }
  if (!region %in% all) {
    stop(region, " was not in the list of available scenarios.")
  } else {
    return(long[which(all == region)])
  }
}
