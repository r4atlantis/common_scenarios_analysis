#' Read in the data from common scenarios Atlantis runs.
#'
#' Reads in data from a scenario and region and
#' returns a single list.
#'
#' @param scenario
#' @param region
#' @param dir The location where all of the \code{.txt} and
#' \code{BasicInfo} files are stored from Atlantis runs.
#'
#' @author Kelli Faye Johnson
#' @return A \code{list} of the lookup information,
#' catch, and biomass for the specified scenario and region.
#'
#' @examples
#' temp <- read_data(scenario = "BC",
#'   region = "CalCu_", dir = "common_scenarios_data")
#'
read_data <- function(scenario, region, dir = getwd()) {

  lookup <- read_lookup(file.path(dir, paste0(region, "_BasicInfo.csv")))
  # Read in the files
  bio <- read.table(
    file.path(dir, paste(region, scenario, "BiomIndx.txt", sep = "_")),
    header = TRUE)
  catch <- read.table(
    file.path(dir, paste(region, scenario, "Catch.txt", sep = "_")),
    header = TRUE)

  returnme <- list("lookup" = lookup, "bio" = bio, "catch" = catch)
  attr(returnme, "region") <- region
  attr(returnme, "scenario") <- scenario

  return(returnme)
} # end function
