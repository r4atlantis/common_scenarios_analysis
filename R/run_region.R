#' Run all three correlation types for a given region
#'
#' @param datalocation A directory that houses the data
#' @param resultslocation A directory where you want the results stored
#' @param region A character value specifying the region
#' @param mgmtlevel A numerical value specifying the level of $B_0$ where
#' overfishing is thought to occur.
#' @param indicators A vector of character values specifying the column names
#' resulting from \code{\link{get_indicators}} that you wish to use as
#' indicators
#' @param attributes A vector of character values specifying the column names
#' resulting from \code{\link{get_indicators}} that you wish to use as
#' attributes
#' @param ... Arguments that you wish to pass to \code{\link{run_scenario}}
#'
#' @author Kelli Faye Johnson
#'
run_region <- function(datalocation, resultslocation = getwd(),
  region, mgmtlevel = 0.5,
  indicators, attributes, ...) {
  scens <- read_scenarios(dir = datalocation, region = region)

  saveme <- lapply(scens, function(x) {
    scen_data <- read_data(scenario = x, region = region,
      dir = datalocation)
      # Hard-code the Management Threshold to 0.5 of Bzero.
    scen_data$lookup[, grepl("target reference", colnames(scen_data$lookup))] <- mgmtlevel
    run_term <- run_scenario(data = scen_data,
      indicators = indicators, attributes = attributes,
      dir = resultslocation, ...)
  })

  return(saveme)
}
