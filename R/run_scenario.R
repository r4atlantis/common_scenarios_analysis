#' Run all three correlation methods for a given scenario
#'
#' @param data
#' @param indicators A vector of character values specifying the column names
#' resulting from \code{\link{get_indicators}} that you wish to use as
#' indicators
#' @param attributes A vector of character values specifying the column names
#' resulting from \code{\link{get_indicators}} that you wish to use as
#' attributes
#' @param nyears
#' @param end
#' @param fishlevel A numerical value specifying the level of $B_0$ where
#' the stock is assumed to be overfished.
#' @param dir The location of where the results should be stored.
#'
#' @author Kelli Faye Johnson
#'
run_scenario <- function(data, indicators, attributes,
  nyears = 30, end = 49, fishlevel = 0.5, dir = getwd()) {

  region <-  ifelse(is.null(attr(data, "region")), NA, attr(data, "region"))
  scenario <-ifelse(is.null(attr(data, "scenario")),NA, attr(data, "scenario"))

  resultsfile <- file.path(dir, paste0(region, "_", scenario))

  ind <- with(data, get_indicators(bio, catch, lookup,
    overfishedproportion = fishlevel))
  ind <- get_years(ind, end - nyears + 1, end)

  saveme <- lapply(seq_along(indicators), function(x) {
    run_indicator(data = ind,
      attribute = attributes[x], indicator = indicators[x],
      region = region, scenario = scenario)
  })

  # tsinfo <- do.call("rbind", lapply(saveme, "[[", "tsinfo"))

  return(saveme)
}
