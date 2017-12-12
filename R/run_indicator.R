#' Run analyses for a single indicator
#'
#' @param data
#' @param attribute
#' @param indicator
#' @param region
#' @param scenario
#' @param file
#'
#' @author Kelli Faye Johnson
#' @export
#'
run_indicator <- function(data, attribute, indicator,
  region = NA, scenario = NA,
  file = NULL) {

  # Split the scenario name into one of three different groups.
  if (!is.na(scenario)) {
    scenariogroup <- strsplit(scenario, "_")[[1]][1]
    scenarioend <- gsub(paste0(scenariogroup, "_"), "", scenario)
  } else {
    scenariogroup <- NA
    scenarioend <- NA
  }

  if (NCOL(data) != 2 & !"Time" %in% colnames(data)) data <- t(data)
  if (is.null(colnames(data))) colnames(data) <- c("attribute", "indicator")
  if (!"Time" %in% colnames(data)) data$Time <- seq_len(NROW(data))

  data <- data.frame(
    c(data[, "Time"]),
    c(data[, attribute]),
    c(data[, indicator])
    )
  colnames(data) <- c("Time", attribute, indicator)

  data_original <- data
  data <- as.data.frame(data)

  # Check that the relationships can be calculated, which depends on
  # the variance of both the indicator and the attribute being > 0.
  if (length(unique(data[, attribute])) == 1) return(NULL)
  if (length(unique(data[, indicator])) == 1) return(NULL)
  if (diff(range(data[, attribute], na.rm = TRUE)) <
    .Machine$double.eps ^ 0.5) return(NULL)
  if (diff(range(data[, indicator], na.rm = TRUE)) <
    .Machine$double.eps ^ 0.5) return(NULL)
  # Check that the attribute and indicator are not exactly equal
  if (length(all.equal(data[, attribute], data[, indicator],
                       tolerance = 1e-9) == TRUE) != 1) browser()
  if (all.equal(data[, attribute], data[, indicator],
    tolerance = 1e-9) == TRUE) return(NULL)

  if (!is.null(file)) {
    plot_indicators_ts(data = data, scenario = scenario,
      region = region, file = file,
      attribute = attribute, indicator = indicator)
  }

  analysis <- run_analysis(data = data[, c(attribute, indicator)],
    standardize = TRUE)
  if (is.null(analysis[1])) return(NULL)

  analysis$pars$region <- region
  analysis$pars$scenario <- scenario
  analysis$pars$scenariogroup <- scenariogroup
  analysis$pars$scenarioend <- scenarioend
  analysis$pars$estimate_a <- tail(data_original[, attribute], 1)
  analysis$pars$estimate_i <- tail(data_original[, indicator], 1)
  analysis$pars$attribute <- attribute
  analysis$pars$indicator <- indicator

  analysis$filename <- file

  # Save the data
  if(!is.null(file)) {
    file <- normalizePath(file, mustWork = FALSE)
    file <- paste0(file, ".RData")
    filename <- file
    save(analysis, file, filename, data,
      region, scenario, scenariogroup, scenarioend,
      file = file)
  }

  return(analysis)
}
