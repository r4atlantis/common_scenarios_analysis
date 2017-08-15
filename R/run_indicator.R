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

  data <- data[, c("Time", attribute, indicator)]

  data_original <- data

  # Check that the relationships can be calculated, which depends on
  # the variance of both the indicator and the attribute being > 0.
  if (length(unique(data[, attribute])) == 1) return(NULL)
  if (length(unique(data[, indicator])) == 1) return(NULL)
  if (diff(range(data[, attribute], na.rm = TRUE)) <
    .Machine$double.eps ^ 0.5) return(NULL)
  if (diff(range(data[, indicator], na.rm = TRUE)) <
    .Machine$double.eps ^ 0.5) return(NULL)
  # Check that the attribute and indicator are not exactly equal
  if (all(data[, attribute] == data[, indicator])) return(NULL)


  if (!is.null(file)) {
    plot_indicators_ts(data = data, scenario = scenario,
      region = region, file = file,
      attribute = attribute, indicator = indicator)
  }

  analysis <- run_analysis(data = data[, c(attribute, indicator)])
  # marss_ts <- run_MARSS(data = data, attribute = attribute, indicator = indicator)
  # cor_ts <- run_cor(data = data_original, attribute = attribute, indicator = indicator)
  # cross_ts <- run_crosscor(data = data, attribute = attribute, indicator = indicator)

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
