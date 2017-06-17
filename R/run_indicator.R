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

  my.size <- 0.75

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

  data[, attribute] <- calc_stdnormal(data[, attribute])
  data[, indicator] <- calc_stdnormal(data[, indicator])

  if (!is.null(file)) {
    scenarioname <- gsub("cur_", " ", scenario)
    scenarioname <- gsub("mpa", "MPA", scenarioname)
    scenarioname <- gsub("OA_01", "OA_extreme", scenarioname)
    scenarioname <- gsub("OA_005", "OA_mild", scenarioname)
    scenarioname <- gsub("_", " ", scenarioname)
    indicatorname <- gsub("fish|tot|sur", "", indicator)
    indicatorname <- gsub("^m", "", indicatorname)
    png(filename = normalizePath(paste0(file, ".png"), mustWork = FALSE),
      res = 100)
    par(mfrow = c(3, 1), mar = c(0, 4, 0.5, 4), oma = c(4, 1, 2, 1),
      cex = my.size)
    plot(x = data_original$Time, y = data_original[, attribute],
      ylim = c(0, max(data_original[, c(attribute, indicator)])),
      type = "l", lty = 1, xaxt = "n",
      xlab = "", ylab = "absolute")
    mtext(side = 3, line = 0.2, cex = my.size * 1.1,
      text = paste0(scenarioname, " in ", region))
    lines(x = data_original$Time, y = data_original[, indicator],
      lty = 2)
    legend("bottomright", legend = c("attribute", "indicator"),
      lty = c(1, 2),
      bty = "n", horiz = TRUE)
    plot(x = data$Time, y = data[, attribute],
      ylim = c(min(data[, c(attribute, indicator)]),
               max(data[, c(attribute, indicator)])),
      type = "l", lty = 1, xaxt = "n",
      xlab = "year", ylab = "standardized")
    lines(x = data$Time, y = data[, indicator],
      lty = 2)
    plot(x = data_original$Time, y = data_original[, attribute],
      type = "l", lty = 1, xaxt = "n",
      xlab = "", ylab = "attribute")
    par(new = TRUE)
    plot(x = data_original$Time, y = data_original[, indicator],
      type = "l", lty = 2, xaxt = "n", yaxt = "n",
      xlab = "", ylab = "")
    axis(side = 1, labels = data$Time / 365,
      at = data$Time)
    axis(side = 4)
    mtext(side = 4, line = 2.5, text = indicatorname,
      font.lab = 2, cex = my.size)
    mtext(side = 1, outer = TRUE, cex = my.size, line = 2.2,
      text = "post burn-in to terminal year")
    dev.off()
  }

  marss_ts <- run_MARSS(data = data, attribute = attribute, indicator = indicator)
  cor_ts <- run_cor(data = data_original, attribute = attribute, indicator = indicator)
  cross_ts <- run_crosscor(data = data, attribute = attribute, indicator = indicator)

  tsinfo <- merge(merge(
    marss_ts$tsinfo,
    cor_ts, all = TRUE),
    cross_ts$tsinfo, all = TRUE)

  tsinfo$region <- region
  tsinfo$scenario <- scenario
  tsinfo$scenariogroup <- scenariogroup
  tsinfo$scenarioend <- scenarioend
  tsinfo$estimate_a <- tail(data_original[, attribute], 1)
  tsinfo$estimate_i <- tail(data_original[, indicator], 1)

  returnme <- list(
    "marss" = marss_ts,
    "cor_ts" = cor_ts,
    "crosscor" = cross_ts,
    "tsinfo" = tsinfo,
    "filename" = file
    )

  # Save the data
  if(!is.null(file)) {
    file <- normalizePath(file, mustWork = FALSE)
    file <- paste0(file, ".RData")
    filename <- file
    save(cor_ts, cross_ts, file, data, region,
      scenario, scenariogroup, scenarioend,
      marss_ts, tsinfo, filename,
      file = file)
  }

  return(returnme)
}

#' Run analyses for an indicator given a subset data
#'
#' @param data
#' @param attribute
#' @param indicator
#' @param file
#'
#' @author Kelli Faye Johnson
#' @export
#'
run_indicator_subset <- function(data, attribute, indicator,
  file = NULL, length = 15, n = 10, ...) {

  set.seed(n + 500)
  timeserieslength <- NROW(data)
  extrayears <- timeserieslength - length

  if (extrayears < 0) {
    stop("Your data has ", timeserieslength, " but you set",
      " length to ", length, "\nplease shorten length.")
  }
  if (extrayears < n) {
    warning("You set n to ", n, " but you only have ",
      extrayears, " of data and risk resampling.")
  }

  getrow <- sample(1:(timeserieslength - length), size = n, replace = FALSE)

  runs <- list()
  for (n_it in 1:n) {
    getrows <- getrow[n_it]:(getrow[n_it] + length - 1)
    runs[[n_it]] <- run_indicator(data = data[getrows, ],
      attribute = attribute,
      indicator = indicator,
      ...)
  }

  return(runs)
}
