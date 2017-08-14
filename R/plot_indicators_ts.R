#'
#' @param data A data.frame on the normal scale
#' @param scenario
#' @param region
#' @param file A character value providing the full name for the file to be
#' saved. If you want the plot printed to the screen leave as the default,
#' which is \code{NULL}.
#' @param attribute Character values supplying the appropriate column name,
#' or a numeric value specifying the column number, but the labels on the
#' plot look better if it is the former rather than the later.
#' @param indicator Character values supplying the appropriate column name,
#' or a numeric value specifying the column number, but the labels on the
#' plot look better if it is the former rather than the later.
#'
plot_indicators_ts <- function(data, scenario, region, file = NULL,
  attribute, indicator) {

  my.size <- 0.75
  if (!is.null(file)) on.exit(dev.off())
  if (is.matrix(data)) data <- as.data.frame(data)

  scenarioname <- gsub("cur_", " ", scenario)
  scenarioname <- gsub("mpa", "MPA", scenarioname)
  scenarioname <- gsub("OA_01", "OA_extreme", scenarioname)
  scenarioname <- gsub("OA_005", "OA_mild", scenarioname)
  scenarioname <- gsub("_", " ", scenarioname)
  indicatorname <- gsub("fish|tot|sur", "", indicator)
  indicatorname <- gsub("^m", "", indicatorname)

  if (!"Time" %in% colnames(data)) data$Time <- 1:NROW(data) * 365
  data_original <- data
  data[, attribute] <- calc_stdnormal(data[, attribute])
  data[, indicator] <- calc_stdnormal(data[, indicator])

  if (!is.null(file)) {
    png(filename = normalizePath(paste0(file, ".png"), mustWork = FALSE),
        res = 100)
  }
  par(mfrow = c(3, 1), mar = c(0, 4, 0.5, 4), oma = c(4, 1, 2, 1),
    cex = my.size)
  plot(x = data_original$Time, y = data_original[, attribute],
    ylim = range(data_original[, c(attribute, indicator)]),
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
}
