#' @param forvalues Three values that you want to use for the range and
#' middle value of process error correlation, autocorrelation, and
#' interactions.
#' @param plotcolours Three values for colors used for different levels
#' of process error correlation
#' @param directory The directory where you would like to save the plot.
#' The plot will be saved as 01_timeseriesexample.png. If this argument
#' is \code{NULL}, then the plot is printed to the screen instead.

plot_exampletimeseries <- function(forvalues = c(-0.9, 0, 0.9),
  plotcolours = c("#bdbdbd", "#969696", "#636363"),
  directory = getwd()) {

  if (!is.null(directory)) {
    png(file.path(directory, "01_timeseriesexample.png"),
      res = 300, units = "in", width = 11, height = 8)
  }

  par(
    mfcol = c(length(forvalues), length(forvalues)),
    mar = c(0, 0, 0, 0), oma = c(4, 4, 4, 4),
    xpd = TRUE)
  plotcolours <- plotcolours

  for (auto in c(min(forvalues), 0, max(forvalues))) {
  for (interaction in c(min(forvalues), 0, max(forvalues))){
  set.seed(1)
  a <- lapply(c(min(forvalues), 0, max(forvalues)), function(x) {
    set.seed(1)
    calc_MARSSsim(Bom = c(auto, interaction, auto),
    Rom = 0,
    Qom = x,
    iterations = 1, tslength = 15)$sim.data[, , 1]
  })
  a <- do.call("rbind", a)

  plot(a[2, ], type = "l", ylab = "", xlab = "", axes = FALSE,
    ylim = c(min(a), max(a)),
    col = plotcolours[1], lwd = 3, las = 1)
  lines(a[4, ], col = plotcolours[2], lwd = 3)
  lines(a[6, ], col = plotcolours[3], lwd = 3)
  abline(v = 10, col = "black", lty = 1, lwd = 1)
  lines(a[1, ], col = "black", lty = 1, lwd = 4)

  if (auto == -0.9) axis(side = 2, las = 1)
  if (interaction == 0.9) axis(side = 1)
  if (auto == max(forvalues) & interaction == min(forvalues)) mtext(side = 4, min(forvalues), las = 1)
  if (auto == max(forvalues) & interaction == 0.0) mtext(side = 4, "0", las = 1)
  if (auto == max(forvalues) & interaction == 0) mtext(side = 4, "interaction", las = 0, line = 1.5)
  if (auto == max(forvalues) & interaction == max(forvalues)) mtext(side = 4, max(forvalues), las = 1)
  if (auto == min(forvalues) & interaction == min(forvalues)) mtext(side = 3, min(forvalues))
  if (auto == 0 & interaction == min(forvalues)) mtext(side = 3, "0")
  if (auto == max(forvalues) & interaction == min(forvalues)) mtext(side = 3, max(forvalues))
  if (auto == 0 & interaction == min(forvalues)) mtext(side = 3, "autocorrelation", line = 1.5)
  if (auto == min(forvalues) & interaction == min(forvalues)) legend("topleft", bty = "n", legend = c(min(forvalues), 0, max(forvalues)), col = plotcolours, lty = 1,
    title = "process", lwd = 3, cex = 1.5)
  }}

  if (!is.null(directory)) {
    dev.off()
  }
}
