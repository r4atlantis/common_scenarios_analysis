#'
#' @param data
#' @param MARSS
#' @param correlation
#' @param crosscor
#' @param verbose A logical.
#'
get_tscorrelation <- function(data, MARSS, correlation, crosscor,
  verbose = FALSE) {

  if (NCOL(data) < 3) stop("indicator must be of three columns: ",
    "(a) Time, (b) attribute, and (c) indicator")

  tsinfo <- merge(merge(MARSS, correlation, all = TRUE), crosscor, all = TRUE)

  # Bring in the attribute and indicator
  # new <- data.frame(rbind(

  #   data.frame(
  #     "time" = data$Time,
  #     "estimate" = calc_stdnormal(data[, 2]),
  #     "type" = "data",
  #     "group" = "attribute",
  #     lag = 1),

  #   data.frame(
  #     "time" = data$Time,
  #     "estimate" = calc_stdnormal(data[, 3]),
  #     "type" = "data",
  #     "group" = "indicator",
  #     "lag" = 0)),

  #   "lowerCI" = NA, "upperCI" = NA, "n" = NROW(data))

  # tsinfo <- merge(tsinfo, new, all = TRUE)

  attr(tsinfo, "attribute") <- as.character(MARSS$attribute)
  attr(tsinfo, "indicator") <- as.character(MARSS$indicator)

  return(tsinfo)

}
