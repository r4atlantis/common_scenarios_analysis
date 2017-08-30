#' Calculate cross correlation on a data frame
#'
#' @details Run a cross-correlation analysis on a data frame.
#'
#' @param data A \code{data.frame} with two columns,
#' where the first column is the attribute and the second column is the
#' indicator, or the dependent variable. The columns need not be named.
#'
#' @return A list of information regarding the pre-whitening models and
#' the final estimates from the cross correlation function.
#' @author Kelli Faye Johnson
#' @export
#'
calc_ccf <- function(data, maximumlag = 2) {

  x <- data[, 1]
  y <- data[, 2]
  res.list <- list("xmodel" = NULL, "ymodel" = NULL, "ccf" = NULL)

  if (!"forecast" %in% installed.packages()[, 1]) {
    install.packages("forecast", repos = "https://cloud.r-project.org")
  }

  model <- try(forecast::auto.arima(x, seasonal = FALSE),
    silent = TRUE)
  if (class(model)[1] == "try-error") {
    return(res.list)
  } else res.list$xmodel <- model

  # Use the model results to fit the ccf function
  # temp <- TSA::prewhiten(x, y, ylim = c(-1, 1), plot = FALSE)
  ymodel <- try(forecast::Arima(y, model = model), silent = TRUE)
  if ("try-error" %in% class(ymodel)) {
    return(model)
  } else {
    res.list$ymodel <- ymodel
  }
  yresids <- ymodel$residuals

  ccf <- try(ccf(model$residuals, yresids, na.action = na.pass,
      plot = FALSE, lag.max = maximumlag), silent = TRUE)
  if ("try-error" %in% class(ccf)) {
    return(res.list)
  } else {
    res.list$ccf <- ccf
  }

  return(res.list)

}

