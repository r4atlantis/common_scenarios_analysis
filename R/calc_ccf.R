#' Calculate cross correlation with prewhitening
#'
#' @details Run a cross-correlation analysis on a data frame.
#'
#' @param data A \code{data.frame} with two columns,
#' where the first column is the attribute and the second column is the
#' indicator, or the dependent variable. The columns need not be named.
#' @param maximumlag An integer specifying the maximum lag one wants to
#' investigate, where the lag will be looked at in both directions.
#' @param modeltype A character value specifying the type of model for
#' prewhitening the leading time series. If \code{model = "no"},  the data
#' are not prewhitened. If \code{model = "AR1"} a AR(1) model
#' is used to prewhiten the data rather than a model that best fits the data as
#' chosen by AIC. The latter, i.e., \code{model = "AIC"},
#' represents the default, where multiple models are ran,
#' but only the best model as chosen by AIC is reported.
#' @return A list of information regarding the pre-whitening models and
#' the final estimates from the cross correlation function.
#' @author Kelli Faye Johnson
#' @export
#'
#' @examples
#' set.seed(20)
#' n <- 1000; my.sd <- 0.05
#' dataset <- data.frame("x" = rnorm(n, mean = 0, sd = my.sd),
#'   "y" = rnorm(n, mean = 0, sd = my.sd))
#' dataset$y <- c(0, dataset$x[-n]) * 0.5 + dataset$y
#' results <- calc_ccf(data = dataset, maximumlag = 3)
#' # Print cross correlation
#' print(results$ccf)
#' Print prewhitening model fit to time series "x"
#' print(results$xmodel)
#'
calc_ccf <- function(data, maximumlag, modeltype = "AIC") {

  # Rank function taken from stats::cor
  Rank <- function(u) {
      if (length(u) == 0L)
          u
      else if (is.matrix(u) | is.data.frame(u)) {
          if (nrow(u) > 1L)
              apply(u, 2L, rank, na.last = "keep")
          else row(u)
      }
      else rank(u, na.last = "keep")
  }

  # Set up
  x <- data[, 1]
  y <- data[, 2]

  pars <- rep(NA, maximumlag * 2 * 2 + 2)
  names(pars) <- c(
    paste0("ccf.p.", -maximumlag:maximumlag),
    paste0("ccf.s.", -maximumlag:maximumlag))
  pars <- as.data.frame(t(pars))
  res.list <- list(
    "xmodel" = NULL,
    "ymodel" = NULL,
    "ccf" = NULL,
    "ccf.spear" = NULL,
    "residuals" = NULL,
    "rank" = NULL,
    "pars" = pars)


  if (modeltype == "no") {
    res.list$residuals <- data.frame("x" = x, "y" = y)
    model <- modeltype
  }
  if (modeltype == "AR1") {
    model <- try(stats::arima(x, c(1, 0, 0), include.mean = FALSE),
      silent = TRUE)
    # Checked with
    # try(forecast::auto.arima(x, seasonal = FALSE,
    # max.d = 0, max.q=0, max.p=1),silent = TRUE)
    # and they gave very similar results
  }
  if (modeltype == "AIC") {
    model <- try(forecast::auto.arima(x, seasonal = FALSE),
    silent = TRUE)
  }

  if (modeltype != "no"){
    if (class(model)[1] == "try-error") {
      return(res.list)
    } else res.list$xmodel <- model

    # Use the model results to fit the ccf function
    # temp <- TSA::prewhiten(x, y, ylim = c(-1, 1), plot = FALSE)
    # The below code matches stats::filter(y, filter = c(1, -coef(model)),
    # sides = 1, method = "convolution")
    res.list$ymodel <- try(forecast::Arima(y, model = model), silent = TRUE)
    if ("try-error" %in% class(res.list$ymodel)) {
      return(res.list)
    }
    res.list$residuals <- data.frame("x" = model$residuals,
      "y" = res.list$ymodel$residuals)
  }
  # Ranked values
  res.list$rank <- Rank(res.list$residuals)
  ccf <- try(ccf(res.list$residuals[, 1], res.list$residuals[, 2],
    na.action = na.pass, plot = FALSE, lag.max = maximumlag), silent = TRUE)
  ccf.spear <- try(ccf(res.list$rank[, 1], res.list$rank[, 2],
    na.action = na.pass, plot = FALSE, lag.max = maximumlag), silent = TRUE)

  if (!"try-error" %in% class(ccf)) {
    res.list$ccf <- ccf
    pars[1:(maximumlag * 2 + 1)] <- ccf$acf[, 1, 1]
  }
  if (!"try-error" %in% class(ccf)) {
    res.list$ccf.spear <- ccf.spear
    pars[(maximumlag * 2 + 2):NCOL(pars)] <- ccf.spear$acf[, 1, 1]
  }

  if (modeltype != "no") {
    arima <- coef(model)
    orders <- forecast::arimaorder(model)
    names(orders) <- c("p", "d", "q")

    pars <- cbind(pars,
      "arima" = deparse(substitute(round(arima, 2)), width.cutoff = 500),
      "ar1" = ifelse("ar1" %in% names(arima), arima["ar1"], NA),
      "ar2" = ifelse("ar2" %in% names(arima), arima["ar2"], NA),
      t(orders))
  }
  res.list$pars <- pars

  return(res.list)

}

