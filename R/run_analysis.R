#' Run correlation analysis for a given data set.
#'
#' @details Calculate correlation along a time series.
#' @param data A \code{data.frame} with two columns,
#' where the first column is the attribute and the second column is the
#' indicator, or the dependent variable. The columns need not be named.
#' @param method The statistical method to be used for the correlation
#' analyses, where either spearman (the default) or pearson correlation
#' are allowed methods. Spearman analysis does not assume normality in the
#' data and allows for more data sets to be considered.
#' @param Qem Specifications for process error in the estimation method.
#' @param Rem Specifications for observation error in the estimation method.
#' @param ... Pass arguments to \code{\link{calc_ccf}}.
#'
#' @author Kelli Faye Johnson
#'
run_analysis <- function(data,
  method = c("spearman", "pearson"),
  Qem = "unconstrained", Rem = "zero", ...) {

  method <- match.arg(method)

  #' A. Standardize the data
  data.std <- data.frame(
    calc_stdnormal(data[, 1]),
    calc_stdnormal(data[, 2]))
  colnames(data.std) <- colnames(data)

  #' 1. Run correlation
  res.cor <- suppressWarnings(
    cor.test(data[, 1], data[, 2], method = method))
  res.corstd <- suppressWarnings(
    cor.test(data.std[, 1], data.std[, 2], method = method))

  #' 2. Run cross-correlation
  res.ccf <- calc_ccf(data, ...)
  res.ccfstd <- calc_ccf(data.std, ...)

  #' 3. Run MARSS
  res.marss <- calc_MARSS(data, Q = Qem, R = Rem)

  res.list <- list("data" = data, "data_std" = data.std,
    "correlation" = res.cor,
    "correlation_std" = res.corstd,
    "ccf" = res.ccf,
    "ccf_std" = res.ccfstd,
    "marss" = res.marss)

  return(res.list)
}
