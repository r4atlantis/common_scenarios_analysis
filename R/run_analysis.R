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
  method = c("pearson", "spearman"),
  Qem = "unconstrained", Rem = "zero", ...) {

  method <- match.arg(method)

  #' A. Standardize the data
  data.std <- data.frame(
    calc_stdnormal(data[, 1]),
    calc_stdnormal(data[, 2]))
  colnames(data.std) <- colnames(data)

  #' 1. Run correlation
  res.cor <- cor(data[, 1], data[, 2],
    method = method, use = "na.or.complete")
  res.corstd <- cor(data.std[, 1], data.std[, 2],
    method = method, use = "na.or.complete")

  #' 2. Run cross-correlation
  res.ccf <- calc_ccf(data, ...)
  res.ccfstd <- calc_ccf(data.std, ...)

  #' 3. Run MARSS
  tempgrid <- expand.grid(Qem, Rem)
  res.marss <- apply(tempgrid, 1,
    function(x) calc_MARSS(data, Q = x[1], R = x[2]))

  #' 4. Get results
  pars <- cbind(tempgrid,
    t(sapply(lapply(res.marss, MARSS:::parmat), "[[", "B")[
      c(1, 2, 4), , drop = FALSE]),
    t(sapply(lapply(res.marss, MARSS:::parmat), "[[", "Q")[
      c(1, 2, 4), , drop = FALSE]),
    t(sapply(lapply(res.marss, MARSS:::parmat), "[[", "R")[
      c(1, 2, 4), , drop = FALSE]),
     rep(res.corstd, NROW(tempgrid)),
     matrix(rep(res.ccfstd$ccf$acf, each = NROW(tempgrid)),
       nrow = NROW(tempgrid)),
     rep(NA, NROW(tempgrid)))
  colnames(pars) <- c(
    "EM_process", "EM_observation",
    paste(rep(c("b", "q", "r"), each = 3),
      c("[\'1,1\']", "[\'2,1\']", "[\'2,2\']"), sep = "_"),
    "rho",
    paste0("ccf:", res.ccfstd$ccf$lag),
    "AR"
    )
  if ("ar1" %in% names(res.ccfstd$xmodel$coef)) {
    pars$AR <- res.ccfstd$xmodel$coef["ar1"]
  }
  rownames(pars) <- NULL
  pars$n <- NROW(data)
  pars$converged <- sapply(res.marss, "[[", "convergence")

  res.list <- list("data" = data, "data_std" = data.std,
    "correlation" = res.cor,
    "correlation_std" = res.corstd,
    "ccf" = res.ccf,
    "ccf_std" = res.ccfstd,
    "marss" = res.marss,
    "pars" = pars,
    "n" = NROW(data))

  return(res.list)
}

