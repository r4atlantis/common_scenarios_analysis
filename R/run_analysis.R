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
  res.cor <- suppressWarnings(
    cor.test(data[, 1], data[, 2], method = method))
  res.corstd <- suppressWarnings(
    cor.test(data.std[, 1], data.std[, 2], method = method))

  #' 2. Run cross-correlation
  res.ccf <- calc_ccf(data, ...)
  res.ccfstd <- calc_ccf(data.std, ...)

  #' 3. Run MARSS
  tempgrid <- expand.grid(Qem, Rem)
  res.marss <- apply(tempgrid, 1,
    function(x) calc_MARSS(data, Q = x[1], R = x[2]))

  #' 4. Get results
  est <- data.frame(
    "EM_process" = "",
    "EM_observation" = "",
    "par" = c("correlation", paste0("ccf:", res.ccfstd$ccf$lag),
    "ccf:ar1"),
    "est" = c(res.corstd$estimate, res.ccfstd$ccf$acf,
    ifelse("ar1" %in% names(res.ccfstd$xmodel$coef),
      res.ccfstd$xmodel$coef["ar1"], 0)))

  B <- cbind(
    t(sapply(lapply(res.marss, MARSS:::parmat), "[[", "B"))[,
      c(1, 2, 4), drop = FALSE],
    t(sapply(lapply(res.marss, MARSS:::parmat), "[[", "Q"))[,
      c(1, 2, 4), drop = FALSE],
    t(sapply(lapply(res.marss, MARSS:::parmat), "[[", "R"))[,
      c(1, 2, 4), drop = FALSE],
    tempgrid)
  colnames(B) <- c(
    "par_b[\'1,1\']", "par_b[\'2,1\']", "par_b[\'2,2\']",
    "par_q[\'1,1\']", "par_q[\'2,1\']", "par_q[\'2,2\']",
    "par_r[\'1,1\']", "par_r[\'2,1\']", "par_r[\'2,2\']",
    "EM_process", "EM_observation")
  B <- reshape(B, direction = "long",
    varying = list(1:(NCOL(B) - 2)),
    v.names = "est", timevar = "par",
    times = gsub("par_", "", colnames(B))[1:(NCOL(B) - 2)])
  rownames(B) <- NULL
  # Remove the zero constrained process errors
  B <- B[!(B$EM_observation == "zero" &
    grepl(x = B$par, pattern = "R[a-z]:[a-z]")), ]
  # Remove the off-diagonals of the identity matrices
  B <- B[!(grepl(pattern = "diagonal", x = B$EM_observation) &
    grepl(x = B$par, pattern = "Ri:a")), ]
  B <- B[!(grepl(pattern = "diagonal", x = B$EM_process) &
    grepl(x = B$par, pattern = "Qi:a")), ]
  # Remove the ID
  B <- B[, -which(colnames(B) == "id")]

  pars <- merge(est, B, all = TRUE)
  pars <- data.frame(pars, "n" = NROW(data))

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
