#' Run correlation analysis for a given data set.
#'
#' @details Calculate correlation along a time series.
#' @param data A \code{data.frame} with at least two columns. Columns must
#' alternate between attributes and indicators, but they
#' need not be named.
#' @param lag The maximum lag investigated for the cross correlation functions.
#' @param Bem Specifications for the interaction matrix in the
#' estimation method (EM).
#' @param Qem Specifications for process error in the EM.
#' @param Rem Specifications for observation error in the EM.
#'
#' @author Kelli Faye Johnson
#'
run_analysis <- function(data, lag = 2,
  Bem = "fixed", Qem = "unconstrained", Rem = "zero",
  prewhiten = c("AIC", "AR1", "no")) {

  #' 1. Deal with multiple columns
  Zem <- colnames(data) <- rep(c("a", "i"), times = NCOL(data) / 2)
  data.stacked <- data.frame(
    "a" = c(data[, which(Zem == unique(Zem)[1])]),
    "i" = c(data[, which(Zem == unique(Zem)[2])]))

  #' 2. Standardize the data
  data.std <- apply(data, 2, calc_stdnormal)
  data.std.stacked <- apply(data.stacked, 2, calc_stdnormal)

  #' 3. Run cross-correlation
  prewhiten <- match.arg(prewhiten)
  ccf <- calc_ccf(data.stacked, maximumlag = lag,
    modeltype = prewhiten)
  ccf.std <- calc_ccf(data.std.stacked, maximumlag = lag,
    modeltype = prewhiten)
  names(ccf$pars) <- paste("raw", names(ccf$pars), sep = ".")
  names(ccf.std$pars) <- paste("std", names(ccf.std$pars), sep = ".")
  ccfpars <- c(unlist(ccf$pars), unlist(ccf.std$pars))
  if (prewhiten != "no") {
    nonwhiten <- calc_ccf(data.stacked, maximumlag = lag,
      modeltype = "no")
    nonwhiten.std <- calc_ccf(data.std.stacked, maximumlag = lag,
      modeltype = "no")
    morepars <- c(nonwhiten$pars, nonwhiten.std$pars)
    names(morepars) <- c(paste("raw.no", names(nonwhiten$pars), sep = "."),
      paste("std.no", names(nonwhiten.std$pars), sep = "."))
    ccfpars <- c(ccfpars, morepars)
  }

  #' 4. Run MARSS
  res.marss <- list()
  for (ii_B in Bem) {
  for (ii_Q in Qem) {
  for (ii_R in Rem) {
  for (ii_std in c("raw", "std")) {
    counter <- length(res.marss) + 1
    if (ii_std == "raw") use <- data
    if (ii_std == "std") use <- data.std
    temp <- calc_MARSS(use,
      Q = ii_Q, R = ii_R, B = ii_B, Z = Zem,
      U = ifelse(ii_std == "raw", "unconstrained", "zero"),
      MARSScntliterations = 500)
    if (is.null(temp)) next
    res.marss[[counter]] <- temp
    get <- paste(
      substring(c(ii_std, ii_B, ii_Q, ii_R), 1, 3),
      collapse = ".")
    parvals <- MARSS:::parmat(temp)[c("B", "Q", "R")]
    #' warning: only reporting some of the R matrix
    parvals$R <- parvals$R[1]
    parvals$Qrho <- parvals$Q[2] / (sqrt(parvals$Q[1]) * sqrt(parvals$Q[4]))
    res.marss[[counter]]$parvals <- unlist(parvals)
    names(res.marss)[counter] <- get
  }}}}

  #' 4. Get results
  aa <- sapply(res.marss, "[[", "parvals")
  bb <- c(t(aa))
  names(bb) <- c(outer(colnames(aa), rownames(aa), paste, sep = "."))
  aa <- c(sapply(res.marss, "[[", "convergence"))
  names(aa) <- paste(names(res.marss), "hess", sep = ".")

  pars <- c(ccfpars, bb, aa)
  pars <- c(pars, setNames(c(NROW(data), table(Zem)[1]), c("n", "ngroups")))

  res.list <- list(
    "data" = data,
    "ccf.raw" = ccf, "ccf.std" = ccf.std,
    "marss" = res.marss,
    "pars" = pars,
    "ngroups" = table(Zem),
    "n" = NROW(data)
  )
  if (exists("morepars")) {
    res.list$ccf.raw.no <- nonwhiten
    res.list$ccf.std.no <- nonwhiten.std
  }

  return(res.list)
}
