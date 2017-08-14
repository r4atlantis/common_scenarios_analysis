#' Calculate estimates from a MARSS model
#'
#' @details Run a multivariate-autoregressive state-space (MARSS) model.
#' The model structure assumes that there are two time series available,
#' where the attribute affects the indicator. The magnitude of this affect
#' will be estimated using the $B$ matrix in the off-diagonal lower element.
#' The MARSS model is ran using \code{\link[MARSS]{MARSS}}.
#' Warning messages from \code{\link[MARSS]{MARSS}} are suppressed using
#' \code{sink} and \code{\link[base]{tryCatch}}.
#'
#' @param data A \code{data.frame} with two columns,
#' where the first column is the attribute and the second column is the
#' indicator, or the dependent variable. The columns need not be named.
#' @param iterations An integer value specifying the maximum number of
#' iterations to run in the estimation model.
#' @param B
#' @param Q Specifications for process error in the estimation method.
#' @param R Specifications for observation error in the estimation method.
#'
#' @return A list object is returned with the model results and names of
#' the attributes and indicators, along with the original data used to run
#' the model and a status indicator providing information on if the model was
#' ran (i.e., "good") or not (i.e., "bad").
#' @author Kelli Faye Johnson
#' @export
#'
calc_MARSS <- function(data, iterations = 100,
  B = matrix(list("a:a", "i:a", 0, "i:i"), 2, 2),
  Q = "unconstrained", R = "zero") {

  choices <- c(
    "unconstrained",
    "diagonal and equal",
    "diagonal and unequal",
    "identity",
    "zero")

  Q <- match.arg(Q, choices = choices, several.ok = FALSE)
  R <- match.arg(R, choices = choices, several.ok = FALSE)

  # Control variables for the MARSS model
  cntl <- list(allow.degen = FALSE, maxit = iterations, safe = TRUE)

  # Use the attribute and indicator names to determine
  # the B matrix, where an indicator cannot affect an attribute,
  # i.e., a = attribute i = indicator c = correlation
  # a 0
  # c i
  model <- list(
    # tinitx = 1, # Initial state of time-step 0 (default) or 1
    x0 = "zero",
    V0 = "zero", # default is zero x(0) ~ MVN(x0, V0)
    U = "zero", # b/c the data are z-scored U should ~ == 0
    B = B,
    # B = matrix(list("B:1,1","B:2,1",0,"B:2,2"),2,2),
    Q = Q, # Proc error ~MVN(0,Q)
    Z = "identity",
    R = R # Obs error ~MVN(0,R),
    )

  data_model <- t(data[, 1:2])

  ci <- try(
    MARSS::MARSS(data_model, model = model, control = cntl,
    silent = TRUE, fit = TRUE, method = "kem"),
    silent = TRUE)
  if (class(ci) != "try-error") {
    ci <- suppressWarnings(try(MARSS::MARSSparamCIs(ci), silent = TRUE))
    if (any(unlist(lapply(ci$par.se, is.na)))) ci$convergence <- 11
  }

  return("marss" = ci)
}
