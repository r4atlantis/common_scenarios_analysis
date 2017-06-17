#' Run a MARSS model
#'
#' @details Run a multivariate-autoregressive state-space (MARSS) model.
#' The model structure assumes that there are two time series available,
#' where the attribute affects the indicator. The magnitude of this affect
#' will be estimated using the $B$ matrix in the off-diagonal lower element.
#' The MARSS model is ran using \code{\link[MARSS]{MARSS}}.
#' Warning messages from \code{\link[MARSS]{MARSS}} are suppressed using
#' sink and \code{\link[base]{tryCatch}}.
#'
#' @param data A data frame containing at least three columns
#' (1) \code{"Time"}, (2) an attribute as specified using \code{attribute},
#' and (3) an indicator specified using \code{indicator}.
#' @param attribute A character value specifying the column name of the
#' attribute in \code{data}.
#' @param indicator A character value specifying the column name of the
#' indicator in \code{data}.
#'
#' @return A list object is returned with the model results and names of
#' the attributes and indicators, along with the original data used to run
#' the model and a status indicator providing information on if the model was
#' ran (i.e., "good") or not (i.e., "bad").
#' @author Kelli Faye Johnson
#' @export

run_MARSS <- function(data, attribute, indicator,
  Q = c("unconstrained", "diagonal and equal", "diagonal and unequal"),
  R = c("zero", "unconstrained", "diagonal and equal", "diagonal and unequal")
  ) {

  choices <- c("zero", "unconstrained",
    "diagonal and equal", "diagonal and unequal")
  Q <- match.arg(Q, several.ok = FALSE)
  R <- match.arg(R, several.ok = FALSE)

  # Control variables for the MARSS model
  cntl <- list(allow.degen = FALSE, maxit = 100,
    safe = TRUE)

  # Use the attribute and indicator names to determine
  # the B matrix, where an indicator cannot affect an attribute,
  # i.e., a = attribute i = indicator c = correlation
  # a 0
  # c i
  B <- matrix(list(
    paste0(attribute, ":", attribute),
    paste0(indicator, ":", attribute),
    0,
    paste0(indicator, ":", indicator)), 2, 2)
  model <- list(
    # tinitx = 1, # Initial state of time-step 0 (default) or 1
    x0 = "zero",
    V0 = "zero", # default is zero x(0) ~ MVN(x0, V0)
    U = "zero", # b/c the data are z-scored U should ~ == 0
    B = B,
    # B = matrix(list("B:1,1","B:2,1",0,"B:2,2"),2,2),
    Q = Q, # Proc error ~MVN(0,Q)
    Z = "identity",
    A = "zero", # Data are already standardized
    R = R # Obs error ~MVN(0,R),
    )

  # reshape the data
  if ("Time" %in% colnames(data)) {
    data <- data[order(data$Time), ]
  }

  data_model <- t(data[, c(attribute, indicator)])

  ci <- try(
    MARSS::MARSS(data_model, model = model, control = cntl,
    silent = TRUE, fit = TRUE, method = "kem"),
    silent = TRUE)
  if (class(ci) != "try-error") {
    ci <- try(MARSS::MARSSparamCIs(ci), silent = TRUE)
  }

  tsinfo <- data.frame(
    "type" = "MAR",
    "estimate" = NA, "lowerCI" = NA, "upperCI" = NA,
    "attribute" = attribute, "indicator" = indicator,
    "lag" = 1, "inside" = NA)
  tsinfo$nyears <- NROW(data)

    if (class(ci) != "try-error") {
      ci$data <- data_model
      tsinfo$estimate <- coef(ci)$B[2, 1]
      tsinfo$lowerCI <- tsinfo$estimate - 1.96 * ci$par.se$B[2, 1]
      tsinfo$upperCI <- tsinfo$estimate + 1.96 * ci$par.se$B[2, 1]
      tsinfo$inside <- tryCatch({
        ifelse(
          findInterval(0, c(tsinfo$lowerCI, tsinfo$upperCI)) == 1L,
          FALSE, TRUE)
        }, error = function(ex) {FALSE}, warning = function(ex) {FALSE}
      )
    }
    ci$tsinfo <- tsinfo

  return("marss" = ci)
}


