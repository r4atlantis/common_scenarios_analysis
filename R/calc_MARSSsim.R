#' Simulate data using \code{\link[MARSS]{MARSSsimulate}}
#'
#' @param Bom A vector of length three, specifying the upper diagonal,
#' the lower off diagonal, and the lower diagonal of the B, or interaction,
#' matrix of the \code{MARSS} model.
#' @param Rom A single value, specifying the level of observation error,
#' where the two time series are subject to the same level of observation
#' error, though they will be independently generated, I believe.
#' @param Qom A single value specifying the lower off diagonal of the process-
#' error matrix, where the diagonals are set to one.
#' @param Zom A character vector specifying the states to
#' which observations belong.
#' @param iterations An integer value specifying the number of simulated
#' data sets to produce.
#' @param tslength An integer value specifying the number of years to
#' include in each simulated data set.
#'
#' @examples
#'  yes <- calc_MARSSsim(Bom = c(0.9, 0.1, 0.9),
#'   Rom = 0.1, Zom = c("a", "a", "i", "i"), Qom = c(1.0, 0.2, 1.0),
#'   iterations = 2, tslength = 20)
#' rm(yes)
#'
calc_MARSSsim <- function(Bom = c(0.01, 0.0, 0.01), Rom = 0.01,
  Qom = c(1, 0, 1), Zom = c("a", "i"),
  iterations = 100, tslength = 20) {

  if (length(Bom) != 3) stop("Bom must be of length 3")
  if (length(Qom) != 3) stop("Qom must be of length 3")
  if (length(unique(Zom)) != 2) stop("Zom must have two types, a and i.")

  # Data
  data <- sapply(Zom, function(x) rnorm(tslength))

  # Specify the model
  # "identity", "unconstrained", "diagonal and unequal",
  # "diagonal and equal", "equalvarcov", "zero"
  #' Equal var cov sets the off diagonals to be equal and the diagonals
  #' to be equal, but the off diagonals are not equal to the diagonals.
  model <- list(
    # tinitx = 1, # Initial state of time-step 0 (default) or 1
    x0 = "zero",
    V0 = "zero", # default is zero x(0) ~ MVN(x0, V0)
    U = "zero", # b/c the data are z-scored U should ~ == 0
    B = matrix(list("a:a", "i:a", 0, "i:i"), 2, 2),
    # B = matrix(list("B:1,1","B:2,1",0,"B:2,2"),2,2),
    Q = "unconstrained", # Proc error ~MVN(0,Q)
    Z = factor(Zom),
    A = "zero", # Data should not have different scales
    R = "diagonal and equal" # Obs error ~MVN(0,R),
    )
  # Fit a MARSS model to the data
  MLEobj <- MARSS::MARSS(y = t(data), inits = NULL,
    model = model,
    method = "kem", form = "marxss",
    fit = TRUE, silent = TRUE,
    control = list("maxit" = 100, "conv.test.slope.tol" = 1, "abstol" = 1),
    MCbounds = NULL,
    fun.kf = "MARSSkfss")
  # Fixing parameters
  # c("marss", "method", "par")
  MLEobj$par$B[1:length(MLEobj$par$B)] <- Bom
  MLEobj$par$Q[1:length(MLEobj$par$Q)] <- Qom
  MLEobj$par$R[1:length(MLEobj$par$R)] <- Rom

  # Simulate
  set.seed(iterations)
  sim <- suppressWarnings(MARSS::MARSSsimulate(MLEobj, tSteps = tslength,
    nsim = iterations))
  sim$sim.data <- aperm(sim$sim.data, c(2, 1, 3))
  for (ii in 1:dim(sim$sims)[3]) {
    colnames(sim$sims[,,ii]) <- Zom
  }
  sim$Zom <- Zom

  return(sim)
}
