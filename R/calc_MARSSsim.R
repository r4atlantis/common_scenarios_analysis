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
#' @param iterations An integer value specifying the number of simulated
#' data sets to produce.
#' @param tslength An integer value specifying the number of years to
#' include in each simulated data set.
#'
calc_MARSSsim <- function(Bom = c(0.01, 0.0, 0.01), Rom = 0,
  Qom = 0, iterations = 100, tslength = 20) {

  choices <- c("diagonal and equal", "diagonal and unequal",
    "equalvarcov", "identity", "unconstrained", "zero")
  #' Equal var cov sets the off diagonals to be equal and the diagonals
  #' to be equal, but the off diagonals are not equal to the diagonals.
  if (Qom == 0) Q <- "identity"
  if (Qom != 0) Q <- "unconstrained"

  if (Rom == 0) R <- "zero"
  if (Rom != 0) R <- "diagonal and equal"

  # Data
  data <- data.frame("a" = rnorm(tslength), "i" = rnorm(tslength))
  colnames(data) <- c("a", "i")
  # Specify the model
  # "identity", "unconstrained", "diagonal and unequal",
  # "diagonal and equal", "equalvarcov", "zero"
  model <- list(
    # tinitx = 1, # Initial state of time-step 0 (default) or 1
    x0 = "zero",
    V0 = "zero", # default is zero x(0) ~ MVN(x0, V0)
    U = "zero", # b/c the data are z-scored U should ~ == 0
    B = matrix(list("a:a", "i:a", 0, "i:i"), 2, 2),
    # B = matrix(list("B:1,1","B:2,1",0,"B:2,2"),2,2),
    Q = Q, # Proc error ~MVN(0,Q)
    Z = "identity",
    A = "zero", # Data are already standardized
    R = R # Obs error ~MVN(0,R),
    )
  # Fit a MARSS model to the data
  MLEobj <- MARSS::MARSS(y = t(data), inits = NULL,
    model = model,
    method = "kem", form = "marxss",
    fit = TRUE, silent = TRUE, control = NULL, MCbounds = NULL,
    fun.kf = "MARSSkfss")
  # Fixing parameters
  # c("marss", "method", "par")
  MLEobj$par$B[1:length(MLEobj$par$B)] <- Bom
  if (Q != "identity") MLEobj$par$Q[1:length(MLEobj$par$Q)] <- c(1, Qom, 1)
  if (Rom != 0) {
    MLEobj$par$R[1:length(MLEobj$par$R)] <- Rom
  }

  # Simulate
  sim <- suppressWarnings(MARSS::MARSSsimulate(MLEobj, tSteps = tslength,
    nsim = iterations))
  sim$sims <- aperm(sim$sim.data, c(2, 1, 3))

  # dev.new(); matplot(t(sim$sim.data[, , 1]), main = 0.4)
  # print(MARSS(sim$sim.data[, , 1], model = model))
  # print(cov(t(sim$sim.data[, , 1])))
  # print(cor(t(sim$sim.data[, , 1])))

  return(sim)
}
