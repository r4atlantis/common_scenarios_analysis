#' List parameters necessary for MARSS
#'
#' Create a list of matrices containing the proper naming convention for
#' \code{\link{MARSS}} models. The function takes two integer values,
#' one for the number of states and one for the number of observations.
#'
#' @param m The number of states in the model
#' @param n The number of input time series.

get_parmat <- function(m = 2, n = 2) {

  out <- list(
    "B" = matrix(rep(0, m^2), m, m),
    "U" = matrix(rep(0, m), m, 1),
    "Q" = diag(m),
    "Z" = do.call("rbind", lapply(1:(n/2), function(x) return(diag(m)))),
    "A" = matrix(rep(0, n * 2), n, 1),
    "R" = matrix(rep(0, n^2), n, n),
    "x0" = matrix(rep(0, m), m, 1),
    "V0" = matrix(rep(0, m^2), m, m)
    )

  return(out)

}
