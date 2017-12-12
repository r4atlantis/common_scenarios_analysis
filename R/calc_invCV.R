#' Calculate inverse of coefficient of variation for a vector
#'
#' @param data
#' @param n_years The number of points to include in the rolling
#' calculation. The calculation uses a "right-aligned" window, where
#' the beginning is filled with NAs if \code{fillNA = TRUE}.
#' @param fillNA A logical value indicating if the return vector
#' should be the same length as the input time series. The default
#' is true, which gives \code{NA} for the left-justified boundary years.
#'
#' @author Kelli Faye Johnson
#' @return A vector of inverse CV biomass
#'
calc_invCV <- function(data, n_years = 10, fillNA = TRUE) {

  # Ensure that there are enough time steps give n_years
  if (length(data) < n_years) {
    stop("The time series is not of length ", n_years,
      "\n check your input data to calc_invCV.")
  }

  .rapply <- function(vec, width, FUN) {
    sapply(seq_along(vec),
      function(i) if (i < width) NA else FUN(vec[i:(i-width+1)]))
  }

  # Checked the rolling mean with zoo::rollmean and it worked
  means <- .rapply(data, width = n_years, FUN = mean)
  sds <- .rapply(data, width = n_years, FUN = sd)

  invcv <- means / sds

  if (length(names(data)) == length(invcv)) names(invcv) <- names(data)

  if (fillNA) {
    return(invcv)
  } else { return(invcv[!is.na(invcv)]) }

}
