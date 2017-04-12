#'
#'
#' @param data
#' @param n_years
#' @param fillNA A logical value indicating if the return vector
#' should be the same length as the input time series. The default
#' is true, which gives \code{NA} for the boundary years.
#'
#' @return A vector of inverse CV biomass
#'
calc_invCV <- function(data, n_years = 10, fillNA = TRUE) {

  # Ensure that there are enough time steps give n_years
  if (length(data) < n_years) {
    stop("The time series is not of length ", n_years,
      "\n check your input data to calc_invCV.")
  }

  # Checked the rolling mean with zoo::rollmean and it worked
  means <- .rapply(data, width = n_years, FUN = mean)
  sds <- .rapply(data, width = n_years, FUN = sd)

  invcv <- means / sds

  if (length(names(data)) == length(invcv)) names(invcv) <- names(data)

  if (fillNA) return(invcv)
    else (return[!is.na(invcv)])

}
