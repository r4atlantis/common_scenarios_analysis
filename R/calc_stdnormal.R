#' Normalize a vector to N(0,1)
#'
#' @param x A vector of numeric values you wish to standardize
#'
#' @return A vector with a mean of zero and a standard deviation of 1.
#'
calc_stdnormal <- function(x, na.rm = TRUE) {

  if (length(x) <= 1) {
    stop("The length of x is ", length(x), "and must be > 1")
  }
  if (is.array(x)) {
    if(is.na(dim(x)[2])) {
     x <- as.vector(x)
    }
  }
  if (!is.null(dim(x)[2])) {
    stop("calc_stdnormal only works on vectors")
  }

  final <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(final)
}
