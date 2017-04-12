#' Helper functions used for indicator calculations
#'
#' @author Kelli Faye Johnson
.rapply <- function(vec, width, FUN) {
  sapply(seq_along(vec),
    function(i) if (i < width) NA else FUN(vec[i:(i-width+1)]))
}
