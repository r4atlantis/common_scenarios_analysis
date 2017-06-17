#' Extract B parameter estimates
#'
#' @param data
#'

get_bpars <- function(data) {
  rows <- grepl("B\\.", data$par)
  data <- data[rows, ]
  data$par <- gsub("B\\.", "", data$par)
  names <- strsplit(data$par, ":")
  data$a <- sapply(names, "[[", 1)
  data$b <- sapply(names, "[[", 2)
  names <- sapply(names, function(x) ifelse(x[1] == x[2], FALSE, TRUE))

  return(data[names, ])
}
