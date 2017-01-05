#' Extract yearly data from Atlantis output
#'
#' todo: place more details here.
#'
#' @param data
#' @param start A numeric value specifying the start year.
#' @param end A numeric value specifying the end year.
#'
#' @author Kelli Faye Johnson
#'
get_years <- function(data, start, end) {
  # 01. Find the "Time" column
  if (!"Time" %in% colnames(data)) {
    get <- 1
    warning("Time is not a column name in your data\n",
      "assuming the first column refers to the time step.")
  } else {get <- grep("Time", colnames(data), ignore.case = TRUE)}
  if (!class(data[, get]) %in% c("integer", "numeric")) {
    stop("The Time column in your data frame is not numeric.")
  }

  # 02. Find the maximum number of years
  max.years <- data[NROW(data), get] / 365
  if (max.years < end) stop("Year range exceeds years in data.")

  # 03. Deal with the fact that not all models have 365 day outputs.
  # Find rows corresponding to the correct years.
  row.start <- min(which(data[, get] / 365 >= start))
  row.end <- min(which(data[, get] / 365 >= end))

  # 04. Extract only year data
  data <- data[row.start:row.end, ]
  keep <- data$Time / 365
  keep <- which(floor(keep) == keep)

  return(data[keep, ])
}
