#' Get information from a basic info file for the summary table
#'
#' @param object A basic info file read in from the \code{.csv} file.
#'
#' @author Kelli Faye Johnson
#' @return A data frame.
#'
get_basicinfo <- function(object) {

  name <- deparse(substitute(object))

  object <- object[, c(1, which(object[grep("trophic", object[, 1],
      ignore.case = TRUE), -1] > 1) + 1)]

  ngroups <- NCOL(object) - 1

  end <- data.frame("name" = name, "n" = ngroups,
  "fish" = sum(as.numeric(object[grep("IsFish", object[, 1],
          ignore.case = TRUE), -1]), na.rm = TRUE) / ngroups,
  "surveyed" = sum(as.numeric(object[grep("IsSurveyed", object[, 1],
          ignore.case = TRUE), -1]), na.rm = TRUE) / ngroups,
  "assessed" = sum(as.numeric(object[grep("IsAssessed", object[, 1],
          ignore.case = TRUE), -1]), na.rm = TRUE) / ngroups,
  "targeted" = sum(as.numeric(object[grep("IsTarget", object[, 1],
          ignore.case = TRUE), -1]), na.rm = TRUE) / ngroups
  )

 return(end)

}
