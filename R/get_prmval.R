#' Get information from a prm file
#'
#' @param text The text string to search for in \code{prm}.
#' @param prm The file to extract information from, read in using
#' \code{readLines}
#'
#' @author Kelli Faye Johnson
#'
get_prmval <- function(text, prm) {

  info <- grep(text, prm, value = TRUE)
  info <- strsplit(info, "[[:space:]]+")
  keep <- data.frame(
    "code" = gsub(text, "", sapply(info, "[[", 1)),
    "value" = sapply(info, "[[", 2),
    stringsAsFactors = FALSE)
  keep$value <- as.numeric(keep$value)

  return(keep)

}
