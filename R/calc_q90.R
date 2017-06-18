#'
#'
#' @param biomass
#' @param virgin
#' @param threshold
#' @param tile
#' @param n_bins
#' @param type
#'
#'
calc_q90 <- function(biomass, virgin = NULL, threshold = 0.2,
  tile = 0.1, n_bins = 100, type = c("log", "linear")) {

  type <- match.arg(arg = type, several.ok = FALSE)

  # Remove NA
  biomass[is.na(biomass)] <- 0
  if (!is.null(virgin)) {
    if (length(virgin) != length(virgin)) {
      stop("The biomass and virgin are different lengths: ", length(biomass),
        " and ", length(virgin), ", respectively.")
    }
    use <- biomass[!biomass < (threshold * virgin)]
  } else { use <- biomass}

  b <- length(use)

  # Log
  if (type == "log") {
    lb <- tile * b
    ub <- (1 - tile) * b

    maxbiomass <- max(use, na.rm = TRUE)
    logbinsize <- maxbiomass * (0.9^(n_bins - seq(1, n_bins, by = 1)))
    if (any(is.na(logbinsize))) {
      stop("NAs found in logbinsize: ", paste(logbinsize, collapse = ", "))
    }
    table_bin <- table(
      findInterval(x = unlist(use), vec = logbinsize, all.inside = TRUE))
    bin <- rep(0, n_bins)
    bin[as.numeric(names(table_bin))] <- table_bin

    sumbin <- cumsum(bin)
    whichlb <- which(sumbin > lb)[1]
    whichub <- which(sumbin > ub)[1]

    if (length(whichlb) == 0 | length(whichub) == 0) {
      stop("The lower or upper bounds, respectively, were not found in",
       "calc_q90 ", whichlb, " ", whichub)
    }

    snR <- sum(bin[whichlb:whichub])
    nR1 <- bin[whichlb] / 2
    nR2 <- bin[whichub] / 2
    R1 <- logbinsize[whichlb] * 0.5
    R2 <- logbinsize[whichub] * 0.5
  }

  # Linear
  if (type == "linear") {
    stop("linear is not yet implemented in calc_q90.")
  }

  q90 <- (nR1 + nR2 + snR) / (log(R2 / R1))
  return(q90)

}
