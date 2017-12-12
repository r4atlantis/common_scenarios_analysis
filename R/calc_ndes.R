#' Proportion of non-declining species over a ten-year time window.
#'
#' The function calculates the proportion of non-declining species using
#' Kendall's tau to assess if trends are of the same sign. If Kendall's tau
#' is larger than or equal to zero then the species is non declining.
#' The original method was proposed by Lynam *et al*. (2010) and was later
#' used in the *IndiSeas* project by Kleisner *et al*. (2015).
#'
#' @param data
#' @param time A character value supplying the column name containing
#' the temporal information. The default is \code{"Time"}.
#' @param width
#' @param spp A character value supplying the column name containing
#' the Atlantis group information. The default is \code{"Code"}.
#' @param metric A character value supplying the column name containing
#' the metric you wish to calculate over. The default is \code{"Biomass"}.
#'
#' @author Kelli Faye Johnson
#'
calc_ndes <- function(data, time = "Time", width = 10,
  spp = "Code", metric = "Biomass") {

  data_all <- data[order(data[, time]), ]
  data_all_times <- unique(data_all[, time])
  ndes <- rep(NA, length(data_all_times))
  names(ndes) <- data_all_times

  # If there are more than 10 unique times, then calculate the
  # metric on a moving time window of ten years
  for (it_time in seq_along(data_all_times)) {
    if (it_time < width) next

    data <- data_all[data_all[, time] %in%
      data_all_times[(it_time - width + 1):it_time], ]

    # Determine for a given time interval a tau for each spp.
    unique_spp <- unique(data[, spp])
    taus <- data.frame("spp" = unique_spp, "tau" = NA)

    for (it_spp in seq_along(unique_spp)) {
        use <- data[data[, spp] == unique_spp[it_spp], c(time, metric)]
        if (length(unique(use[, metric])) == 1) {
          taus[it_spp, 2] <- 0
          next
        }
        taus[it_spp, 2] <- cor(use, method = "kendall")[2, 1]
      }

    ndes[it_time] <- sum(taus$tau >= 0) / NROW(taus)
  }

  return(ndes)

}
