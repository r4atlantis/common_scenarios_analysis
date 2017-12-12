#'
#'
#' @param biomass
#' @param catch
#' @param lookup
#' @param minTL
#'
clean_data <- function(biomass = NULL, catch = NULL, lookup = NULL,
  minTL = 1.0) {

  # Lookup
  # 01 - Remove duplicated species
  # 02 - Remove species with TL <= minTL
  # 03 - Remove bacteria, detritus, and DIN if not already removed.
  if (!is.null(lookup)) {
    # Remove duplicated species in the lookup
    dups <- duplicated(lookup$"Atlantis species code")
    lookup <- lookup[!dups, ]
    # Remove trophic levels <= 1
    lows <-
      lookup[, grep("Trophic", colnames(lookup), ignore.case = TRUE)] <= minTL
    lookup <- lookup[!lows, ]
    commo <- grep("Common", colnames(lookup), ignore.case = TRUE)
    yuck <- lookup[grep("bacteria|detritus|^DIN",
      as.character(lookup[, commo]),
      ignore.case = TRUE), ]
    if (dim(yuck)[1] > 0) {
      lookup <- lookup[-match(yuck[, 1], lookup[, 1]), ]
    }
  }

  # Biomass and catch data
  # 01 - Remove species that are not in the lookup
  # 02 - Remove duplicated species
  # 03 - Remove duplicated time steps
  for (it_data in c("biomass", "catch")) {
    temp <- get(it_data)
    if (is.null(temp)) next
    if (!is.null(lookup)) {
      # Remove columns without species codes
      bad <- colnames(temp) %in% c("Time", as.character(lookup$"Atlantis species code"))
      temp <- temp[, bad]
    }
    # Remove duplicated species
    colnames(temp) <- gsub("\\.[0-9]+$", "", colnames(temp))
    dups <- duplicated(colnames(temp))
    if (any(dups)) {
      dups_names <- names(which(table(colnames(temp)) > 1))
      for (dups_it in dups_names) {
        duplicated <- temp[, colnames(temp) %in% dups_it]
        duplicated_sums <- apply(duplicated, 2, sum, na.rm = TRUE)
        temp <- temp[, -which(colnames(temp) == dups_it)[-which.max(duplicated_sums)]]
        colnames(temp) <- gsub("\\.[0-9]+$", "", colnames(temp))
      }
    }
    # Remove duplicated time steps
    bad <- names(which(table(temp$Time) > 1))
    if (length(bad) > 0) {
      remove <- NULL
      for (bad_it in bad) {
        remove <- c(remove, which(temp$Time %in% bad_it)[-1])
      }
      temp <- temp[-remove, ]
    }
    assign(it_data, temp)
  }

  return(list("biomass" = biomass, "catch" = catch, "lookup" = lookup))
}


