#' Calculate indicators based on Atlantis biomass and catch text-file output
#'
#' Import time series of biomass and catch (by species) and return a
#' list of ecosystem indicators, given flags for species groups as detailed
#' in the return from \code{get_lookup()}.
#' @author Gavin Fay
#'
#' @param bio A dataframe of biomass time series, such as that
#' obtained from reading in \code{BiomIndx.txt}.
#' @param cat A dataframe of catch time series, such as that
#' obtained from reading in \code{Catch.txt}.
#' @param lookup A list of flags and summary stats by species.
#' This is the list returned from \code{get_lookup()}.
#' @param overfishedproportion A numeric value indicating the proportion below
#' $B_target$ which a group is considered overfished. The default is 0.5.
#' If the target depletion level is 0.4, then 0.4 * 0.5 = 0.2, and
#' \code{get_indicators} will assess if the groups current level of depletion
#' is below 0.2 or not.
#'
#' @return Returns a list of time series of ecosystem indicators.
#'
get_indicators <- function(bio, cat, lookup,
  overfishedproportion = 0.5) {

  #### Helper functions
  .reshape <- function(x, name = "Biomass") {
    if (any(duplicated(x$Time))) {
      stop("Found duplicated 'Time' values in ", name,
        "\n  data while running get_indicators")
    }
    code <- colnames(x)[-1]
    colnames(x)[-1] <- paste0(name, ".", code)
    rows <- 1:((length(unique(x$Time)) + 1) * (NCOL(x) - 1))
    x <- reshape(x, direction = "long",
      varying = list(2:NCOL(x)), idvar = "Time", v.names = name,
      timevar = "Code", times = code, new.row.names = rows)
    return(x)
  }

  .sum <- function(x, subby, column) {
    if (length(unique(x[, column])) == 1) NA
    if (sum(x[, subby], na.rm = TRUE) > 0) {
    for (int in subby) {
      x <- x[x[, int] == 1, ]
    }
    with(x, tapply(eval(parse(text = column)), Time,
        sum, na.rm = TRUE))
    } else NA
  }

  .missingyear <- function(x, years, fill = 0) {
    xnames <- names(x)
    missing <- years[!years %in% xnames]
    x <- c(x, rep(fill, length(missing)))
    names(x) <- c(xnames, missing)
    x <- x[order(as.numeric(names(x)))]
    return(x)
  }

  #### Manipulate the data
  # Get NPP, which requires lower TL than what I want to include
  small <- clean_data(bio, cat, lookup, minTL = 0.99999)
  temp <- merge(merge(
    .reshape(x = small$biomass, name = "Biomass"),
    .reshape(x = small$catch, name = "Catch"),
    by = c("Time", "Code"), all.x = TRUE),
    lookup,
    by.x = "Code", by.y = "Atlantis species code",
    all.x = TRUE, all.y = FALSE)
  npp <- .sum(temp, "IsPrimaryProducer", "Biomass")

  # turn biomass and catch data into long form after subsetting
  # for those species in the lookup table because there
  # can be additional columns.
  use <- clean_data(bio, cat, lookup, minTL = 1.0)
  bio <- use$biomass; cat <- use$catch; lookup <- use$lookup

  # Find out what groups are fished
  fishedgroups <- apply(
    cat[, !grepl("time", colnames(cat), ignore.case = TRUE)],
    2, sum, na.rm = TRUE)
  fishedgroups <- fishedgroups[fishedgroups > 0]
  nfishedgroups <- length(fishedgroups)

  # merge data.frames
  # If there is catch but no associated biomass it will be left out
  results <- merge(
    .reshape(x = bio, name = "Biomass"),
    .reshape(x = cat, name = "Catch"),
    by = c("Time", "Code"), all.x = TRUE)

  results <- merge(results, lookup,
    by.x = "Code", by.y = "Atlantis species code",
    all.x = TRUE, all.y = FALSE)
  # Fix Catch of NA
  results$Catch[is.na(results$Catch)] <- 0

  #make some variables easier to reference
  colnames(results)[grep("VirginBiomass", colnames(results))] <- "Bzero"
  colnames(results)[grep("target reference", colnames(results))] <- "Btarget"
  colnames(results)[grep("vulnerability", colnames(results))] <- "vulnerability"
  colnames(results)[grep("infinity", colnames(results), ignore.case = TRUE)] <- "L_infinity"
  colnames(results)[grep("maxage", colnames(results), ignore.case = TRUE)] <- "MaxAge"

  # Depletion and target status
  # Depletion is the current depletion level for that time step.
  results$Depletion <- results$Biomass / results$Bzero
  results$IsBelowTarget <- with(results, ifelse(Depletion < Btarget, 1, 0))
  results$IsOverfished <- with(results,
    ifelse(Depletion < overfishedproportion * Btarget, 1, 0))
  results$IsNotOverfished <- with(results,
    ifelse(Depletion >= overfishedproportion * Btarget, 1, 0))

  # Find exploited species
  exploitedspp <- unique(results[results$Catch > 0 & !is.na(results$Catch), "Code"])

  #############################################################################
  #### Indicator data frame
  ind <- data.frame(Time=sort(unique(results$Time)))
  ind$Totbio <- with(results, tapply(Biomass, Time, sum, na.rm = TRUE))
  ind$Totcat <- with(results, tapply(Catch, Time, sum, na.rm = TRUE))

  # Calculate the most basic indicators first.
  ind$Tarbio <- .sum(results, "IsTarget", "Biomass")
  ind$Surbio <- .sum(results, "IsSurveyed", "Biomass")
  ind$Surtarbio <- .sum(results, c("IsTarget", "IsSurveyed"), "Biomass")
  ind$Tarcat <- .sum(results, c("IsTarget"), "Catch")
  ind$Fishbio <- .sum(results, "IsFish", "Biomass")
  ind$Fishsurbio <- .sum(results, c("IsFish", "IsSurveyed"), "Biomass")
  ind$Fishtarbio <- .sum(results, c("IsFish", "IsTarget"), "Biomass")
  ind$Fishtarcat <- .sum(results, c("IsFish", "IsTarget"), "Catch")
  ind$Nppbio <- npp
  ind$Fishcat <- .sum(results, "IsFish", "Catch")

  # Perform some tests before continuing
  if (sum(ind$Fishbio, na.rm = TRUE) <= 0) {
    stop("Fishbio == 0, do you really have no fish in your model?")
  }
  # Check that there is one "IsSurveyed" measurement per time step
  if (length(unique(results[results$IsSurveyed == 1, "Time"])) != NROW(ind)) {
    stop("There is not at least one surveyed biomass per time step")
  }

  #############################################################################
  # Calculate indicators from Gavin's code
  ind$Bird <- .sum(results, "IsBird", "Biomass")
  ind$Foragefish <- .sum(results, "IsForageFish", "Biomass")
  ind$Mammal <- .sum(results, "IsMammal", "Biomass")
  ind$Dem <- .sum(results, "IsDemersal", "Biomass")
  ind$Pel <- .sum(results, "IsPelagic", "Biomass")
  ind$Shark <- .sum(results, "IsShark", "Biomass")
  # todo: customization here, perhaps a new flag,
  # some will have some shark species or possibly even endangered fish
  ind$Teps <- ind$Bird + ind$Mammal
  # KFJ: made a check to see if the "REP" species was already counted
  # in Birds or Mammals. REP stands for reptiles in what would be
  # threatened, endangered, and protected species
  if (any(results$Code == "REP")) {
    codes <- which(results$Code == "REP")
    codes <- as.numeric(names(which(apply(results[codes, c("IsMammal", "IsBird")], 1, sum) == 0)))
    if (length(codes) > 0) {
      ind$Teps <- ind$Teps + with(results[codes, ],
        tapply(Biomass, Time, sum, na.rm = TRUE))
    }
  }

  # Demersal to pelagic ratios
  ind$Dem_pel <- ind$Dem / ind$Pel
  ind$FishDem_pel <-
    .sum(results, c("IsFish", "IsDemersal"), "Biomass") /
    .sum(results, c("IsFish", "IsPelagic"), "Biomass")

  # Catches
  ind$Demcat <- .sum(results, "IsDemersal", "Catch")
  ind$Pelcat <- .sum(results, "IsPelagic", "Catch")
  ind$Sharkcat <- .sum(results, "IsShark", "Catch")

  # Exploitation rates
  ind$Exprate <- ind$Totcat / ind$Tarbio
  # KFJ: Fishexprate should be of just the targeted fish, and not
  # of all fish. For some models not all "IsFish" are "IsTarget"
  ind$Fishexprate <- ind$Fishcat / ind$Fishtarbio

  ind$Prop_of <- .sum(results, "IsAssessedByFisheriesAssessment", "IsOverfished") /
    sum(lookup$IsAssessedByFisheriesAssessment)
  # KFJ: Fixed the Prop_belowtarget for those times when you have a time
  # series of data, and, thus, the new output will have multiple years
  # and provides a single indicator for each time rather than one value
  # repeated over time.
  ind$Prop_belowtarget <- .sum(results,
    "IsAssessedByFisheriesAssessment", "IsBelowTarget") /
    sum(lookup$IsAssessedByFisheriesAssessment)
  ind$Prop_abovetarget <- (1 - ind$Prop_belowtarget)
  ind$Value <- with(results, tapply(Catch * USDollarsPerTon, Time, sum, na.rm = TRUE))

  # NPP ratios
  ind$Bio_pp <- ind$Totbio / ind$Nppbio
  ind$Dembio_pp <- ind$Dem / ind$Nppbio
  ind$Pelbio_pp <- ind$Pel /ind$Nppbio
  ind$Cat_pp <- ind$Totcat / ind$Nppbio
  ind$Demcat_pp <- ind$Demcat / ind$Nppbio
  ind$Pelcat_pp <- ind$Pelcat / ind$Nppbio

  #############################################################################
  #### IndiSeas Indicators
  # Biomass stability
  ind$invcvtot <- calc_invCV(ind[, c("Totbio")], n_years = 10, fillNA = TRUE)
  ind$invcvsur <- calc_invCV(ind[, c("Surbio")], n_years = 10, fillNA = TRUE)

  # Fish size
  # Weighted maximum length of fish in the community
  # IndiSeas used individual fish
  results$MaxLinfWeighted <- with(results, Biomass * L_infinity)
  ind$mlgfishtot <- .sum(results, "IsFish", "MaxLinfWeighted") / ind$Fishbio
  ind$mlgfishsur <- .sum(results, c("IsFish", "IsSurveyed"), "MaxLinfWeighted") / ind$Fishsurbio

  # Life span
  # Of the surveyed community
  results$MaxAgeWeighted <- with(results, Biomass * MaxAge)
  ind$mlsfishtot <- .sum(results, c("IsFish"), "MaxAgeWeighted") / ind$Fishbio
  ind$mlsfishsur <- .sum(results, c("IsFish", "IsSurveyed"), "MaxAgeWeighted") / ind$Fishsurbio

  # Predators
  # Proportion of predatory fish surveyed to the survey biomass
  subbed <-
    results[results$TrophicLevel >= 3.5 &
            results$IsFish == 1, ]
  if (NROW(subbed) > 0) {
    ind$pftot <- with(subbed,
      tapply(Biomass, Time, sum, na.rm = TRUE)) / ind$Totbio
  } else {ind$pftot <- NA}
  subbed <-
    results[results$TrophicLevel >= 3.5 &
            results$IsFish == 1 &
            results$IsSurveyed == 1, ]
  if (NROW(subbed) > 0) {
    ind$pfsur <- with(subbed,
      tapply(Biomass, Time, sum, na.rm = TRUE)) / ind$Surbio
  } else {ind$pfsur <- NA}

  # Sustainable stocks
  # Find the number of groups that have at least some catch
  nonfullyexploitedlevel <- 0.6

  if (length(fishedgroups) > 0) {
    temp <- results[
        results$Code %in% names(fishedgroups) &
        results$Depletion > nonfullyexploitedlevel, ]
    ind$sstot <- with(temp,
      tapply(Code, Time, function(x) length(unique(x)))) /
      nfishedgroups
    temp <- temp[
      temp$IsAssessedByFisheriesAssessment == 1 |
      temp$IsSurveyed == 1, ]

      if (NROW(temp) > 0) {
        ind$sssur <- .missingyear(with(
          temp,
          tapply(Code, Time, function(x) length(unique(x)))) /
          nfishedgroups, ind$Time)
        } else {ind$sssur <- 0}
  } else {
    ind$sstot <- 0
    ind$sssur <- 0
  }
  # Old calculation for just targeted species
  # ind$sstot <- .sum(results, c("IsTarget"), "IsNotOverfished") /
  #   length(unique(results[results$IsTarget == 1, "Code"]))
  # ind$sssur <- .sum(results, c("IsSurveyed", "IsTarget"), "IsNotOverfished") /
  #   length(unique(results[results$IsTarget == 1 & results$IsSurveyed == 1, "Code"]))

  # Trophic level
  ind$mtlbio <- with(results,
    tapply(Biomass * TrophicLevel, Time, sum, na.rm = TRUE)) /
    ifelse(ind$Totbio == 0, NA, ind$Totbio)
  ind$mtlcat <- with(results,
    tapply(Catch * TrophicLevel, Time, sum, na.rm = TRUE)) /
    ifelse(ind$Totcat == 0, NA, ind$Totcat)
  ind$mtlsur <- with(results[results$IsSurveyed == 1, ],
    tapply(Biomass * TrophicLevel, Time, sum, na.rm = TRUE)) /
    ifelse(ind$Surbio == 0, NA, ind$Surbio)

  # Mean vulnerability
  ind$mivfishbio <- with(
    results[results$IsFish == 1, ],
    tapply(Biomass * vulnerability, Time, sum, na.rm = TRUE)) /
    ifelse(ind$Fishbio <= 0, NA, ind$Fishbio)
  if (sum(ind$mivfishbio, na.rm = TRUE) == 0) ind$mivfishbio <- NA
  temp <- with(
    results[results$Catch > 0 & results$IsFish == 1, ],
    tapply(Catch * vulnerability, Time, sum, na.rm = TRUE))
  temp <- .missingyear(temp, names(ind$mivfishbio))
  ind$mivfishcat <- temp / ifelse(ind$Fishcat <= 0, NA, ind$Fishcat)
  rm(temp)
  if (sum(ind$mivfishcat, na.rm = TRUE) == 0) ind$mivfishcat <- NA

  # Non-declining species
  # Kleisner et al. 2015 Eco. Ser. 16: 413-419
  # KFJ: Emailed to see how to calc the significance portion of the equation.
  ind$ndesbio <- .missingyear(
    calc_ndes(results[results$Catch > 0, ]), names(ind$mivfishbio), fill = NA)
  if (sum(results$Catch > 0 & results$IsSurveyed == 1) == 0) {
    ind$ndessurcat <- NA
  } else {
    ind$ndessurcat <- .missingyear(calc_ndes(
      results[
        results$Catch > 0 &
        results$IsSurveyed == 1, ]), names(ind$mivfishbio), fill = NA)
  }

  # Trophic index
  # only groups with a trophic level of greater than 3.25
  ind$mtibio <- with(results[results$TrophicLevel >= 3.25, ],
    tapply(Biomass * TrophicLevel, Time, sum, na.rm = TRUE)) /
    ifelse(ind$Totbio <= 0, NA, ind$Totbio)
  if (sum(ind$mtibio, na.rm = TRUE) == 0) ind$mtibio <- NA

  ind$mticat <- with(results[results$TrophicLevel >= 3.25, ],
    tapply(Catch * TrophicLevel, Time, sum, na.rm = TRUE)) /
    ifelse(ind$Totcat <= 0, NA, ind$Totcat)
  if (sum(ind$mticat, na.rm = TRUE) == 0) ind$mticat <- NA

  # Inverse fishing pressure
  ind$invFbio <- ind$Totbio / ifelse(ind$Totcat <= 0, NA, ind$Totcat)
  ind$invFsur <- ind$Surbio / ifelse(ind$Totcat <= 0, NA, ind$Totcat)

  # q90 <- rbind(by(results[results$IsSurveyed == 1, ],
  #   list(results$Time[results$IsSurveyed == 1]),
  #   function(x) calc_q90(x$Biomass, x$Bzero)))
  # ind <- merge(ind,
  #   data.frame("Time" = as.numeric(dimnames(q90)[[2]]), "q90" = c(q90)),
  #   all.x = TRUE)

  return(ind)
}

