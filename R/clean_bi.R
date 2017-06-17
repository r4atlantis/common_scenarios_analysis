#' Check basic information file before writing to disk
#'
#' @param bi
#' @param headers
#'
clean_bi <- function(bi, headers = cols) {

  if (!is.data.frame(bi)) stop("Your basic info object must be a data frame.")
  if (!is.list(headers)) stop("Your headers must be a list.")

  # Assign headers to cn for easier typing.
  cn <- headers

  #### Virgin biomass
  # Check that B_0 is zero only for DIN
  virginB <- bi[cn$virgi, ][, -1]
  test <- which(virginB == 0) + 1
  test <- bi[cn$code, test]
  test <- test[-grep("DC|Carrion", test, ignore.case = TRUE)]
  if (length(test) > 0) message(paste(test, collapse = "\n"),
    " have a Virgin Biomass of 0.")

  #### Trophic level
  test <- grep("bacteria|detritus|DC$|carrion|DIN$", bi[cn$commo, ], ignore.case = TRUE)
  if (length(test) > 0){
    if (any(bi[cn$troph, test] != 0)){
      message("Changing trophic level of bacteria, carrion, and detritus to zero (FIXED).\n")
      bi[cn$troph, test] <- 0
    }}

  #### Pull turtles from guilds
  turtlenames <- "turtle|terrapin|reptile|ridley|loggerhead"
  turtles <- grep(turtlenames, bi[cn$commo, ], ignore.case = TRUE)
  if (length(turtles) > 0){
    message("Found turtles in the model.\n",
      "Setting fish, shark, and mammal guilds to zero for turtles.\n",
      "Note, turtles are not assigned to demersal or pelagic.\n")
    bi[c(fishy, shark, mamma, demer, pelag), turtles] <- 0
  }

  #### Assessed or notAssessed
  # Set Assessed columns to zero for lower level trophic groups
  bi[c(cn$asses, cn$asses + 1), bi[cn$troph, ] < 1] <- 0
  check <- which(bi[cn$troph, ] >= 1)[-1]
  # All others should have a summed value of 1
  test <- bi[c(cn$asses, cn$asses + 1), check]
  test <- apply(test, 2, as.numeric)
  test <- apply(test, 2, sum)
  if (all(test != 1)) {
    message("The following groups have values in both Assessed",
    " and nonAssessed or in neither:\n",
    paste0(bi[cn$commo, which(test != 1)], collapse = "\n"), "\n")
  }

  #### Surveyed
  # If a species is assessed it should have a survey.
  test <- which(bi[cn$asses, ] == 1)
  if (length(test) > 0){
    if (any(bi[cn$surve, test] == 0)) {
      test <- which(bi[cn$surve, ] == 0 & bi[cn$asses, ] == 1)
      message("Some groups are assessed, but have no survey:\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
    }}

  #### USDollar
  if (any(as.numeric(bi[cn$dolla, -1]) < 0, na.rm = TRUE)) {
    message("Check your USDollar values for negative values.")
  }

  #### Guilds
  # If IsFish, then must have L_infinity MaxAge and Vulnerability
  fishes <- bi[, bi[cn$fishy, ] == 1]
  if (any(!fishes[cn$linfi, ] > 0)) {
    message("check L_infinity for IsFish == 1")
  }
  if (any(!fishes[cn$maxag, ] > 0)) {
    message("check MaxAge for IsFish == 1")
  }
  if (any(!fishes[cn$vulne, ] > 0)) {
    message("check vulnerabilities for IsFish == 1")
  }

  # Fix IsPrimaryProducer
  test <- which(bi[cn$troph, ] > 2.0 | bi[cn$troph, ] < 1.0)
  if (length(test) > 0){
    if (any(bi[cn$primp, test] == 1)) {
      test <- test[which(bi[cn$primp, test] == 1)]
      message("The following groups with a TL <1 & >2 were removed",
        "from IsPrimaryProducer (FIXED):\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
      bi[cn$primp, test] <- 0
    }}

  # Fix IsBird
  test <- grep("bird", bi[cn$commo, ], ignore.case = TRUE)
  if (length(test) > 0){
    if (any(bi[cn$birdy, test] != 1)) {
      message("There were some groups that include the word ",
        "bird in their common name\nbut they are not IsBird == 1.\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
    }}

  # Fix IsMammal
  test <- grep("dolphin|whale|[^i]lion|pinniped|orca|otter",
    bi[cn$commo, ], ignore.case = TRUE)
  if (length(test) > 0){
    if (any(bi[cn$mamma, test] != 1)) {
      message("There were some groups that include words ",
        "related to mammals in their common name\n",
        "but they are not IsMammal == 1.\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
    }}

  # Fix IsSquid
  test <- grep("squid",
    bi[cn$commo, ], ignore.case = TRUE)
  if (length(test) > 0){
    if (any(bi[cn$squid, test] != 1)) {
      message("There were some groups that include words ",
        "related to squids in their common name\n",
        "but they are not IsSquid == 1.\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
    }}

  # Fix IsFilter
  test <- grep("filter|bivalve|coral|oyster",
    bi[cn$commo, ], ignore.case = TRUE)
  if (length(test) > 0){
    if (any(bi[cn$filte, test] != 1)) {
      message("There were some groups that include words ",
        "related to filter feeders in their common name\n",
        "but they are not IsFilter == 1.\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
    }}
  test <- grep("^BF", bi[cn$code, ], ignore.case = TRUE)
  if (length(test) > 0){
    if (any(bi[cn$filte, test] != 1)) {
      message("Some groups that have codes starting with BF",
        " which typically related to benthic filter feeders\n",
        "but they are not IsFilter == 1.\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
    }}

  # Fix IsEpibenthos
  test <- grep("shrimp|star|crab|octopus|urchin",
    bi[cn$commo, ], ignore.case = TRUE)
  if (length(test) > 0){
    if (any(bi[cn$benth, test] != 1)) {
      message("There were some groups that include words ",
        "related to epibenthos in their common name\n",
        "but they are not IsEpibenthos == 1.\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
    }}
  test <- grep("^BM", bi[cn$code, ], ignore.case = TRUE)
  if (length(test) > 0){
    if (any(bi[cn$benth, test] != 1)) {
      message("Some groups have codes starting with BM",
        " which typically related to epibenthos\n",
        "but they are not IsEpibenthos == 1.\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
    }}

  # Fix IsZooplankton
  test <- grep("zoopl|pteropod|ctenophore|nettle",
    bi[cn$commo, ], ignore.case = TRUE)
  if (length(test) > 0){
    if (any(bi[cn$zoopl, test] != 1)) {
      message("There were some groups that include words ",
        "related to zooplankton in their common name\n",
        "but they are not IsZooplankton == 1.\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
    }}

  # Fix IsInfauna
  test <- grep("^BC$", bi[cn$code, ], ignore.case = TRUE)
  if (length(test) > 0){
    if (any(bi[cn$infau, test] != 1)) {
      message("There were some groups that have codes related to infauna\n",
        "but they are not IsInfauna == 1.\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
    }}

  #### Fix IsShark
  test <- grep("shark|[^G]ray|skate", bi[cn$commo, ], ignore.case = TRUE)
  if (length(test) > 0){
    if (any(bi[cn$shark, test] != 1)) {
      message("There were some groups that include words ",
        "related to sharks in their common name\n",
        "but they are not IsShark == 1.\n ",
        paste0(bi[cn$commo, test], collapse = "\n "), "\n")
    }}
  # Sharks should also be IsFish
  test <- which(bi[cn$shark, ] == 1)
  if (length(test) > 0){
    bad <- bi[cn$fishy, test]
    bad <- bad[which(bad == 0)]
    if (length(bad) > 0) {
      message("IsShark == 1 but they were not included in IsFish (FIXED)\n ",
        paste0(bi[cn$commo, match(names(bad), colnames(bi))],
          collapse = "\n "), "\n")
      bi[cn$fishy, test] <- 1
    }}

  # Fix IsDemersal and IsPelagic
  # If trophic level is less than one they are not either
  bi[c(cn$pelag, cn$demer), bi[cn$troph, ] < 1] <- 0
  # If they are a primary producer they should not be benthic or pelagic
  bi[c(cn$demer, cn$pelag), which(bi[cn$primp, ] == 1)] <- 0
  # If they are a bird or  mammal they are not benthic or pelagic
  bi[c(cn$pelag, cn$demer), which(bi[cn$birdy, ] == 1)] <- 0
  bi[c(cn$pelag, cn$demer), which(bi[cn$mamma, ] == 1)] <- 0

  test <- bi[cn$pelag, bi[cn$zoopl, ] == 1 & bi[cn$troph, ] >=2]
  if (any(test != 1)) {
    message("Fix consumer zooplankton to be pelagic.")
  }
  keep <- bi[, bi[cn$troph, ] >= 2 & bi[cn$mamma, ] == 0 & bi[cn$birdy, ] == 0]
  keep <- keep[, !grepl(turtlenames, keep[cn$commo, ], ignore.case = TRUE)]
  bad <- apply(keep[c(cn$pelag, cn$demer), -1], 2,
    function(x) sum(as.numeric(x)))
  bad <- bad[bad != 1]
  if (length(bad) > 0) {
    message("The following guilds are not assigned to \n",
      "demersal or pelagic, or are both:\n ",
      paste0(keep[cn$commo, match(names(bad), colnames(keep))],
        collapse = "\n "), "\n")
  }
  # todo: check corals are demersal and look at Guam to see guild.

  keep <- bi[, bi[cn$shark, ] == 1]
  if (any(keep[cn$fishy, ] != 1)) {
    message("Not all sharks are in the fish guild.")
  }
  keep <- keep[c(cn$commo, cn$shark, cn$mamma, cn$birdy, cn$squid,
    cn$filte, cn$benth, cn$zoopl, cn$primp, cn$infau), ]
  if (any(apply(keep[-1, ], 2, function(x) sum(as.numeric(x))) != 1)) {
    message("Some of the sharks are part of more guilds than shark and fish.")
  }

  keep <- bi[, which(bi[cn$troph, ] >= 1)[-1]]
  keep <- keep[,
    !grepl(turtlenames, keep[cn$commo, ], ignore.case = TRUE)]
  keep <- keep[c(cn$commo, cn$fishy, cn$mamma,
    cn$birdy, cn$squid, cn$filte, cn$benth, cn$zoopl, cn$primp, cn$infau), ]
  bad <- apply(keep[-1, ], 2, function(x) sum(as.numeric(x)))
  bad <- bad[which(bad != 1)]
  if (length(bad) > 0) {
    message("The following groups are in no guild or more than one guild:\n ",
      paste0(keep[1, match(names(bad), colnames(keep))], collapse = "\n "),
      "\n")
  }

  #### Target
  # I assume that all groups with a dollar value are targeted, but nothing
  # is changed, I only output messages.
  test <- t(bi[, -1])
  colnames(test) <- bi[, 1]
  keep <- test[, grep("dollar", colnames(test), ignore.case = TRUE)] > 0
  strong <- test[keep, ]
  whichbad <- which(as.numeric(strong[, "IsTarget"]) != 1)
  if (length(whichbad) > 0) {
    message("The following groups have USDollar > 0, but are not targeted:\n ",
      paste(strong[whichbad, grep("common", colnames(strong),
        ignore.case = TRUE)], collapse = "\n "), "\n")
  }
  keep <- test[, grep("dollar", colnames(test), ignore.case = TRUE)] <= 0
  strong <- test[keep, ]
  whichbad <- which(as.numeric(strong[, "IsTarget"]) == 1)
  if (length(whichbad) > 0) {
    message("The following groups are targeted, but have USDollar = 0:\n", paste(strong[whichbad, grep("common", colnames(strong),
      ignore.case = TRUE)], collapse = "\n"), "\n")
  }

  invisible(bi)
}
