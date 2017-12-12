

# Setup libraries
library(doParallel, quietly = TRUE, verbose = FALSE)
library(foreach, quietly = TRUE, verbose = FALSE)
library(forecast, quietly = TRUE, verbose = FALSE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(MARSS, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(pander, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(rfishbase, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

# Setup initial inputs
dir.main <- file.path(paste0(letters, ":"),
  file.path("atlantis", "common_scenarios_analysis"))
dir.main <- dir.main[file.exists(dir.main)]
if (length(dir.main) > 1) stop(length(dir.main),
  " atlantis/common_scenarios_analysis directories were found.")
dir.data <- file.path(dir.main, "common_scenarios_data")
dir.results <- file.path(dir.main, "results")
dir.create(dir.results, showWarnings = FALSE)
# Source the files to run the analysis
ignore <- sapply(
  dir(file.path(dir.main, "R"), full.names = TRUE, pattern = "\\.r|\\.R"),
  source)
setwd(dir.main)

basicinfo <- FALSE
my.name <- "testingindicators"
my.year.start <- 20
my.year.end <- 49
my.tslength <- 30
my.mgmt.threshold <- my.mgmt.overfished <- 0.5
my.nparallel <- 4
my.theme <- plot_theme()

#### Clean up BasicInfo files for the common scenarios data set.
# ICK also used these files, but he did a manual clean up.

#### Guidelines for creating the BasicInfo file
#' 3. Trophic levels were visually checked and all bacteria and detritus were
#' set to zero. Meiobenthos will be specific to an ecosystem because it is
#' really a classification of size rather than guild, and can include
#' benthic organisms that range from 1-5.
#' Bacteria == 0
#' Carrion == 0
#' Detritus == 0
#' DIN == 0
#' Algae == 1
#' Coccolithophore == 1
#' Dinoflagellates == 1
#' Microphytobenthos == 1
#' Phytoplankton == 1
#' Seagrass == 1
#' Benthic Carnivore == 2
#' Benthic Feeder == 2
#' Bivalves == 2
#' Corals == 2
#' Pteropods == 2
#' Sardine == 2
#' Sea star == 2
#' Shrimp == 2
#' Zooplankton == 2
#' 4. B_0: Carrion should have a B_0 == 0.
#' Best to trust the user supplied the virgin biomass.
#' There were two values in the CalCu that I
#' thought were suspicious, predators and chinook salmon, both of these groups
#' really do go extinct with no fishing.
#' 5. Management: not verified b/c set to 0.5
#' 6. IsAssessed:
#' All trophic levels < 1 are neither assessed or nonAssessed.
#' Check for IsAssessed + IsNonAssessed except for TL < 1 == 1.
#' Check that all IsAssessed species have a survey and warn.
#' 7. IsNonAssessed: set to opposite of IsAssessed except for TL == 0
#' 8. USDollar
#' Check to make sure all values are positive.
#' 9. L_infinity
#' 10. MaxAge
#' 11. IntrinsicVulnerability
#' 9-11 are checked for values > 0 for IsFish == 1
#' 12. IsSurveyed
#' 13. IsFish
#' 14. IsPredatoryFish: not verified b/c we use trophic level of 3.5
#' 15. IsPelagic:
#' 16. IsDemersal
#' 17. IsPrimaryProducer:
#' All groups with a trophic level of 1-<2 should be a primary producer.
#' 18. IsBird:
#' search for the word bird in the common name
#' 19. IsMammal
#' search for common marine mammal names in the common name
#' 20. IsJellyfish: not verified.
#' 21. IsForageFish: not verified
#' 22. IsKrill: not verified
#' 23. IsShark:
#' Includes skates, rays, sharks, dogfish
#' 24. IsSquid:
#' search for squid in common names.
#' 25. IsFilterFeeder:
#' Check for BF in code and filter in common name.
#' 26. IsEpibenthos:
#' Check for BM in code and deposit, star, crab, octopus, urchin, shrimp
#' 27. IsZooplankton:
#' Check for zooplankton and pteropod
#' 28. IsInfauna are animals living in the sediments of the ocean floor
#' 29. IsPiscivore: not verified.
#' 30. IsTarget:
#' Must have USDollar value, throws a warning if dollar value but not targeted,
#' although this is okay.

# Find the the correct place holders
code <- 1
commo <- 2
troph <- 3
virgi <- 4
asses <- 6
dolla <- 8
linfi <- 9
maxag <- 10
vulne <- 11
surve <- 12
fishy <- 13
pelag <- 15
demer <- 16
primp <- 17
birdy <- 18
mamma <- 19
shark <- 23
squid <- 24
filte <- 25
benth <- 26
zoopl <- 27
infau <- 28
targe <- 30
notverified <- c(5, 14, 20:22, 27)
cols <- list("code," = code, "commo" = commo, "troph" = troph,
  "virgi" = virgi, "asses" = asses, "dolla" = dolla, "linfi" = linfi,
  "maxag" = maxag, "vulne" = vulne, "surve" = surve, "fishy" = fishy,
  "pelag" = pelag, "demer" = demer, "primp" = primp, "birdy" = birdy,
  "mamma" = mamma, "shark" = shark, "squid" = squid, "filte" = filte,
  "benth" = benth, "zoopl" = zoopl, "infau" = infau, "targe" = targe)

if (basicinfo) {
table1 <- NULL

#### AustSE
aus_file <- "AustSE_BasicInfo.csv"
aus_file_2 <- "AustSE_DynEffort_BasicInfo.csv"
aus_file_update <- "AustSE_updated.csv"

# Reading in the file as emailed
aus <- read.table(file.path(dir(pattern = "_data", full.names = TRUE),
  aus_file), sep = ",", header = FALSE, stringsAsFactors = FALSE)
aus_update <- read.table(file.path(dir(pattern = "_data", full.names = TRUE),
  aus_file_update), sep = ",", header = FALSE, stringsAsFactors = FALSE)

aus[birdy, grep("Warehou", aus[commo, ])] <- 0
aus[pelag, grep("Warehou", aus[commo, ])] <- 1
aus[benth, grep("Prawn", aus[commo, ])] <- 1

# Get survey updates from BF
update <- grep("IsSurveyed_InReality", aus_update[, 1])
aus_update[update, -1] <- ifelse(aus_update[update, -1] >= 3, 1, 0)
aus_update <- aus_update[c(1, update), ]
aus[surve, match(aus_update[1, ], aus[code, ])[-1]] <- aus_update[2, -1]

# Write the AustSE file to the disk
aus <- clean_bi(aus)
write.table(x = aus,
  file = file.path(dir(pattern = "_data", full.names = TRUE), aus_file),
  append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(x = aus,
  file = file.path(dir(pattern = "_data", full.names = TRUE), aus_file_2),
  append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
table1 <- rbind(table1, get_basicinfo(aus))

#### California Current: CalCu
calcu_file <- dir(pattern = "CalCu_sent20171016.csv", recursive = TRUE,
  full.names = TRUE)

calcu <- read.table(calcu_file, sep = ",", header = FALSE,
  stringsAsFactors = FALSE)

# Write the CalCu file to the disk
calcu <- clean_bi(calcu)
write.table(x = calcu,
  file = file.path(dir(pattern = "_data", full.names = TRUE), "CalCu_BasicInfo.csv"),
  append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
table1 <- rbind(table1, get_basicinfo(calcu))

#### CAM
cam_file <- "CAM_sent20170317.csv"

# Reading in the file as emailed
cam <- read.table(file.path(dir(pattern = "_data", full.names = TRUE),
  cam_file), sep = ",", header = FALSE, stringsAsFactors = FALSE)

# Fix IsPelagic
# Change zooplankton to pelagic and benthic thing to demersal
cam[pelag, grep("zooplankton+", cam[commo, ])] <- 1
cam[demer, grep("^BO$", cam[code, ])] <- 1

# Fix nonAssessed
cam[asses + 1, grep("PWN", cam[code, ])] <- 0
cam[asses + 1, grep("^BO$", cam[code, ])] <- 1

# Fix guilds
cam[filte, grep("FPS", cam[code, ])] <- 0
cam[filte, grep("FMM", cam[code, ])] <- 0
cam[filte, grep("FPL", cam[code, ])] <- 0
cam[benth, grep("BFF", cam[code, ])] <- 0
cam[infau, grep("^BB$", cam[code, ])] <- 0
cam[infau, grep("^MB$", cam[code, ])] <- 0
cam[infau, grep("BFD$", cam[code, ])] <- 0
cam[infau, grep("BFS", cam[code, ])] <- 0
cam[benth, grep("^BC$", cam[code, ])] <- 0
cam[infau, grep("^BC$", cam[code, ])] <- 1

# Dinoflagellates as Zooplankton not PP
cam[grep("Primary", cam[, 1]), grep("^DF$", cam[code, ])] <- 0

# Write the GOC file to the disk
cam <- clean_bi(cam)
write.table(x = cam,
  file = file.path(dir(pattern = "_data", full.names = TRUE), "CAM_BasicInfo.csv"),
  append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
table1 <- rbind(table1, get_basicinfo(cam))

#### Gulf of California
goc_file <- "GOC_sent20171018.csv"

# Reading in the file as emailed
goc <- read.table(file.path(dir(pattern = "_data", full.names = TRUE),
  goc_file), sep = ",", header = FALSE, stringsAsFactors = FALSE)

# Remove a guild from BMS and BC b/c they are IsInfauna
goc[filte, grep("BMS|BC$", goc[code, ], ignore.case = TRUE)] <- 0
goc[demer, grep("Snails", goc[commo, ], ignore.case = TRUE)] <- 1
goc[pelag, grep("pelagics", goc[commo, ], ignore.case = TRUE)] <- 1

# Write the GOC file to the disk
goc <- clean_bi(goc)
write.table(x = goc,
  file = file.path(dir(pattern = "_data", full.names = TRUE), "GOC_BasicInfo.csv"),
  append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
table1 <- rbind(table1, get_basicinfo(goc))

#### GoMex
gom_file <- "GoMex_sent20170317.csv"

# Reading in the file as emailed
gom <- read.csv(file.path(dir(pattern = "_data", full.names = TRUE),
  gom_file), sep = ",", header = FALSE, stringsAsFactors = FALSE)

gom[shark, grep("shark", gom[commo, ])] <- 1
gom[demer, grep("Deep water", gom[commo, ])] <- 1
gom[demer, grep("Microphytobenthos", gom[commo, ])] <- 1
gom[troph, grep("Microphytobenthos", gom[commo, ])] <- 1
gom[pelag, grep("Filter feeding", gom[commo, ])] <- 1
gom[pelag, grep("Pinfish", gom[commo, ])] <- 1
gom[fishy, grep("bird", gom[commo, ])] <- 0
gom[filte, grep("fish|shark|bird|Menhaden", gom[commo, ])] <- 0
gom[filte, which(gom[mamma, ] == 1)] <- 0
gom[fishy, which(gom[mamma, ] == 1)] <- 0
gom[fishy, grep("shrimp", gom[commo, ])] <- 0
gom[filte, grep("Other shrimp", gom[commo, ])] <- 0
gom[filte, grep("Bivalves", gom[commo, ])] <- 0
gom[filte, grep("Sessile", gom[commo, ])] <- 0
gom[dolla, grep("Bivalves", gom[commo, ])] <- 5429
gom[dolla, grep("Oysters", gom[commo, ])] <- 15194
# Fix marine mammals to be surveyed
gom[surve, grepl("MAN$|MYS$|DOL$", gom[code, ])] <- 1

# Write the GOM file to the disk
gom <- clean_bi(gom)
write.table(x = gom,
  file = file.path(dir(pattern = "_data", full.names = TRUE), "GoMex_BasicInfo.csv"),
  append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
table1 <- rbind(table1, get_basicinfo(gom))

#### GUAM
# Mariska Weijerman: mariska.weijerman@noaa.gov
# 1. Vulnerability row was not filled out, used rfishbase.
# 2. Turtles are green turtles, thus, TL = 2.2 is correct
# 3. planktivorous fish have a trophic level of 3.2, MW checked
# 4. Corals: are primary producers and filter feeders, they will eat inverts
# though as well, so some may have trophic levels higher than 1.9.
# 5. No species is assessed
# 6. All species are surveyed
guam_file <- "GuamAtlantis_sent_20171016.csv"

# Reading in the file as emailed
guam <- read.table(file.path(dir(pattern = "_data", full.names = TRUE),
  guam_file), sep = ",", header = FALSE, stringsAsFactors = FALSE)

# Fix USDollarsPerTon
# 8 groups were assigned a dollar value of 1000, but I think they should be 0
# because 1000 was the previous default
guam[dolla, which(guam[dolla, ] == 1000)] <- 0

# Fix IsShark
# Assign Rays as sharks
guam[shark, grep("RAY", guam[code, ])] <- 1

# Fix IsTarget
# Assign groups with no dollar value to 0
guam[targe, guam[dolla, ] == 0] <- 0

# Fix IsSurveyed
# Do not survey any species with a trophic level less than 1
guam[surve, guam[troph, ] < 1] <- 0

# Fix IsPrimaryProducer
# Primary producers should have a TL of < 2 and >= 1.0
guam[primp, guam[troph, ] >= 2][-1] <- 0

# Write the GUAM file to the disk
guam <- clean_bi(guam)

# Fix Vulnerability

all <- read.csv(
  dir(pattern = "GUAM_sent20170306_fish.csv", full.names = TRUE,
    recursive = TRUE), header = TRUE, sep = ",",
  nrows = 170)
colnames(all) <- c("functionalgroup",
  "common", "family", "scientific", "tl", "biom", "prop")
all$Vulnerability <- NA
all$scientific <- gsub("\\s", " ", all$scientific)
all$scientific <- gsub("Zebrasoma veliferum", "Zebrasoma velifer", all$scientific)
all$scientific <- gsub("Dioton hysterix", "Diodon hystrix", all$scientific)
all$functionalgroup <- gsub("Benthis", "Benthic", all$functionalgroup)
# Fix proportions
all$prop <- unlist(tapply(all$prop, all$functionalgroup, prop.table))

dealwith <- c(
  "Acanthurus",
  "Aetobatus narinari",
  "Amblygobius phalaena",
  "Bolbometopon muricatum",
  "Canthigaster solandri",
  "Carcharhinus melanopterus",
  "Chaetodon lunulatus",
  "Cheilinus trilobatus",
  "Chlorurus microrhinos",
  "Cirripectes variolosus",
  "Ctenochaetus striatus",
  "Ctenochaetus binotatus",
  "Dascyllus reticulatus",
  "Diodon hystrix",
  "Epinephelus",
  "Halichoeres margaritaceus",
  "Lethrinus harak",
  "Lutjanus monostigma",
  "Macolor",
  "Mulloidichthys vanicolensis",
  "Myripristis",
  "Naso lituratus",
  "Parupeneus barberinus",
  "Pempheris oualensis",
  "Scomberoides lysan",
  "Sphyraena qenie",
  "Thalassoma lutescens",
  "Triaenodon obesus",
  "Variola louti",
  "Zebrasoma flavescens",
  "Zebrasoma velifer")

trouble <- c("Blenniidae", "Cheilinus sp", "Chlorurus sp",
  "Halichoeres sp", "Kyphosus", "Labridae",
  "Scarus sp", "Scaridae", "Stegastes sp")

info <- all[!grepl(paste(c("other", dealwith, trouble), collapse = "|"),
  all$scientific, ignore.case = TRUE), ]

# Get easily accessed Vulnerabilities
naming <- validate_names(info$scientific, limit = 5)
got <- species(naming)
all$Vulnerability[match(got$sciname, all$scientific)] <- got$Vulnerability

# Work with the dealwith category
info_dealwith <- species(
  all[grep(paste(dealwith, collapse = "|"),
  all$scientific, ignore.case = TRUE), "scientific"]
  )
all$Vulnerability[match(info_dealwith$sciname, all$scientific)] <-
  info_dealwith$Vulnerability

# Substitute Kyphosus sp with Kyphosus vaigiensis
all$Vulnerability[all$scientific == "Kyphosus sp"] <-
  species("Kyphosus vaigiensis")$Vulnerability

# Substitute means for "sp"
all$Vulnerability[all$scientific == "Halichoeres sp"] <-
  mean(all$Vulnerability[grep("Halichoeres [^sp]", all$scientific)])
all$Vulnerability[all$scientific == "Chlorurus sp"] <-
  mean(all$Vulnerability[grep("Chlorurus [^sp]", all$scientific)])
all$Vulnerability[all$scientific == "Cheilinus sp"] <-
  mean(all$Vulnerability[grep("Cheilinus [^sp]", all$scientific)])
all$Vulnerability[all$scientific %in% c("Scarus sp", "Scaridae")] <-
  mean(all$Vulnerability[grep("Scarus [^sp]", all$scientific)])

# Use Bluestreak cleaner wrasse for both Labridae even though it is
# really only one of the two entries
all$Vulnerability[grep("Labridae", all$scientific)] <-
species(common_to_sci("Blue streak"))$Vulnerability

# Use Pacific gregory for Stegastes sp
all$Vulnerability[grep("Stegastes sp", all$scientific)] <-
species(common_to_sci("Pacific gregory"))$Vulnerability

# Use clown blenny for Blenniidae
all$Vulnerability[grep("Blenniidae", all$scientific)] <-
  species(common_to_sci("yaeyama blenny"))$Vulnerability

keep <- tapply(apply(all[,c("Vulnerability", "prop")], 1, prod),
  all$functionalgroup, sum, na.rm = TRUE)

# Match names in the BasicInfo with the weighted vulnerabilities
guam[vulne, -1] <- 0
guam[vulne, na.omit(match(
  tolower(gsub("-associated", "", names(keep))),
  tolower(gsub("Fish |Herbivores ", "", unlist(guam[commo, ])))
  ))] <- keep

write.table(x = guam,
  file = file.path(dir(pattern = "_data", full.names = TRUE), "GuamAtlantis_BasicInfo.csv"),
  append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
table1 <- rbind(table1, get_basicinfo(guam))

#### NEUS
# Gavin emailed a csv with major 'important' species per group so I could
# calculate vulnerabilities from fishbase.
neus_file <- "NEUSDyn_sent20170317.csv"
neus_file_majorspp <- "NEUSv1_MainSpecies_for_Kelli_20170427.csv"
neus_file_prm <- "at_biol_neus_DE.prm"
neus_file_initial <- "inneus_2007_RM.cdf"
neus_names <- "NeusGroups.csv"
neus_surveyed <- "NEUSDyn_sent_GF_20170526.csv"

# Reading in the file as emailed
neus <- read.table(file.path(dir(pattern = "_data", full.names = TRUE),
  neus_file), sep = ",", header = FALSE, stringsAsFactors = FALSE)
neusspp <- read.table(file.path(dir(pattern = "_data", full.names = TRUE),
  neus_file_majorspp), sep = ",", header = TRUE, stringsAsFactors = FALSE)
neusnames <- read.table(file.path(dir(pattern = "_data", full.names = TRUE),
  neus_names), sep = ",", header = TRUE, stringsAsFactors = FALSE)
neussurveyed <- read.table(file.path(dir(pattern = "_data", full.names = TRUE),
  neus_surveyed), sep = ",", header = TRUE, stringsAsFactors = FALSE)

neus[filte, grep("whale", neus[commo, ])] <- 0
neus[filte, grep("mackerel", neus[commo, ])] <- 0
neus[filte, grep("herring", neus[commo, ])] <- 0
neus[filte, grep("pelagics", neus[commo, ])] <- 0
neus[filte, grep("fish", neus[commo, ])] <- 0
neus[filte, grep("Shrimp", neus[commo, ])] <- 0

# Get fish spp names
fishnames <- t(neus[1:2, neus[fishy, ] == 1])
colnames(fishnames) <- c("Code", "Group_name")
fishnames <- merge(fishnames, neusspp, all = TRUE, by = "Code")
fishnames$Main_species <- ifelse(is.na(fishnames$Main_species),
  fishnames$Group_name.x, fishnames$Main_species)
fishnames$sci <- NA
fishnames$sci[fishnames$Main_species == "Butterfish"] <- "Peprilus triacanthus"
fishnames$sci[fishnames$Main_species == "Silver hake"] <- "Merluccius bilinearis"
fishnames$sci[fishnames$Main_species == "redfish"] <- "Sebastes fasciatus"
fishnames$sci[fishnames$Main_species == "pollock"] <- "Pollachius virens"
fishnames$sci[fishnames$Main_species == "black sea bass"] <- "Centropristis striata"
fishnames$sci[fishnames$Main_species == "Goosefish"] <- "Lophius americanus"
fishnames$sci[fishnames$Main_species == "Menhaden"] <- "Brevoortia tyrannus"
fishnames$sci[fishnames$Main_species == "Alewives"] <- "Alosa pseudoharengus"
fishnames$sci[fishnames$Main_species == "Haddock"] <- "Melanogrammus aeglefinus"
fishnames$sci[fishnames$Main_species == "lanternfish?"] <- "Ceratoscopelus maderensis"
fishnames$sci[fishnames$Main_species == "Bluefish"] <- "Pomatomus saltatrix"
fishnames$sci[fishnames$Main_species == "Spiny dogfish"] <- "Squalus acanthias"
fishnames$sci[fishnames$Main_species == "smooth dogfish"] <- "Mustelus canis"
fishnames$sci[fishnames$Main_species == "blue shark"] <- "Prionace glauca"

# Lanternfish include, bot only (i and iv) are seasonally abundant
# The mean of the vulnerabilities was close to that of (i), so
# I am just using (i)
# (i) Horned lanternfish-Ceratoscopelus maderensis.
# (ii) Dumril's headlightfish-Diaphus dumerilii.
# (iii) Crocodile lanternfish-Lampanyctus crocodilus.
# (iv) Doflein's false headlightfish-Lobianchia dofleini.
# (v) Spotted lanternfish-Myctophum punctatum.

# Get vulnerabilities
fishnames$Vulnerability <- NA
onename <- c(4, 7, 8, 12, 14, 16, 17, 18, 19, 20, 22, 25, 27, 28)
fishnames$sci[onename] <- common_to_sci(
  fishnames$Main_species[onename])
fishnames$Vulnerability <- species(fishnames$sci, limit = 1)$Vulnerability
answer <- tapply(fishnames$Vulnerability, fishnames$Code, mean)
neus[vulne, match(names(answer), neus[code, ])] <- answer

# Could get max lengths from fishbase
# species(fishnames$sci, limit = 1)$Length

#### Read in the data
prm <- readLines(file.path(dir(pattern = "_data", full.names = TRUE),
  neus_file_prm), n = 9440, warn = FALSE)
prm <- readLines(file.path(dir(pattern = "_data", full.names = TRUE),
  neus_file_prm), n = 9440, warn = FALSE)
initial <- readLines(file.path(dir(pattern = "_data", full.names = TRUE),
  neus_file_initial), warn = FALSE)

#### Find the appropriate constants
wet2dry <- as.numeric(strsplit(grep("k_wetdry", prm, value = TRUE),
  "[[:space:]]+")[[1]][2])
redfield <- as.numeric(strsplit(grep("X_CN", prm, value = TRUE),
  "[[:space:]]+")[[1]][2])

#### Find the max ages
extrainfo <- get_prmval("_AgeClassSize", prm)
extrainfo$maxage <- extrainfo$value * 10
extrainfo <- extrainfo[extrainfo$code %in% neus[code, ], ]
extrainfo[, "commo"] <- unlist(neus[commo,
  match(extrainfo$code, neus[code, ])])

#### Find weight-length parameters
lia <- get_prmval("li_a_", prm)
lib <- get_prmval("li_b_", prm)

extrainfo <- merge(extrainfo, merge(lia, lib, all = TRUE, by = "code"),
  by = "code", all.x = TRUE, all.y = FALSE)
colnames(extrainfo)[grep("value\\.", colnames(extrainfo))] <- c("a", "b")
extrainfo$gavinname <- neusnames$"Name"[match(extrainfo$code, neusnames$Code)]

#### Find nitrogen data per stage group
nitroinfo <- data.frame(
  "common" = gsub("\\t", "", sapply(strsplit(grep("10_StructN:long_name", initial, value = TRUE), "_StructN|10"), "[[", 1)),
  "structN" = as.numeric(sapply(strsplit(grep("10_StructN:_FillValue", initial, value = TRUE), "FillValue = | ;$"), "[[", 2)),
  "resN" = as.numeric(sapply(strsplit(grep("10_ResN:_FillValue", initial, value = TRUE), "FillValue = | ;$"), "[[", 2)))

# Get names from Gavin
extrainfo <- merge(extrainfo, nitroinfo, by.x = "gavinname", by.y = "common", all.x = TRUE)
rm(nitroinfo)

#### Calculate max length
# length of a vertebrate
# ((wet2dry * XCN * (Two forms of Nitrogen)) / (alpha * 1000))^(1/beta)
# XCN the redfield ratio of carbon to nitrogen
# wet2dry is the conversion coefficient from weight to dry weight,
# alpha is a scaling coefficient in the length-weight relationship
# beta is the exponent in the length-weight relationship.

extrainfo$length <-
  (
    (
      (wet2dry * redfield) *
      (extrainfo$structN + extrainfo$resN)
    ) /
    (
      (extrainfo$a * 1000)
    )
  )^(1 / extrainfo$b)

# Put the estimates into the basic info file
neus[c(linfi, maxag), -1] <- NA
neus[linfi, match(extrainfo$code, neus[code, ])] <- extrainfo$length
neus[maxag, match(extrainfo$code, neus[code, ])] <- extrainfo$maxage

# Fix is surveyed based on GF interpretation
neus[surve, ] <- neussurveyed[grepl("survey", neussurveyed[,1], ignore.case = TRUE), match(neussurveyed[grepl("common", neussurveyed[,1], ignore.case = TRUE), ], neus[commo,])]

# Write the NEUS file to the disk
neus <- clean_bi(neus)
write.table(x = neus,
  file = file.path(dir(pattern = "_data", full.names = TRUE),
    "NEUSDyn_BasicInfo.csv"),
  append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
write.table(x = neus,
  file = file.path(dir(pattern = "_data", full.names = TRUE),
    "NEUSFixedF_BasicInfo.csv"),
  append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
table1 <- rbind(table1, get_basicinfo(neus))

#### NOBA
# Emailed Cecilie and she corrected an earlier version of my revisions,
# informing me that there is in fact a zooplankton fishery in NOBA,
# "There is a targeted fishery in the Norwegian sea on Calanus
# finmarchicus (mesozooplankton), with a quota of roughly 300 000 tons.
# In our yearly ecosystem survey in the Barents Sea
# (and also in the Norwegian Sea) we do sample and estimate zooplankton,
# but that might not be enough to be categorized as ‘isSurveyed’?"
# The file `NOBA_BasicInfo_v2.csv` was emailed to ICK and KFJ on 2017-03-06
# after ICK asked for updated information. I saved the file as:
noba_file <- "NOBA_sent20170317.csv"

# Reading in the file as emailed
noba <- read.csv(file.path(dir(pattern = "_data", full.names = TRUE),
  noba_file), sep = ",", header = FALSE, stringsAsFactors = FALSE)

noba[surve, grep("Minke|Hooded", noba[commo, ])] <- 1
noba[asses, grep("Minke|Hooded", noba[commo, ])] <- 1
noba[asses + 1, grep("Minke|Hooded", noba[commo, ])] <- 0

# Write the NOBA file to the disk
noba <- clean_bi(noba)
write.table(x = noba,
  file = file.path(dir(pattern = "_data", full.names = TRUE), "NOBA_BasicInfo.csv"),
  append = FALSE, sep = ",", row.names = FALSE, col.names = FALSE)
table1 <- rbind(table1, get_basicinfo(noba))

#### Write out summary information
write.csv(table1, row.names = FALSE,
  file = file.path(dir(pattern = "results", full.names = TRUE), "grouppercents.csv"))
}

#### Regions
# List each model individually and then put in table
AustSE <- c("ecosystem" = "SE~Australia",
  "abbreviation" = "AustSE",
  "type" = "ocean",
  "area" = 3000000, "initial year" = 0,
  "region" = "Southwestern Pacific", "country" = "Australia",
  "n polygons" = 71, "n layers" = 5, "n fleets" = 33)
AustSE_DynEffort <- c("ecosystem" = "SE~Australia~DE",
  "abbreviation" = "AustSE_DynEffort",
  "type" = "ocean",
  "area" = 3000000, "initial year" = 0,
  "region" = "Southwestern Pacific", "country" = "Australia",
  "n polygons" = 71, "n layers" = 5, "n fleets" = 33)
CalCu <- c("ecosystem" = "California~Current",
  "abbreviation" = "CalCu",
  "type" = "ocean",
  "area" = 0, "initial year" = 2013,
  "region" = "Eastern Pacific", "country" = "CA, Mexico, USA",
  "n polygons" = 0, "n layers" = 0, "n fleets" = 0)
CAM <- c("ecosystem" = "Chesapeake~Bay",
  "abbreviation" = "CAM",
  "type" = "brackish",
  "area" = 8896, "initial year" = 2009,
  "region" = "Western Atlantic", "country" = "USA",
  "n polygons" = 97, "n layers" = 5, "n fleets" = 1)
GOC <- c("ecosystem" = "N~Gulf~of~California",
  "abbreviation" = "GOC",
  "type" = "gulf",
  "area" = 57800, "initial year" = 2008,
  "region" = "Northeastern Pacific", "country" = "Mexico",
  "n polygons" = 66, "n layers" = 7, "n fleets" = 33)
GoMex <- c("ecosystem" = "Gulf~of~Mexico",
  "abbreviation" = "GoMex",
  "type" = "gulf",
  "area" = 564200, "initial year" = 2012,
  "region" = "Gulf of Mexico", "country" = "Cuba, Mexico, USA",
  "n polygons" = 66, "n layers" = 7, "n fleets" = 0)
GuamAtlantis <- c("ecosystem" = "Guam",
  "abbreviation" = "GuamAtlantis",
  "type" = "reef",
  "area" = 110, "initial year" = 1985,
  "region" = "Western Pacific", "country" = "USA",
  "n polygons" = 55, "n layers" = 2, "n fleets" = 7)
NeusDyn <- c("ecosystem" = "NE~USA",
  "abbreviation" = "NeusDyn",
  "type" = "ocean",
  "area" = 293000, "initial year" = 1964,
  "region" = "Northwestern Atlantic", "country" = "USA",
  "n polygons" = 22, "n layers" = 4, "n fleets" = 18)
NeusFixedF <- c("ecosystem" = "NE~USA~DE",
  "abbreviation" = "NeusFixedF",
  "type" = "ocean",
  "area" = 293000, "initial year" = 1964,
  "region" = "Northwestern Atlantic", "country" = "USA",
  "n polygons" = 22, "n layers" = 4, "n fleets" = 18)
NOBA <- c("ecosystem" = "Nordic~and~Barents~Sea",
  "abbreviation" = "NOBA",
  "type" = "ocean",
  "area" = 4000000, "initial year" = 1981,
  "region" = "Northeastern Atlantic", "country" = "EU",
  "n polygons" = 60, "n layers" = 7, "n fleets" = 1)

regionnames.table <- as.data.frame(rbind(AustSE, AustSE_DynEffort,
  CalCu, CAM, GOC, GoMex, GuamAtlantis,
  NeusDyn, NeusFixedF, NOBA), stringsAsFactors = FALSE)

colnames(regionnames.table)[which(colnames(regionnames.table) == "area")] <- "area ($km^2$)"
colnames(regionnames.table) <- gsub("\\.", " ", colnames(regionnames.table))
rownames(regionnames.table) <- NULL

#### Attributes and Indicators
invcv <- c("attribute" = "invcvtot", "indicator" = "invcvsur",
  "acronym" = "BS",
  "label" = "Biomass stability",
  "definition" = "1 / CV(Surveyed 10-year biomass)")
mlg <- c("attribute" = "mlgfishtot", "indicator" = "mlgfishsur",
  "acronym" = "LG",
  "label" = "Fish size",
  "definition" = "Biomass weighted mean max length of surveyed fish")
mls <- c("attribute" = "mlsfishtot", "indicator" = "mlsfishsur",
  "acronym" = "LS",
  "label" = "Life span",
  "definition" = "Biomass weighted mean max life span of surveyed fish")
pf <- c("attribute" = "pftot", "indicator" = "pfsur",
  "acronym" = "PF",
  "label" = "Predators",
  "definition" = "Proportion of predatory fish in the survey")
ss <- c("attribute" = "sstot", "indicator" = "sssur",
  "acronym" = "SS",
  "label" = "Sustainable stocks",
  "definition" = "Proportion of under and moderately exploited stocks")
tot <- c("attribute" = "Totbio", "indicator" = "Surbio",
  "acronym" = "TB",
  "label" = "Biomass",
  "definition" = "Total biomass of surveyed groups")
mtlcat <- c("attribute" = "mtlbio", "indicator" = "mtlcat",
  "acronym" = "TLc",
  "label" = "Trophic level",
  "definition" = "Biomass weighted mean trophic level of catch")
mivfish <- c("attribute" = "mivfishbio", "indicator" = "mivfishcat",
  "acronym" = "IVI",
  "label" = "Mean vulnerability",
  "definition" = "Mean intrinsic vulnerability index of targeted fish catch")
ndes <- c("attribute" = "ndesbio", "indicator" = "ndessurcat",
  "acronym" = "NDES",
  "label" = "Non-declining species",
  "definition" = "Proportion of non-declining targeted species")
mti <- c("attribute" = "mtibio", "indicator" = "mticat",
  "acronym" = "MTI",
  "label" = "Trophic index",
  "definition" = "Catch-based marine trophic index")
mtlsur <- c("attribute" = "mtlbio", "indicator" = "mtlsur",
  "acronym" = "TLsc",
  "label" = "Trophic level of survey",
  "definition" = "Biomass weighted mean tropic level of surveyed community")
invfsur <- c("attribute" = "invFbio", "indicator" = "invFsur",
  "acronym" = "invF",
  "label" = "Inverse fishing pressure",
  "definition" = "Biomass of surveyed community over catch")

indicators.table <- as.data.frame(rbind(
  invcv, mlg, mls, pf, ss, tot, mtlcat, #shark
  mivfish, ndes, mti, mtlsur, invfsur),
  stringsAsFactors = FALSE)
rownames(indicators.table) <- NULL
attributes <- indicators.table$attribute
indicators <- indicators.table$indicator
# To get all combinations
selfcombo <- cbind(indicators, attributes)
xcombos <- do.call("rbind", strsplit(as.vector(outer(
  indicators,
  c("Bio_pp", "mtlbio", "Nppbio", "Tarbio", "Totbio"),
  paste, sep = "__")), split = "__"))
runcombos <- rbind(selfcombo, xcombos)
runcombos <- runcombos[!duplicated(apply(runcombos, 2, paste)), ]

tsfull <- lapply(regionnames.table$abbreviation, function(x) {
  run_region(datalocation = dir.data, resultslocation = dir.results,
    region = x, mgmtlevel = my.mgmt.threshold,
    indicators = runcombos[, 1], attributes = runcombos[, 2],
    fishlevel = my.mgmt.overfished, nyears = my.tslength, end = my.year.end)
})

tsterm <- do.call("rbind", sapply(unlist(unlist(tsfull, recursive = FALSE),
  recursive = FALSE), "[[", "pars"))
tsterm$scenario <- tolower(tsterm$scenario)
colnames(tsterm) <- gsub("-", "\\.", colnames(tsterm))
tsterm$type <- regionnames.table$type[
  match(tsterm$region, regionnames.table$abbreviation)]
unique(tsterm$region)
tsterm$region <- gsub("atl.{+}$|_|Fix.{+}$", "",
  tsterm$region, ignore.case = TRUE)
tsterm$region <- gsub("Dy.+$", "DE", tsterm$region, ignore.case = TRUE)
shit <- apply(do.call("rbind", sapply(unlist(unlist(tsfull,
  recursive = FALSE), recursive = FALSE),
  function(x) c(x$marss[[1]]$par.lowCI$B[2],
    x$marss[[1]]$par.upCI$B[2]))), 1, function(x) order(c(x, 0))[2] == 3)
shitq <- apply(do.call("rbind", sapply(unlist(unlist(tsfull,
  recursive = FALSE), recursive = FALSE),
  function(x) c(x$marss[[1]]$par.lowCI$Q[2],
    x$marss[[1]]$par.upCI$Q[2]))), 1, function(x) order(c(x, 0))[2] == 3)

#### Plotting
for (ii_letter in c("b", "q")) {
pdf(file.path(dir.results, paste0(my.name, "_indicators_", ii_letter, ".pdf")),
  width = 15, height = 11)
  if (ii_letter == "b") {
    data <- tsterm[!shit, ]
  }
  if (ii_letter == "q") {
    data <- tsterm[!shitq, ]
  }
for (ii_indicator in unique(tsterm$indicator)) {
p <- ggplot(data = data[
  !grepl("fcur_[b-z]", data$scenario) &
  data$indicator == ii_indicator, ],
  aes(x = scenariogroup, fill = region, col = region, pch = factor(type))) +
  guides(col = guide_legend(title = "scenario"), fill = "none") +
  geom_jitter(
    aes_string(y = paste0("fixed.unconstrained.zero_", ii_letter, "21")),
    cex = 3, alpha = 0.5) +
  facet_wrap(c("indicator", "attribute"), scales = "free") +
  my.theme +
  xlab("") + ylab(bquote(b[2][1])) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  geom_text(aes_string(label = "scenarioend", y = paste0("fixed.unconstrained.zero_", ii_letter, "21")))
  # theme(legend.position = c(0.8, 0.99), legend.direction = "horizontal",
  #   legend.key = element_blank())
print(p)
}
dev.off()
}

pdf(file.path(dir.results, paste0(my.name, "_crosstest_scenarios.pdf")),
  width = 15, height = 11)
for (ii_indicator in unique(tsterm$indicator)) {
  temp <- tsterm[
    shit &
    tsterm$indicator == ii_indicator &
    !grepl("fcur_[b-z]", tsterm$scenario) &
    tsterm$fixed.unconstrained.zero == 0, ]
    # grepl("0$", tsterm$scenario), ]
  p <- ggplot(
  data = temp,
  aes(x = as.factor(scenariogroup))) +
  geom_violin(aes(y = fixed.unconstrained.zero_b21)) +
  geom_jitter(aes(y = fixed.unconstrained.zero_b21, col = type), size = 3) +
  facet_grid(indicator ~ attribute, scales = "free") +
  my.theme +
  ylab(bquote(b[2][1])) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  theme(legend.position = "top", legend.direction = "horizontal") +
  # ylim(c(-0.4, 0.4)) +
  geom_text(data = aggregate(fixed.unconstrained.zero ~ scenariogroup + attribute + indicator, data = temp, function(x) sum(!is.na(x))), y = Inf, aes(label = fixed.unconstrained.zero), vjust = 1)
print(p)
}
dev.off()

pdf(file.path(dir.results, paste0(my.name, "_crosstest_regions.pdf")),
  width = 15, height = 11)
for (ii_indicator in unique(tsterm$indicator)) {
  temp <- tsterm[
    shit &
    tsterm$indicator == ii_indicator &
    tsterm$fixed.unconstrained.zero == 0, ]
  p <- ggplot(
  data = temp,
  aes(x = region)) +
  guides(col = guide_legend(title = "scenario")) +
  geom_violin(aes(y = fixed.unconstrained.zero_b21)) +
  geom_jitter(aes(y = fixed.unconstrained.zero_b21,
    col = as.factor(scenariogroup))) +
  facet_grid(indicator ~ attribute, scales = "free") +
  my.theme +
  ylab(bquote(b[2][1])) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, lty = 2, col = "red") +
  theme(legend.position = c(0.5, 0.9), legend.direction = "horizontal",
    legend.key = element_blank())
  # geom_text(data = aggregate(fixed.unconstrained.zero ~ region + attribute + indicator, data = temp, function(x) sum(!is.na(x))), y = Inf, aes(label = fixed.unconstrained.zero), vjust = 1)
print(p)
}
dev.off()

pdf(file.path(dir.results, paste0(my.name, "_all_regions.pdf")),
  width = 15, height = 11)
for (ii_region in unique(tsterm$region)) {
p <- ggplot(tsterm[!shit & tsterm$region == ii_region, ],
# ggplot(tsterm,
  aes(x = scenario, pch = scenariogroup)) +
  # geom_boxplot(aes_string(y="fixed.unconstrained.zero_q21"), alpha = 0.5) +
  geom_point(aes(y = fixed.unconstrained.zero_b21),
    size = 2) +
  facet_grid(indicator ~ attribute) +
  my.theme +
  ylab(paste("B off diagonal in", ii_region)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = 0, lty = 2, col = "red")
print(p)
}
dev.off()


pdf(file.path(dir.results, paste0(my.name, "_meanviolins_b.pdf")))
for (ii_indicator in indicators) {
temp <- data.frame(tsterm, "notzero" = shit)
temp <- temp[, !apply(temp, 2, function(x) all(is.na(x)))]
temp <- temp[temp$indicator == ii_indicator, ]
if (NROW(temp) == 0) next
colnames(temp) <- gsub("fixed.unconstrained.", "", colnames(temp))
stuff <- aggregate(notzero ~ scenariogroup + indicator + attribute,
  data = temp,
  function(x) c(length(x), sum(x)))

g <- ggplot(aggregate(zero_b21 ~ scenariogroup + attribute+indicator,
  data = temp[!shit, ], FUN = mean),
  aes(x = attribute, y = zero_b21, fill = indicator)) +
  geom_violin() +
  geom_violin(data = aggregate(zero_b21 ~ type + attribute+indicator,
  data = temp[!shit, ], FUN = mean),
  aes(x = attribute, y = zero_b21, fill = indicator), alpha = 0.2)
print(g)
}
dev.off()

pdf(file.path(dir.results, paste0(my.name, "_meanviolins_q.pdf")))
for (ii_indicator in indicators) {
temp <- data.frame(tsterm, "notzero" = shit)
temp <- temp[, !apply(temp, 2, function(x) all(is.na(x)))]
temp <- temp[temp$indicator == ii_indicator, ]
if (NROW(temp) == 0) next
colnames(temp) <- gsub("fixed.unconstrained.", "", colnames(temp))
stuff <- aggregate(notzero ~ scenariogroup + indicator + attribute,
  data = temp,
  function(x) c(length(x), sum(x)))

g <- ggplot(aggregate(zero_q21 ~ scenariogroup + attribute+indicator,
  data = temp[!shit, ], FUN = mean),
  aes(x = attribute, y = zero_q21, fill = indicator)) +
  geom_violin() +
  geom_violin(data = aggregate(zero_q21 ~ type + attribute+indicator,
  data = temp[!shit, ], FUN = mean),
  aes(x = attribute, y = zero_q21, fill = indicator), alpha = 0.2)
print(g)
}
dev.off()
