#' Read in the data from common scenarios Atlantis runs.
#'
#' Reads in data from multiple scenarios and regions and
#' returns a single data frame, with the basic information
#' and biomass and catches.
#'
#' @param scenarios
#' @param regions
#' @param dir The location where all of the \code{.txt} and
#' \code{BasicInfo} files are stored from Atlantis runs.
#'
#' @author Kelli Faye Johnson
#' @return A \code{data.frame} of catch and biomass for each scenario and
#' region specified in the arguments. The returned object is in \code{"long"}
#' format, where the data is read in in \code{"wide"} format.
#'
#' @examples
#' temp <- read_data(scenarios = c("BC", "OA_01"),
#'   regions = list("CalCu_", "GOC_"), dir = "common_scenarios_data")
#'
read_data <- function(scenarios, regions, dir = getwd()) {

  ignore <- validate_senarioname(scenarios)
  models.list <- list()

  #Helper function
  .reshape <- function(x) {
    code <- colnames(x)[-1]
    colnames(x)[-1] <- paste0("Mass.", code)
    x <- reshape(x, direction = "long",
      varying = list(2:NCOL(x)),
      idvar = "Time", v.names = "Mass",
      timevar = "Code", times = code,
      new.row.names = NULL)
    rownames(x) <- NULL
    return(x)
  }

# LOOP THROUGH EACH MODEL
for (scenario_it in scenarios) {
for (region_it in regions) {
  lookup <- read_lookup(file.path(dir, paste0(region_it, "_BasicInfo.csv")))
  # Read in the files
  bio <- .reshape(read.table(
    file.path(dir, paste(region_it, scenario_it, "BiomIndx.txt", sep = "_")),
    header = TRUE))
  catch <- .reshape(read.table(
    file.path(dir, paste(region_it, scenario_it, "Catch.txt", sep = "_")),
    header = TRUE))
  # Need to combine the data across regions
  data <- merge(bio, catch, by = c("Time", "Code"), all = TRUE,
    suffixes = c(".bio", ".catch"))
  data$scenario <- scenario_it
  data$region <- region_it
  data <- merge(data, lookup, by.x = "Code", by.y = "Atlantis species code",
    all = TRUE)
  models.list[[length(models.list) + 1]] <- data
}  # end loop over regions
} # end loop over scenarios

  return(do.call("rbind", models.list))
} # end function
