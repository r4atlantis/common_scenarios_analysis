#' Validate the scenario name
#'
#' Check against available scenario names
#'
#' @param scenario
#' @author Kelli Faye Johnson
#'
#' @examples
#' # The following example should generate an error
#' # because the second scenario name is not valid.
#' validate_senarioname(c("BC", "CC_add_15"))
#'
validate_senarioname <- function(scenario) {
  all <- c("BC",
    "CC_add_25", "CC_add_35",
    "Fcur_all_05", "Fcur_all_0", "Fcur_all_1", "Fcur_all_2",
    "Fcur_dem_05", "Fcur_dem_0", "Fcur_dem_2",
    "Fcur_inV_05", "Fcur_inV_0", "Fcur_inV_2",
    "Fcur_Lpel_05", "Fcur_Lpel_0", "Fcur_Lpel_2",
    "Fcur_Spel_05", "Fcur_Spel_0", "Fcur_Spel_2",
    "mpa_10", "mpa_25", "mpa_50",
    "OA_005", "OA_01", "SB_15", "SB_3")
  check <- !scenario %in% all
  if (any(check)) {
    stop("\n",paste(scenario[check], collapse = " "),
      "\n not in the list of available scenarios.")
  } else return(TRUE)
}
