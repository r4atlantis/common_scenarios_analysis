#' Run a simulation with an attribute and an indicator through the OM and EM.
#'
#' @param specs A vector of specifications needed to run the simulation.
#'
#' @author Kelli Faye Johnson
#' @return A data frame as well as results saved to the disk.
#' @export
#'
run_simulation <- function(specs, file = NULL) {

  # Parse out the specifications so they can be used for each function.
  for (ii in seq_along(specs)) {
    assign(names(specs)[ii], specs[[ii]])
  }

  Bom <- as.numeric(c(B1, B2, B1))
  Qom <- as.numeric(Qom)
  Rom <- as.numeric(Rom)
  if (grepl("\\(|:", tslength)) {
    tslength <- eval(parse(text = tslength))
  }
  tslength <- as.numeric(tslength)
  maxtslength <- max(tslength)
  iterations <- as.numeric(iterations)

  simulateddata <- calc_MARSSsim(
    Bom = Bom, Qom = Qom, Rom = Rom,
    tslength = maxtslength, iterations = iterations)
  # Create more data sets if length(tslength) > 1
  analysis <- lapply(tslength, function(x) {
    apply(simulateddata$sims[1:x, , ], 3, run_analysis,
    Qem = eval(parse(text = Qem)),
    Rem = eval(parse(text = Rem)))
  })

  results <- lapply(analysis, function(x) lapply(x, function(y) data.frame(y$pars, "iteration" = parent.frame()$i[])))
  results <- unlist(results, recursive = FALSE)

  # analysis <- unlist(analysis, recursive = FALSE)

  results <- do.call("rbind", results)
  results$autocorrelation <- B1
  results$correlation <- B2
  results$process <- Qom
  results$observation <- Rom

  if (!is.null(file)) {
    if (file == "specs") {
      file <- paste(c(specs[c(1:5, 7)], ".RData"), collapse = "_")
    }
    save(specs, simulateddata, analysis, results, file = file)
  }

  return(results)
}
