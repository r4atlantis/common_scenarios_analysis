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
  tslength <- as.numeric(tslength)
  iterations <- as.numeric(iterations)

  simulateddata <- calc_MARSSsim(
    Bom = Bom, Qom = Qom, Rom = Rom,
    tslength = tslength, iterations = iterations)
  analysis <- apply(simulateddata$sims, 3, run_analysis,
    Qem = Qem, Rem = Rem)
  results <- calc_results(analysis)
  results$autocorrelation <- B1
  results$correlation <- B2
  results$process <- Qom
  results$observation <- Rom
  results$EM <- Qem
  results$n <- tslength

  if (!is.null(file)) {
    save(specs, simulateddata, analysis, results, file = file)
  }

  return(results)
}
