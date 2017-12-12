#' Run a simulation with two states through the OM and EM.
#'
#' @param specs A vector of specifications needed to run the simulation.
#' @param file A character value specifying where to save the \code{.RData}
#' file to. If \code{file = "specs"} the file name is generated according
#' to the values specified in the vector \code{specs}.
#' @param full A logical value specifying if the analysis should be done
#' on all of the data and a truncated data set comprised of the first unique
#' groups (i.e., columns of the simulated data) based on \code{Zom}, which
#' is in \code{specs}. The default value is \code{TRUE}.
#'
#' @author Kelli Faye Johnson
#' @return A data frame as well as results saved to the disk.
#' @export
#'
run_simulation <- function(specs, file = NULL, full = TRUE) {

  ## 01 Set up
  # Parse out the specifications so they can be used for each function.
  for (ii in seq_along(specs)) {
    assign(names(specs)[ii], specs[[ii]])
  }

  Bom <- as.numeric(c(B1, B2, B1))
  Qom <- as.numeric(c(Q1, Q2, Q1))
  Rom <- as.numeric(Rom)
  Zom <- unlist(strsplit(Zom, ";"))
  if (grepl(";", tslength)) {
    tslength <- unlist(strsplit(tslength, ";"))
  }
  tslength <- as.numeric(tslength)
  maxtslength <- max(tslength)
  iterations <- as.numeric(iterations)

  # Fix Q if Q1 != 1
  Q2input <- Q2
  Q2 <- Q2input * sqrt(Q1) * sqrt(Q1)

  ## 02 Simulate data
  obj <- get_parmat(n = length(Zom))
  obj$B[1:4] <- as.numeric(c(B1, B2, 0, B1))
  obj$Q[1:4] <- as.numeric(c(Q1, Q2, Q2, Q1))
  diag(obj$R) <- Rom

  set.seed(iterations)
  simulateddata <- simulate(obj, tSteps = maxtslength, nsim = iterations)
  simulateddata$sim.data <- aperm(simulateddata$sim.data, c(2, 1, 3))

  if (full & length(Zom) > 2) {
    data <- list(
      simulateddata$sim.data,
      simulateddata$sim.data[, !duplicated(Zom), ]
      )
  } else {data <- list(simulateddata$sim.data)}

  all <- list()
  keep <- list()

  for (ii in 1:length(data)) {
    # Create more data sets if length(tslength) > 1
    keep[[ii]] <- lapply(tslength, function(x) {
      apply(data[[ii]][1:x, , ], 3, run_analysis,
      Bem = unlist(strsplit(Bem, ";")),
      Qem = unlist(strsplit(Qem, ";")),
      Rem = unlist(strsplit(Rem, ";")),
      standardize = standardize
      )
    })

    all[[ii]] <- unlist(lapply(keep[[ii]],
      function(x) {lapply(x, function(y) {
        data.frame(y$pars, "iteration" = parent.frame()$i[])
      })}), recursive = FALSE)
  }

  results <- do.call("rbind", unlist(all, recursive = FALSE))
  results$autocorrelation <- B1
  results$correlation <- B2
  results$processdiag <- Q1
  results$process <- Q2
  results$Q2input <- Q2input
  results$observation <- Rom

  if (!is.null(file)) {
    if (file == "specs") {
      file <- paste0(paste(gsub("\\s", "", specs), collapse = "_"), ".Rdata")
    }
    save(keep, all, specs, simulateddata, results, file = file)
  }

  return(results)
}
