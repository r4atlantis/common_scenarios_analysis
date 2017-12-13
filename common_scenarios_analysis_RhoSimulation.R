###############################################################################
###############################################################################
#### Author: Kelli Faye Johnson
#### Coauthors: Isaac C. Kaplan and Andre E. Punt
#### Short title: rhosimulation
#### Description: A simulation to test the effects of autocorrelation on
####   the estimation of correlation and per-capita changes
#### Date: 2017-12-13
#### Notes: "####" is a section header and "#" is a comment
###############################################################################
###############################################################################

#### Set up inputs
my.name <- "rhosimulation"

#### Setup libraries
library(doParallel, quietly = TRUE, verbose = FALSE)
library(forecast, quietly = TRUE, verbose = FALSE)
library(foreach, quietly = TRUE, verbose = FALSE)
library(ggplot2, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(MARSS, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(pander, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)
library(rmarkdown, warn.conflicts = FALSE, quietly = TRUE, verbose = FALSE)

#### Setup file structure
dir.main <- file.path(paste0(letters, ":"),
  file.path("atlantis", "common_scenarios_analysis"))
dir.main <- dir.main[file.exists(dir.main)]
if (length(dir.main) > 1) stop(length(dir.main),
  " atlantis/common_scenarios_analysis directories were found.")
dir.results <- file.path(dir.main, "results")
dir.create(dir.results, showWarnings = FALSE)
# Source the files to run the analysis
ignore <- sapply(
  dir(file.path(dir.main, "R"), full.names = TRUE, pattern = "\\.r|\\.R"),
  source)

#### Functions
range0 <- function(data) {
  return(c(range(data), 0))
}

#### Run simulation
run_rhosimulation_all()

#### Plot_exampletimeseries
plot_exampletimeseries(forvalues = c(-0.9, 0, 0.9),
  plotcolours = c("#bdbdbd", "#969696", "#636363"),
  directory = dir.results)

