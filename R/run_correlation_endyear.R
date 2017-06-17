#' Calculate correlation across facets using terminal year data
#'
#' @param results
#' @param facet
#' @param significance
#' @param method
#' @param return
#' @param verbose
#'
run_correlation_endyear <- function(results,
  facet = c("indicator", "attribute", "region", "scenario", "version"),
  significance = 0.05, method = c("spearman", "pearson"),
  return = c("all", "results"),
  verbose = FALSE) {

  method <- match.arg(method, choices = c("spearman", "pearson"),
    several.ok = FALSE)
  return <- match.arg(return, choices = c("all", "results"),
    several.ok = FALSE)

  # Internal function
  # Calculate correlation
  .getcor <- function(x, y, meth.return = "spearman",
    sig.return = 0.05) {
    temp <- tryCatch({
      est <- suppressWarnings(cor.test(x, y, method = meth.return))
      nn <- as.numeric(sum(!is.na(c(x, y))))
      est <- data.frame(
        "estimate" = est$estimate,
        "pval" = est$p.value,
        "lag" = 0,
        "inside" = ifelse(est$p.value <= sig.return, TRUE, FALSE),
        stringsAsFactors = FALSE)
      est$lowerCI <- -2 / sqrt(nn)
      est$upperCI <- 2 / sqrt(nn)
      return(est)
    }, error = function(x) {NA}, message = function(ex) {NA})
    # if (all(is.na(temp))) return(NA)
    return(temp)
  }

  if ("type" %in% facet) {
    if (verbose) message("Removing type from facet, as ",
      "all types will produce the same results.")
    facet <- facet[-which(facet == "type")]
  }

  resultstrue <- results
  if ("type" %in% colnames(results)) {
    results <- results[results$type == unique(results$type)[1], ]
  }

  results$named <- apply(results[, facet], 1, paste, collapse = "--")

  if (all(table(results$named) == 1)) {
    stop("You faceted across too many variables.")
  }
  correlation <- tapply(1:NROW(results), results$named,
    function(x) {
      .getcor(results[x, "estimate_a"], results[x, "estimate_i"],
        meth.return = method, sig.return = significance)
    })

  tempnames <- names(correlation)
  correlation <- do.call("rbind", correlation)
  tempnames <- as.data.frame(do.call("rbind",
      strsplit(x =  as.character(tempnames), split = "--")))
  colnames(tempnames) <- facet
  correlation <- cbind(correlation, tempnames)
  rownames(correlation) <- NULL
  correlation$type <- "end-year~corr"
  correlation$estimate_inside <- ifelse(correlation$inside,
    correlation$estimate, NA)

  if (return == "all") {
    return(merge(resultstrue, correlation, all = TRUE,
      by = c("type", facet,
        "estimate", "estimate_inside", "pval",
        "lag", "lowerCI", "upperCI", "inside")))
  }
  if (return == "results") {
    return(correlation)
  }

}
