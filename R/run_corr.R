#' Calculate correlation along a time series.
#'
#' todo: place more details here.
#'
#' @param data
#' @param attribute
#' @param indicator
#'
#' @author Kelli Faye Johnson
#'
run_cor <- function(data, attribute, indicator,
  method = c("spearman", "pearson")) {

  method <- match.arg(method, choices = c("spearman", "pearson"),
    several.ok = FALSE)

  # Set up the output
  end <- data.frame(
    "estimate" = rep(NA, 2),
    "lag" = c(0, 0),
    "type" = c("corr", "corr[orig]"),
    "pval" = rep(NA, 2),
    "attribute" = rep(attribute, 2),
    "indicator" = rep(indicator, 2),
    "nyears" = rep(NROW(data), 2),
    "inside" = rep(FALSE, 2),
    stringsAsFactors = FALSE)

  x <- calc_stdnormal(data[, attribute])
  y <- calc_stdnormal(data[, indicator])
  corrstdn <- try(cor.test(x, y, method = method), silent = TRUE)
  corrorig <- try(cor.test(data[, attribute], data[, indicator],
    method = method), silent = TRUE)

  end$estimate <- c(
    ifelse(class(corrstdn) == "try-error", NA, corrstdn$estimate),
    ifelse(class(corrorig) == "try-error", NA, corrorig$estimate))
  end$pval <- c(
    ifelse(class(corrstdn) == "try-error", NA, corrstdn$p.value),
    ifelse(class(corrorig) == "try-error", NA, corrorig$p.value))

  end$inside <- c(
    ifelse(is.na(end$pval[1]) | corrstdn$p.value >= 0.05,
      FALSE, TRUE),
    ifelse(is.na(end$pval[2]) | corrorig$p.value >= 0.05,
      FALSE, TRUE))

  return(end)
}
