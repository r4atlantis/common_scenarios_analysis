#' Summarize time-series analysis
#'
#' @param results
#' @param variables
#'
get_summary <- function(results, variables) {

  results$named <- apply(results[, variables], 1, paste, collapse = "__")
  sums <- do.call("rbind", tapply(1:NROW(results), results$named,
    function(rows) {
      x <- results[rows, "estimate_inside"]
      if (length(x) == 1) {
        inest <- c("ins.mean" = x,
          "ins.median" = x,
          "ins.count" = sum(!is.na(x)),
          "ins.max" = x,
          "ins.min" = x,
          "ins.sign" = ifelse(is.na(x), NA, 1))
        x <- results[rows, "estimate"]
        allest <- c("all.mean" = x,
          "all.median" = x,
          "all.count" = sum(!is.na(x)),
          "all.max" = x,
          "all.min" = x,
          "all.sign" = ifelse(is.na(x), NA, 1))
      } else {
        signs <- sign(x)
        signs <- signs[!is.na(signs)]
        inest <- c("ins.mean" = mean(x, na.rm = TRUE),
          "ins.median" = median(x, na.rm = TRUE),
          "ins.count" = sum(!is.na(x)),
          "ins.max" = ifelse(sum(!is.na(x)) == 0, NA, max(x, na.rm = TRUE)),
          "ins.min" = ifelse(sum(!is.na(x)) == 0, NA, min(x, na.rm = TRUE)),
          "ins.sign" = max(c(sum(signs > 0), sum(signs < 0))) / sum(!is.na(x)))
        x <- results[rows, "estimate"]
        allest <- c("all.mean" = mean(x, na.rm = TRUE),
          "all.median" = median(x, na.rm = TRUE),
          "all.count" = sum(!is.na(x)),
          "all.max" = ifelse(sum(!is.na(x)) == 0, NA, max(x, na.rm = TRUE)),
          "all.min" = ifelse(sum(!is.na(x)) == 0, NA, min(x, na.rm = TRUE)),
          "all.sign" = max(c(sum(sign(x) > 0), sum(sign(x) < 0))) / sum(!is.na(x)))
      }
      c(inest, allest)
  }))

  tempname <- rownames(sums)
  tempname <- do.call("rbind", strsplit(tempname, "__"))
  colnames(tempname) <- variables
  sums <- data.frame(tempname, sums, stringsAsFactors = FALSE)
  sums$ins.sign <- ifelse(is.infinite(sums$ins.sign),
    NA, sums$ins.sign)
  sums$ins.min <- ifelse(is.infinite(sums$ins.min), NA, sums$ins.min)
  sums$ins.max <- ifelse(is.infinite(sums$ins.max), NA, sums$ins.max)

  sums$inside_prop <- sums$"ins.count" / sums$"all.count"

  if ("scenario" %in% colnames(results)) {
    if (is.factor(results$scenario)) {
      if ("scenario" %in% colnames(sums)){
            sums$scenario <- factor(sums$scenario,
              levels = levels(results$scenario))
          }
    }
  }

  return(sums)

}

