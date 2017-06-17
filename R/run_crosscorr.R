#'
#'
#' @param data
#' @param attribute
#' @param indicator
#'
run_crosscor <- function(data, attribute, indicator,
  maximumlag = 2) {

  x <- data[, attribute]
  y <- data[, indicator]

  all <- data.frame(
    "estimate" = rep(NA, maximumlag + 1),
    "type" = paste0("cross~corr^", maximumlag:0),
    "lag" = maximumlag:0,
    "attribute" = attribute,
    "indicator" = indicator,
    "inside" = FALSE,
    "pval" = NA,
    "nyears" = length(x),
    stringsAsFactors = FALSE)

  model <- try(forecast::auto.arima(x, seasonal = FALSE),
    silent = TRUE)
  if (class(model)[1] == "try-error") return(all)
  # Use the model results to fit the ccf function
  # temp <- TSA::prewhiten(x, y, ylim = c(-1, 1), plot = FALSE)
  ymodel <- try(forecast::Arima(y, model = model), silent = TRUE)
  if ("try-error" %in% class(ymodel)) return(all)
  yresids <- ymodel$residuals

  temp <- try(ccf(model$residuals, yresids, na.action = na.pass,
      plot = FALSE, lag.max = maximumlag), silent = TRUE)

  # values with a negative lag
  all$"estimate" <- temp$acf[1:(maximumlag + 1)]
  all$"lowerCI" <- -2 / sqrt(temp$n.used)
  all$"upperCI" <- 2 / sqrt(temp$n.used)
  all$inside <- ifelse(all$estimate < all$upperCI & all$estimate > all$lowerCI, FALSE, TRUE)
  temp <- try(cor.test(model$residuals, yresids), silent = TRUE)
  if (!"try-error" %in% class(temp)) all$pval[maximumlag + 1] <- temp$p.val
  rm(temp)

  return(list("xmodel" = model, "ymodel" = ymodel, "tsinfo" = all))

}

