#'
#' @param data
#' @param lims A numeric vector of length 2, supplying the limiations
#' of the y axis.
#' @param ylab
#'
plot_indicators_ts <- function(data,
  lims = c(-1.0, 1.0), ylab, lags = -1:0) {

  # Make the critical intervals very thin and gray
  # Remove all the points and just have lines with different types.

  # specs
  lsize <- 0.75
  pchsize <- 2

  keep <- table(data$time)[which(table(data$time) > 2)]
  data <- data[data$time %in% names(keep), ]
  data$n_years <- 50 - (data$time / 365)
  data$shorttype <- gsub("[[:punct:]][[:space:]][-[0-9]+", "", data$type)
  levels <- unique(data$type)
  data$type <- factor(data$type, levels = c("data", "MARSS", "correlation", levels[rev(order(-1*as.numeric(gsub("[[:alpha:]]+|[[:space:]]|[[:punct:]]", "", levels))))][-(1:3)]))
  data$lag <- factor(data$lag, levels = rev(unique(data$lag)[order(unique(data$lag))]))

  g <- ggplot(data[data$lag %in% c(lags, NA), ],
    aes(x = time, y = estimate, group = group)) +
    facet_grid(type ~ . , scales = "free") +
    geom_line(aes(lty = lag), lwd = lsize) +
    geom_ribbon(data = data[data$type %in% "MARSS", ],
      aes(ymin = lowerCI, ymax = upperCI), alpha = 0.12) +
    geom_ribbon(data = data[grep("ccf|corr", data$type), ],
      aes(ymin = -1, ymax = lowerCI), alpha = 0.12) +
    geom_ribbon(data = data[grep("ccf|corr", data$type), ],
      aes(ymin = upperCI, ymax = 1), alpha = 0.12) +
  # coord_cartesian(ylim = lims) +
  scale_x_continuous(breaks = data$time, labels = data$n_years, expand = c(0, 0)) +
  guides(
    linetype = guide_legend(override.aes = list(size = 0.5), keywidth = 2),
    colour = guide_legend(keywidth = 2)) +
  geom_hline(yintercept = 0, col = "red", linetype = 2, lwd = lsize / 2) +

  xlab("Number of years of data") +
  ylab(paste("Correlation of ", as.character(ylab))) +

  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, size = 1))

  return(g)
}

# plot_indicators_ts(tsinfo,
#   ylab = paste0(indicators.table[ind_it, "label"], " in ", reg_it, ":", sce_it), lags = -2:0)
