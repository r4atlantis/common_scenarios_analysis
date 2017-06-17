#' @title Create a heat-map figure of indicator output.
#'
#' @param data
#'
#' @import ggplot2


plot_heatmap <- function(data, info,
  x = c("type", "region", "scenario"), y = "indicator") {

  if ("nyears" %in% colnames(data)) {
    data$nyears <- factor(data$nyears)
  }

  sums <- get_summary(results = data, variables = c(x, y))
  sums$indicator <- info[match(sums$indicator, info$indicator), "acronym"]
  if ("type" %in% colnames(sums)) {
    if (!is.factor(sums$type)) {
      sums$type <- factor(sums$type)
    }
  }
  dd <- ggplot(sums, aes_string(x = x[1], y = y))

  # Deal with facets
  if (length(x) == 1) {
    facet_formula <- NULL
  }
  if (length(x) == 2) {
    facet_formula <- as.formula(paste(x[2], "~ ."))
  }
  if (length(x) > 2) {
    facet_formula <- as.formula(paste(x[2], "~",
      paste(x[3:length(x)], collapse = " + ")))
  }

  # x label
  if (x[1] == "nyears") {
    xlab <- "n years in time series"
  } else {
    xlab <- x[1]
  }

  p <- dd +
    # geom_tile(aes(fill = all.median), colour = "white") +
    geom_tile(fill = "white", colour = "black") +
    geom_tile(aes(fill = ins.median, width = inside_prop, height = ins.sign),
      colour = "black", alpha = 1) +
    geom_text(aes(label = round(ins.median, 1))) +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white",
      midpoint = 0, limit = c(-1, 1), space = "Lab",
      name = "") +
    xlab(xlab) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      panel.border = element_rect(colour = "white", fill = NA, size = 1),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text.y = element_text(angle = 45))

  if (!is.null(facet_formula)) {
    p <- p + facet_grid(facet_formula, labeller = label_parsed)
  }
  if (y == "type") {
    p <- p +
      scale_y_discrete("type",
        labels = parse(text = levels(sums$type)))
  }
  if (y == "nyears") {
    p <- p + ylab("number of years")
  }
  if (x[1] == "type") {
    p <- p +
      scale_x_discrete("type",
        labels = parse(text = levels(sums$type)))
  }

  return(p)
}

