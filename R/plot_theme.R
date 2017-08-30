plot_theme <- function(get = 1) {
  get <- match.arg(get, choices = 1:2, several.ok = FALSE)
  my.theme <- theme_bw() + theme(
  strip.background = element_blank(),
  panel.border = element_rect(colour = "white", fill = NA, size = 1),
  axis.text.x = element_text(angle = 45, hjust = 1),
  strip.text.y = element_text(angle = 45),
  legend.key = element_rect(colour = NA))

  my.theme2 <- theme_bw() + theme(
  axis.text.x = element_text(angle = 90, hjust = 1),
  strip.text.x = element_text(size = rel(1.5)),
  strip.text.y = element_text(size = rel(1.5)),
  legend.key = element_blank()
  )

  if (get == 1) return(my.theme)
  if (get == 2) return(my.theme2)
}
