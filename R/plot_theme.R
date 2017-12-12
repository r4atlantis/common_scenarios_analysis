plot_theme <- function(get = c("1", "2", "3")) {
  get <- match.arg(get, several.ok = FALSE)

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

  my.theme3 <- theme_bw() + theme(
  strip.background = element_blank(),
  panel.border = element_rect(colour = "black", fill = NA, size = 1),
  strip.text.y = element_text(angle = -270),
  legend.key = element_rect(colour = NA),
  legend.position = "none",
  text = element_text(size = 10))

  if (get == 1) return(my.theme)
  if (get == 2) return(my.theme2)
  if (get == 3) return(my.theme3)
}
