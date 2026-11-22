# ==============================================================================
# Función: theme_owid
# Objetivo: Tema visual unificado estilo Our World in Data con tipografías Google
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(showtext)
  library(ggtext)
})

# Configuración de tipografías
fuentes_ok <- tryCatch({
  sysfonts::font_add_google("Playfair Display", "playfair", regular.wt = 600, bold.wt = 700)
  sysfonts::font_add_google("Lato", "lato", regular.wt = 400, bold.wt = 700)
  TRUE
}, error = function(e) FALSE)

fuente_titulo <- if (fuentes_ok) "playfair" else "serif"
fuente_texto  <- if (fuentes_ok) "lato" else "sans"

showtext::showtext_auto()
showtext::showtext_opts(dpi = 96)

# Paletas de color globales
col <- c(rojo = "#B13507", azul = "#286BBB", gris = "#BDBDBD")
col_asia    <- "#E0195B"
col_latam   <- "#3919E0"
gris_titulo <- "#363636"
gris_texto  <- "#5B5B5B"
gris_suave  <- "#858585"

theme_owid <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = fuente_texto) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_markdown(family = fuente_titulo, size = rel(2.9), color = gris_titulo,
                                       lineheight = 1.05, margin = margin(b = 10)),
      plot.subtitle = element_text(size = rel(1.75), color = gris_texto,
                                   lineheight = 1.15, margin = margin(b = 20)),
      plot.caption  = element_text(size = rel(0.95), color = gris_suave, hjust = 0,
                                   margin = margin(t = 18)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "#DDDDDD", linetype = "dashed", linewidth = 0.4),
      axis.line.x  = element_line(color = "#999999", linewidth = 0.4),
      axis.ticks.x = element_line(color = "#999999", linewidth = 0.4),
      axis.text    = element_text(color = "#666666", size = rel(1.05)),
      axis.title   = element_blank(),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 22, 15, 20)
    )
}
