# ==============================================================================
# Función: juntar
# Objetivo: Ensamblar los paneles del gráfico exploratorio de dispersión con patchwork
# ==============================================================================

suppressPackageStartupMessages({
  library(patchwork)
  library(ggtext)
})

juntar <- function(paneles, titulo, subtitulo, caption) {
  if (!exists("fuente_texto")) fuente_texto <- "sans"
  if (!exists("gris_texto"))  gris_texto  <- "#5B5B5B"
  
  wrap_plots(paneles, ncol = 3) +
    plot_annotation(
      title = titulo, 
      subtitle = subtitulo, 
      caption = caption, 
      theme = theme_owid() +
        theme(
          plot.title    = ggtext::element_markdown(family = fuente_texto, face = "bold",
                                                   size = 45, hjust = 0, halign = 0,
                                                   lineheight = 0.4, margin = margin(b = 6)),
          plot.subtitle = ggtext::element_markdown(size = 30, hjust = 0, halign = 0,
                                                   lineheight = 0.35, margin = margin(b = 12)),
          plot.caption  = element_text(size = 18, hjust = 0, lineheight = 0.4, 
                                       color = gris_texto, margin = margin(t = 10)),
          plot.margin   = margin(5, 5, 5, 5)
        )
    )
}
