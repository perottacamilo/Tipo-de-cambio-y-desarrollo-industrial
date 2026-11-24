# ==============================================================================
# Función: panel_latam
# Objetivo: Construir un panel de dispersión exploratorio para indicadores regionales
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggrepel)
  library(scales)
})

panel_latam <- function(datos, subtitulo = datos$sub_filtrado[1]) {
  if (!exists("col", mode = "character")) {
    col <- c(rojo = "#B13507", azul = "#286BBB", gris = "#BDBDBD")
  }
  if (!exists("col_latam")) col_latam <- "#3919E0"
  if (!exists("col_asia"))  col_asia  <- "#E0195B"
  if (!exists("gris_titulo")) gris_titulo <- "#363636"
  if (!exists("gris_texto"))  gris_texto  <- "#5B5B5B"
  if (!exists("fuente_texto")) fuente_texto <- "sans"
  
  fmt_num <- label_number(big.mark = ".", decimal.mark = ",", drop0trailing = TRUE)
  
  ind <- datos$indicador[1]
  resto <- dplyr::filter(datos, grupo_region == "Resto del Mundo")
  foco  <- dplyr::filter(datos, grupo_region != "Resto del Mundo")
  destacar <- c("Argentina", "China")
  
  tit_y <- "USD por habitante (escala log)"
  tit_x <- if (ind == "Exportaciones") " Índice de nivel de precios (EE. UU. = 100, escala log)" else NULL
  
  ggplot(datos, aes(pib, valor)) +
    geom_point(data = resto, shape = 21, size = 1.1, stroke = 0.25,
               color = "#9E9E9E", fill = alpha(col[["gris"]], 0.55)) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                color = gris_titulo, linewidth = 0.5) +
    geom_point(data = foco, aes(fill = grupo_region), shape = 21, size = 1.7,
               stroke = 0.4, color = "white") +
    geom_label_repel(data = dplyr::filter(datos, codigo %in% destacar), aes(label = codigo),
                     family = fuente_texto, fontface = "bold", size = 5,
                     fill = alpha("white", 0.85), label.size = 0, label.padding = unit(1.5, "pt"),
                     color = gris_titulo, min.segment.length = 0, segment.color = "#777777",
                     nudge_y = 0.3, seed = 1) +
    scale_fill_manual(values = c("América Latina" = col_latam, "Asia Oriental" = col_asia)) +
    (if (ind == "Industria") {
      scale_y_log10(labels = fmt_num, expand = expansion(mult = c(0.1, 0.15)))
    } else {
      scale_y_log10(labels = fmt_num, expand = expansion(mult = c(0.1, 0.25)))
    }) +
    scale_x_log10(breaks = c(30, 50, 100), labels = fmt_num) +
    coord_cartesian(ylim = if (ind == "Exportaciones") c(1, NA) else NULL) +
    labs(title = datos$titulo[1], subtitle = subtitulo, x = tit_x, y = tit_y) +
    theme_owid(base_size = 9.5) %+replace%
    theme(
      plot.title    = element_text(family = fuente_texto, face = "bold", size = rel(3.5),
                                   color = gris_titulo, hjust = 0, margin = margin(b = 3)),
      plot.subtitle = element_text(size = rel(2), color = gris_texto, hjust = 0,
                                   lineheight = 0.6, margin = margin(b = 14)),
      axis.title.x  = element_text(size = rel(1.75), color = gris_texto, margin = margin(t = 6)),
      axis.title.y  = element_text(size = rel(1.75), color = gris_texto, angle = 90, margin = margin(r = 6)),
      panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      axis.line.x = element_blank(), axis.ticks.x = element_blank(),
      plot.background = element_rect(fill = "#FAFAFA", color = "#E3E3E3"),
      plot.margin = margin(8, 10, 8, 8)
    )
}
