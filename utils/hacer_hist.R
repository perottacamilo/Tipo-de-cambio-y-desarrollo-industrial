# ==============================================================================
# Función: hacer_hist
# Objetivo: Generar histogramas de distribución con líneas de media y mediana (graficos 2.R)
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

hacer_hist <- function(df, var, titulo, escala_log = TRUE) {
  x_vals  <- df[[var]]
  media   <- mean(x_vals, na.rm = TRUE)
  mediana <- median(x_vals, na.rm = TRUE)
  
  p <- ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(bins = 22, fill = "#4A779D", color = "white", linewidth = 0.3) +
    geom_vline(xintercept = media, color = "#C0392B", linewidth = 1.1) +
    geom_vline(xintercept = mediana, color = "#1B7837", linetype = "dashed", linewidth = 1.1) +
    labs(
      title = titulo,
      x = NULL,
      y = "Cantidad de países"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5, size = 13),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.7),
      plot.margin      = margin(t = 10, r = 22, b = 10, l = 10)
    )
  
  if (escala_log) {
    p <- p + scale_x_log10(
      breaks = c(10, 100, 1000, 10000, 100000),
      labels = label_number(big.mark = ".", decimal.mark = ","),
      expand = expansion(mult = c(0.04, 0.08))
    )
  } else {
    p <- p + scale_x_continuous(
      breaks = seq(25, 125, 25),
      expand = expansion(mult = c(0.04, 0.06))
    )
  }
  
  return(p)
}
