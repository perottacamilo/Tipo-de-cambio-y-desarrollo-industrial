# ==============================================================================
# Función: hacer_hist
# Objetivo: Generar histogramas con líneas de media y mediana automatizados
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
})

hacer_hist <- function(data, col, titulo, usar_log = FALSE, mostrar_y = FALSE) {
  d <- data |>
    dplyr::select(valor = {{ col }}) |>
    tidyr::drop_na(valor)
  
  if (usar_log) d <- d |> dplyr::filter(valor > 0)
  
  stats <- tibble::tibble(
    estadistico = c("Media", "Mediana"),
    valor = c(mean(d$valor), median(d$valor))
  )
  
  p <- ggplot(d, aes(x = valor)) +
    geom_histogram(bins = 22, fill = "#2C5F8A", color = "white", alpha = 0.85) +
    geom_vline(
      data = stats,
      aes(xintercept = valor, color = estadistico, linetype = estadistico),
      linewidth = 0.8
    ) +
    scale_color_manual(values = c("Media" = "#C0392B", "Mediana" = "#1E8449"), name = NULL) +
    scale_linetype_manual(values = c("Media" = "solid", "Mediana" = "dashed"), name = NULL) +
    labs(
      title = titulo,
      x = NULL,
      y = if (mostrar_y) "Cantidad de países" else NULL
    ) +
    theme_minimal(base_size = 11.5) +
    theme(
      plot.title = element_text(face = "bold", size = 10.5, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  if (usar_log) {
    p <- p + scale_x_log10(labels = label_number(big.mark = ".", decimal.mark = ","))
  } else {
    p <- p + scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))
  }
  
  return(p)
}
