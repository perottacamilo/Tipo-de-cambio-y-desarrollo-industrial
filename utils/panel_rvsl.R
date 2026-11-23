# ==============================================================================
# Función: panel_rvsl
# Objetivo: Construir un panel ggplot de Residuos Estandarizados vs. Apalancamiento (Leverage)
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(ggrepel)
})

panel_rvsl <- function(modelo, datos_df, titulo, subtitulo = NULL,
                       ind = "Industria", umbral_cook = 0.15, es_individual = FALSE) {
  if (!exists("traducir_pais", mode = "function")) {
    traducir_pais <- function(x) x
  }
  if (!exists("col")) {
    col <- c(rojo = "#B13507", azul = "#286BBB", gris = "#BDBDBD")
  }
  if (!exists("gris_titulo")) gris_titulo <- "#363636"
  if (!exists("gris_texto"))  gris_texto  <- "#5B5B5B"
  if (!exists("fuente_texto")) fuente_texto <- "sans"
  
  res_std    <- rstandard(modelo)
  lev        <- hatvalues(modelo)
  cooks_d    <- cooks.distance(modelo)
  paises_raw <- datos_df[names(res_std), "pais"]
  paises     <- traducir_pais(paises_raw)
  p_k        <- length(coef(modelo))
  
  df <- data.frame(
    pais       = paises,
    leverage   = unname(lev),
    res_std    = unname(res_std),
    cook       = unname(cooks_d),
    es_outlier = unname(cooks_d > umbral_cook),
    stringsAsFactors = FALSE
  )
  
  outliers <- df$pais[df$es_outlier]
  if (is.null(subtitulo) && !es_individual) {
    if (length(outliers) > 0) {
      subtitulo <- paste0("Atípicos (D > 0,15): ", paste(outliers, collapse = ", "))
    } else {
      subtitulo <- "Sin atípicos severos (D > 0,15)"
    }
  }
  
  max_h  <- max(df$leverage, na.rm = TRUE) * 1.15
  grid_h <- seq(0.005, min(max_h, 0.45), length.out = 150)
  val_r  <- sqrt(p_k * umbral_cook * (1 - grid_h) / grid_h)
  df_cook_line <- data.frame(
    h = c(grid_h, grid_h),
    r = c(val_r, -val_r),
    curva = rep(c("pos", "neg"), each = length(grid_h))
  ) |> dplyr::filter(abs(r) <= max(abs(df$res_std), na.rm = TRUE) * 1.25)
  
  tit_y <- if (es_individual) "Residuos estandarizados" else "Residuos estandarizados"
  tit_x <- if (es_individual || ind == "Exportaciones") "Apalancamiento (Leverage)" else NULL
  
  p <- ggplot(df, aes(x = leverage, y = res_std)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#BBBBBB", linewidth = 0.4) +
    geom_point(data = dplyr::filter(df, !es_outlier), shape = 21, size = if (es_individual) 1.8 else 1.3,
               stroke = 0.25, color = "#9E9E9E", fill = alpha(col[["gris"]], 0.65)) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                color = gris_titulo, linewidth = if (es_individual) 0.6 else 0.5) +
    geom_point(data = dplyr::filter(df, es_outlier), shape = 21, size = if (es_individual) 2.6 else 2.0,
               stroke = 0.4, color = "white", fill = col[["rojo"]]) +
    { if(nrow(df_cook_line) > 0)
        geom_line(data = df_cook_line, aes(x = h, y = r, group = curva),
                  linetype = "dotted", color = alpha(col[["rojo"]], 0.7), linewidth = 0.4)
    } +
    geom_text_repel(
      data = dplyr::filter(df, es_outlier),
      aes(label = paste0(pais, " (", round(cook, 3), ")")),
      family = fuente_texto,
      fontface = "bold",
      size = if (es_individual) 8.4 else 7.6,
      color = gris_titulo,
      min.segment.length = 0,
      segment.color = "#777777",
      nudge_y = 0.35,
      seed = 1
    ) +
    labs(title = titulo, subtitle = subtitulo, x = tit_x, y = tit_y) +
    theme_owid(base_size = if (es_individual) 11 else 9.5) %+replace%
    theme(
      plot.title    = element_text(family = fuente_texto, face = "bold",
                                   size = if (es_individual) rel(1.5) else rel(3.5),
                                   color = gris_titulo, hjust = 0,
                                   margin = margin(b = if (is.null(subtitulo)) 10 else 3)),
      plot.subtitle = element_text(size = if (es_individual) rel(1.05) else rel(2),
                                   color = gris_texto, hjust = 0,
                                   lineheight = 0.6, margin = margin(b = 14)),
      axis.title.x  = element_text(size = if (es_individual) rel(1.05) else rel(1.75),
                                   color = gris_texto, margin = margin(t = 6)),
      axis.title.y  = element_text(size = if (es_individual) rel(1.05) else rel(1.75),
                                   color = gris_texto, angle = 90, margin = margin(r = 6)),
      panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      axis.line.x = element_blank(), axis.ticks.x = element_blank(),
      plot.background = element_rect(fill = "#FAFAFA", color = "#E3E3E3"),
      plot.margin = margin(8, 10, 8, 8)
    )
  
  p
}
