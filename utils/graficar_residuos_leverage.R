# ==============================================================================
# Función: graficar_residuos_leverage
# Objetivo: Exportar un gráfico individual de Residuos vs. Apalancamiento en PNG
# ==============================================================================

graficar_residuos_leverage <- function(modelo, datos_df, titulo, nombre_archivo,
                                      subtitulo = NULL, umbral_cook = 0.15, ind = "Industria") {
  p <- panel_rvsl(
    modelo = modelo,
    datos_df = datos_df,
    titulo = titulo,
    subtitulo = subtitulo,
    ind = ind,
    umbral_cook = umbral_cook,
    es_individual = TRUE
  )
  ruta_archivo <- file.path("output/graficos/regresion", nombre_archivo)
  ggsave(ruta_archivo, plot = p, width = 6.2, height = 4.2, dpi = 300)
  invisible(p)
}
