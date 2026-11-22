# ==============================================================================
# Función: obtener_outliers_cook
# Objetivo: Identificar países influyentes con Distancia de Cook mayor a un umbral
# ==============================================================================

obtener_outliers_cook <- function(modelo, datos_df, umbral = 0.15) {
  d_cook <- cooks.distance(modelo)
  filas <- names(d_cook[d_cook > umbral & !is.na(d_cook)])
  datos_df[filas, "pais", drop = TRUE]
}
