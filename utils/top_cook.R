# ==============================================================================
# Función: top_cook
# Objetivo: Ranking de los top N países con mayor distancia de Cook
# ==============================================================================

top_cook <- function(modelo, datos_df, n = 5) {
  if (!exists("traducir_pais", mode = "function")) {
    traducir_pais <- function(x) x
  }
  d_cook <- cooks.distance(modelo)
  top <- head(sort(d_cook, decreasing = TRUE), n)
  data.frame(
    pais = traducir_pais(datos_df[names(top), "pais"]),
    cook_distance = round(unname(top), 4),
    stringsAsFactors = FALSE
  )
}
