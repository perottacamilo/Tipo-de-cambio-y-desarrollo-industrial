# ==============================================================================
# Función: abreviar_pais
# Objetivo: Abreviar nombres largos de países en español para etiquetas en gráficos y tablas
# ==============================================================================

paises_abreviados <- c(
  "República Democrática del Congo" = "R. D. Congo",
  "República del Congo"             = "Rep. del Congo",
  "República Centroafricana"        = "Rep. Centroafricana",
  "República Dominicana"            = "Rep. Dominicana",
  "Emiratos Árabes Unidos"          = "EAU",
  "Bosnia y Herzegovina"            = "Bosnia y Herz."
)

abreviar_pais <- function(x) {
  x <- as.character(x)
  unname(ifelse(x %in% names(paises_abreviados), paises_abreviados[x], x))
}
