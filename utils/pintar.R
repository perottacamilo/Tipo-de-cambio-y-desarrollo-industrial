# ==============================================================================
# Función: pintar
# Objetivo: Formatear texto con etiquetas HTML de color para element_markdown
# ==============================================================================

pintar <- function(texto, color) {
  sprintf("<span style='color:%s'><b>%s</b></span>", color, texto)
}
