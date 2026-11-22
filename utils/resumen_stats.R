# ==============================================================================
# Función: resumen_stats
# Objetivo: Calcular estadísticas descriptivas clave para variables de interés
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

resumen_stats <- function(df, nombre_muestra, vars_clave = NULL) {
  if (is.null(vars_clave)) {
    if (exists("vars_clave", envir = parent.frame())) {
      vars_clave <- get("vars_clave", envir = parent.frame())
    } else {
      vars_clave <- names(dplyr::select(df, where(is.numeric)))
    }
  }
  
  df |>
    dplyr::select(dplyr::all_of(vars_clave)) |>
    tidyr::pivot_longer(everything(), names_to = "Variable", values_to = "valor") |>
    dplyr::group_by(Variable) |>
    dplyr::summarise(
      N       = sum(!is.na(valor)),
      Media   = round(mean(valor, na.rm = TRUE), 1),
      Mediana = round(median(valor, na.rm = TRUE), 1),
      Desvio  = round(sd(valor, na.rm = TRUE), 1),
      .groups = "drop"
    ) |>
    dplyr::mutate(Muestra = nombre_muestra)
}
