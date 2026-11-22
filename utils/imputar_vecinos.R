# ==============================================================================
# Función: imputar_vecinos
# Objetivo: Imputar valores faltantes mediante promedio de vecinos temporales (lag y lead)
# ==============================================================================

imputar_vecinos <- function(x, anio) {
  ant <- dplyr::if_else(dplyr::lag(anio) == anio - 1, dplyr::lag(x), NA_real_)
  sig <- dplyr::if_else(dplyr::lead(anio) == anio + 1, dplyr::lead(x), NA_real_)
  dplyr::case_when(
    !is.na(ant) & !is.na(sig) ~ (ant + sig) / 2,
    !is.na(ant)               ~ ant,
    !is.na(sig)               ~ sig,
    TRUE                      ~ NA_real_
  )
}
