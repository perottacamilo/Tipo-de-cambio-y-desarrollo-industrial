# ==============================================================================
# Función: armar_modelos_resumen
# Objetivo: Organizar los modelos (Simple, Controles, Sin atípicos) por año y sector
# ==============================================================================

armar_modelos_resumen <- function(dep, modelos_list = NULL, modelos_sin_list = NULL, anios = c(2001, 2008, 2022)) {
  if (is.null(modelos_list)) {
    modelos_list <- get("modelos_por_anio", envir = parent.frame())
  }
  if (is.null(modelos_sin_list)) {
    modelos_sin_list <- get("modelos_por_anio_sin_outliers", envir = parent.frame())
  }
  
  res <- list()
  for (a in anios) {
    simple_name  <- paste0("log-log ", dep, ": Simple")
    control_name <- paste0("log-log ", dep, ": Control")
    
    res[[paste0("Simple (", a, ")")]]       <- modelos_list[[paste0("anio_", a)]][[simple_name]]
    res[[paste0("Controles (", a, ")")]]    <- modelos_list[[paste0("anio_", a)]][[control_name]]
    res[[paste0("Sin atípicos (", a, ")")]] <- modelos_sin_list[[paste0("anio_", a)]][[control_name]]
  }
  return(res)
}
