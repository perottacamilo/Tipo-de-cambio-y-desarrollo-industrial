# ==============================================================================
# Función: test_reset_adaptativo
# Objetivo: Test de Ramsey RESET adaptativo (corregido por heterocedasticidad si BP p < 0.05)
# ==============================================================================

test_reset_adaptativo <- function(modelo) {
  bp <- lmtest::bptest(modelo)
  es_hetero <- bp$p.value < 0.05
  
  if (es_hetero) {
    res <- lmtest::resettest(modelo, vcov = function(x) sandwich::vcovHC(x, type = "HC1"))
    tipo <- "Robusto (HC1)"
  } else {
    res <- lmtest::resettest(modelo)
    tipo <- "Estándar"
  }
  
  data.frame(
    BP_pval     = bp$p.value,
    Tipo_Test   = tipo,
    Estadistico = unname(res$statistic),
    p_valor     = res$p.value,
    Diagnostico = ifelse(res$p.value > 0.05, "Correcta", "Posible mala espec.")
  )
}
