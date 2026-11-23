# Script Analisis cuantitativo - Regresiones ===================================
# options(timeout = 600)
# ==============================================================================

library(tidyverse)
library(ggrepel)
library(lmtest)
library(sandwich)
library(car)
library(modelsummary)
library(gt)

# ==============================================================================
# CARGA DE FUNCIONES AUXILIARES PROPIAS (utils/)
# ==============================================================================
invisible(lapply(list.files("utils", full.names = TRUE), source))


wdi_regresion <- read.csv("input/wdi_filtrado.csv")


#/*
wdi_2022 <- wdi_regresion |> 
  filter(anio == 2022)

wdi_2001 <- wdi_regresion |> 
  filter(anio == 2001)
wdi_2008 <- wdi_regresion |> 
  filter(anio == 2008)
anios <- c(2001, 2008, 2022)

modelos_por_anio <- list()
tablas_por_anio  <- list()

for (a in anios) {
  
  # 1. Filtrar datos del año
  df_sub <- wdi_regresion |> filter(anio == a)
  
  # 2. Estimación de los 4 modelos seleccionados
  mods <- list(
    # --- INDUSTRIA (per cápita) ---
    "log-log Ind: Simple"  = lm(log(industria_per_capita) ~ log(indice_nivel_precios), data = df_sub),
    
    "log-log Ind: Control" = lm(log(industria_per_capita) ~ log(indice_nivel_precios) + recursos_naturales + 
                          ahorro_bruto_pbi + log(pbi_per_capita), data = df_sub),
    
    # --- EXPORTACIONES MANUFACTURERAS ---
    "log-log Exp: Simple"  = lm(log(exportacion_manufactura_pc) ~ log(indice_nivel_precios), data = df_sub),
    
    "log-log Exp: Control" = lm(log(exportacion_manufactura_pc) ~ log(indice_nivel_precios) + recursos_naturales + 
                          ahorro_bruto_pbi + log(pbi_per_capita), data = df_sub)
  )
  
  # Guardar modelos
  modelos_por_anio[[paste0("anio_", a)]] <- mods
  
  # 3. Generar la tabla comparativa para el Viewer
  tablas_por_anio[[paste0("anio_", a)]] <- modelsummary(
    mods,
    vcov = "HC1",
    title = paste0("Regresiones para Industria y Exportaciones (", a, ")"),
    stars = TRUE,
    statistic = "std.error",
    fmt = 3,
    coef_rename = c(
      "(Intercept)"              = "Constante",
      "log(indice_nivel_precios)"= "log(Índice de nivel de precios)",
      "recursos_naturales"       = "Rentas rec. naturales (% PBI)",
      "ahorro_bruto_pbi"         = "Ahorro bruto (% PBI)",
      "log(pbi_per_capita)"      = "log(PBI pc)"
    ),
    gof_map = list(
      list("raw" = "nobs", "clean" = "N", "fmt" = 0),
      list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
      list("raw" = "adj.r.squared", "clean" = "R² ajustado", "fmt" = 3)
    ),
    output = "gt"
  )
}
#tablas_por_anio$anio_2001
#tablas_por_anio$anio_2008
#tablas_por_anio$anio_2022
gtsave(tablas_por_anio$anio_2001, filename = "output/tablas/tabla_2001.png")
gtsave(tablas_por_anio$anio_2001, filename = "output/tablas/tabla_2001.html")
gtsave(tablas_por_anio$anio_2008, filename = "output/tablas/tabla_2008.png")
gtsave(tablas_por_anio$anio_2008, filename = "output/tablas/tabla_2008.html")
gtsave(tablas_por_anio$anio_2022, filename = "output/tablas/tabla_2022.png")
gtsave(tablas_por_anio$anio_2022, filename = "output/tablas/tabla_2022.html")
# ==============================================================================
# DETECCIÓN AUTOMÁTICA DE OBSERVACIONES INFLUYENTES (DISTANCIA DE COOK)
# ==============================================================================
# Función obtener_outliers_cook cargada desde utils/obtener_outliers_cook.R

# Identificación automática de outliers (D_i > 0.15)
# Industria
outliers_2001_ind <- obtener_outliers_cook(modelos_por_anio$anio_2001[["log-log Ind: Control"]], wdi_2001, umbral = 0.15)
outliers_2008_ind <- obtener_outliers_cook(modelos_por_anio$anio_2008[["log-log Ind: Control"]], wdi_2008, umbral = 0.15)
outliers_2022_ind <- obtener_outliers_cook(modelos_por_anio$anio_2022[["log-log Ind: Control"]], wdi_2022, umbral = 0.15)

# Exportaciones
outliers_2001_exp <- obtener_outliers_cook(modelos_por_anio$anio_2001[["log-log Exp: Control"]], wdi_2001, umbral = 0.15)
outliers_2008_exp <- obtener_outliers_cook(modelos_por_anio$anio_2008[["log-log Exp: Control"]], wdi_2008, umbral = 0.15)
outliers_2022_exp <- obtener_outliers_cook(modelos_por_anio$anio_2022[["log-log Exp: Control"]], wdi_2022, umbral = 0.15)
modelos_por_anio_sin_outliers <- list()

# 1. Estimación para Industria sin outliers
for (a in anios) {
  df_sub <- wdi_regresion |> filter(anio == a)
  
  if (a == 2001) {
    df_sub <- df_sub |> filter(!pais %in% outliers_2001_ind)
  }
  if (a == 2008) {
    df_sub <- df_sub |> filter(!pais %in% outliers_2008_ind)
  }
  if (a == 2022) {
    df_sub <- df_sub |> filter(!pais %in% outliers_2022_ind)
  }
  
  mods <- list(
    # --- INDUSTRIA (per cápita) ---
    "log-log Ind: Control" = lm(
      log(industria_per_capita) ~ log(indice_nivel_precios) + recursos_naturales + 
        ahorro_bruto_pbi + log(pbi_per_capita), 
      data = df_sub
    )
  )
  
  modelos_por_anio_sin_outliers[[paste0("anio_", a)]] <- mods
}

# 2. Estimación para Exportaciones sin outliers
for (a in anios) {
  df_sub_exp <- wdi_regresion |> filter(anio == a)
  
  if (a == 2001) {
    df_sub_exp <- df_sub_exp |> filter(!pais %in% outliers_2001_exp)
  }
  if (a == 2008) {
    df_sub_exp <- df_sub_exp |> filter(!pais %in% outliers_2008_exp)
  }
  if (a == 2022) {
    df_sub_exp <- df_sub_exp |> filter(!pais %in% outliers_2022_exp)
  }
  
  mods_exp <- list(
    # --- EXPORTACIONES MANUFACTURERAS ---
    "log-log Exp: Control" = lm(
      log(exportacion_manufactura_pc) ~ log(indice_nivel_precios) + recursos_naturales + 
        ahorro_bruto_pbi + log(pbi_per_capita), 
      data = df_sub_exp
    )
  )
  
  modelos_por_anio_sin_outliers[[paste0("anio_", a)]][["log-log Exp: Control"]] <- mods_exp[["log-log Exp: Control"]]
}

# 3. Generar la tabla comparativa unificada con todos los modelos
modelos_sin_outliers_todos <- list(
  "2001"   = modelos_por_anio_sin_outliers$anio_2001[["log-log Ind: Control"]],
  "2008"   = modelos_por_anio_sin_outliers$anio_2008[["log-log Ind: Control"]],
  "2022"   = modelos_por_anio_sin_outliers$anio_2022[["log-log Ind: Control"]],
  "2001 "  = modelos_por_anio_sin_outliers$anio_2001[["log-log Exp: Control"]],
  "2008 "  = modelos_por_anio_sin_outliers$anio_2008[["log-log Exp: Control"]],
  "2022 "  = modelos_por_anio_sin_outliers$anio_2022[["log-log Exp: Control"]]
)

tabla_sin_outliers <- modelsummary(
  modelos_sin_outliers_todos,
  vcov = "HC1",
  title = "Regresiones para Industria y Exportaciones sin outliers",
  stars = TRUE,
  statistic = "std.error",
  fmt = 3,
  coef_rename = c(
    "(Intercept)"              = "Constante",
    "indice_nivel_precios"     = "Índice de nivel de precios",
    "recursos_naturales"       = "Rentas rec. naturales (% PBI)",
    "ahorro_bruto_pbi"         = "Ahorro bruto (% PBI)",
    "log(pbi_per_capita)"      = "log(PBI pc)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "N", "fmt" = 0),
    list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
    list("raw" = "adj.r.squared", "clean" = "R² ajustado", "fmt" = 3)
  ),
  output = "gt"
) |> 
  tab_spanner(label = "Industria (per cápita)", columns = 2:4) |> 
  tab_spanner(label = "Exportaciones Manufactureras (per cápita)", columns = 5:7)

# Visualizar en RStudio Viewer y guardar en PNG y HTML
#tabla_sin_outliers
gtsave(tabla_sin_outliers, filename = "output/tablas/tabla_sin_outliers.png")
gtsave(tabla_sin_outliers, filename = "output/tablas/tabla_sin_outliers.html")
#####tests de Breusch-Pagan#####
# 1. Creamos un data.frame vacío para ir guardando los resultados
resultados_bp <- data.frame()

# 2. Bucle para calcular el test en cada año y modelo
for (a in c(2001, 2008, 2022)) {
  
  # Extraemos los modelos
  mod_ind <- modelos_por_anio[[paste0("anio_", a)]][["log-log Ind: Control"]]
  mod_exp <- modelos_por_anio[[paste0("anio_", a)]][["log-log Exp: Control"]]
  
  # Aplicamos el test de Breusch-Pagan
  bp_ind <- lmtest::bptest(mod_ind)
  bp_exp <- lmtest::bptest(mod_exp)
  
  # Guardamos los resultados temporalmente
  temp_df <- data.frame(
    Año = c(a, a),
    Modelo = c("Industria (per cápita)", "Exportaciones Manufactureras"),
    BP_Stat = c(bp_ind$statistic, bp_exp$statistic),
    p_valor = c(bp_ind$p.value, bp_exp$p.value)
  )
  
  # Unimos con el data.frame principal
  resultados_bp <- rbind(resultados_bp, temp_df)
}

# 3. Formateamos y generamos la tabla visual
tabla_bp_final <- resultados_bp |> 
  mutate(
    # Si el p-valor es menor a 0.05, rechazamos H0 (homocedasticidad)
    `Diagnóstico` = ifelse(p_valor < 0.05, "Heteroscedasticidad", "Homocedasticidad")
  ) |> 
  gt() |> 
  tab_header(
    title = "Test de Breusch-Pagan para Heteroscedasticidad",
    subtitle = "H0: Varianza constante (Homocedasticidad)"
  ) |> 
  cols_label(
    BP_Stat = "Estadístico BP",
    p_valor = "p-valor"
  ) |> 
  fmt_number(
    columns = BP_Stat,
    decimals = 2
  ) |> 
  fmt_number(
    columns = p_valor,
    decimals = 4
  )

# Imprimimos la tabla
#tabla_bp_final
gtsave(tabla_bp_final, filename = "output/tablas/tabla_bp_final.png")
gtsave(tabla_bp_final, filename = "output/tablas/tabla_bp_final.html")

# ==============================================================================
# DIAGNÓSTICOS ECONOMÉTRICOS PARA CADA AÑO
# ==============================================================================

# ==============================================================================
# 1. MULTICOLINEALIDAD (VIF) PARA TODOS LOS AÑOS Y MODELOS
# ==============================================================================

# Creamos una lista para almacenar los resultados
vifs_por_anio <- list()

for (a in c(2001, 2008, 2022)) {
  
  # Extraemos los modelos de control para cada año
  mod_ind <- modelos_por_anio[[paste0("anio_", a)]][["log-log Ind: Control"]]
  mod_exp <- modelos_por_anio[[paste0("anio_", a)]][["log-log Exp: Control"]]
  
  # Calculamos el VIF
  vif_ind <- car::vif(mod_ind)
  vif_exp <- car::vif(mod_exp)
  
  # Guardamos en la lista
  vifs_por_anio[[paste0("anio_", a)]] <- list(
    VIF_Industria = vif_ind,
    VIF_Exportaciones = vif_exp
  )
}



# Extraemos los nombres de las variables del primer VIF guardado
nombres_vars <- names(vifs_por_anio$anio_2001$VIF_Industria)

# Armamos el dataframe juntando los años para ambos modelos (Industria y Exportaciones)
tabla_vif <- data.frame(
  Variable = nombres_vars,
  Ind_2001 = vifs_por_anio$anio_2001$VIF_Industria,
  Ind_2008 = vifs_por_anio$anio_2008$VIF_Industria,
  Ind_2022 = vifs_por_anio$anio_2022$VIF_Industria,
  Exp_2001 = vifs_por_anio$anio_2001$VIF_Exportaciones,
  Exp_2008 = vifs_por_anio$anio_2008$VIF_Exportaciones,
  Exp_2022 = vifs_por_anio$anio_2022$VIF_Exportaciones
) |> 
  # Renombramos las variables para que queden presentables
  mutate(
    Variable = case_match(
      Variable,
      "log(indice_nivel_precios)" ~ "log(Índice de nivel de precios)",
      "recursos_naturales"        ~ "Rentas rec. naturales (% PBI)",
      "ahorro_bruto_pbi"          ~ "Ahorro bruto (% PBI)",
      "log(pbi_per_capita)"       ~ "log(PBI per cápita)",
      .default = Variable
    )
  )

# Generamos la tabla con formato agrupada por modelo con spanners
tabla_vif_final <- tabla_vif |> 
  gt() |> 
  tab_header(
    title = "Factor de Inflación de la Varianza (VIF)",
    subtitle = "Diagnóstico de multicolinealidad para los modelos con controles"
  ) |> 
  tab_spanner(
    label = "Modelo Industria (per cápita)",
    columns = c(Ind_2001, Ind_2008, Ind_2022)
  ) |> 
  tab_spanner(
    label = "Modelo Exportaciones (per cápita)",
    columns = c(Exp_2001, Exp_2008, Exp_2022)
  ) |> 
  cols_label(
    Variable = "Variable Explicativa",
    Ind_2001 = "2001",
    Ind_2008 = "2008",
    Ind_2022 = "2022",
    Exp_2001 = "2001",
    Exp_2008 = "2008",
    Exp_2022 = "2022"
  ) |> 
  fmt_number(
    columns = c(starts_with("Ind_"), starts_with("Exp_")),
    decimals = 2
  ) |> 
  cols_align(
    align = "center",
    columns = c(starts_with("Ind_"), starts_with("Exp_"))
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_column_labels(),
      cells_column_spanners()
    )
  ) |> 
  tab_source_note(
    source_note = "Nota: Valores de VIF < 5 indican ausencia de multicolinealidad severa. Las leves diferencias entre ambos modelos se deben a la muestra efectiva de países disponibles con datos completos para cada variable dependiente."
  )

# Imprimimos la tabla
#tabla_vif_final
gtsave(tabla_vif_final, filename = "output/tablas/tabla_vif_final.png")
gtsave(tabla_vif_final, filename = "output/tablas/tabla_vif_final.html")

variables2 <- c(
  "crecimiento_industria",
  "industria_per_capita",
  "indice_nivel_precios",
  "empleo_industrial",
  "exportacion_manufactura_pc",
  "gasto_gobierno_pbi",
  "valor_agregado_trabajador",
  "ahorro_bruto_pbi",
  "credito_privado_pbi",
  "recursos_naturales",
  "pbi_per_capita",
  "pbi_millones")
# 1. Extraemos el modelo de la lista
mod_exp_2022 <- modelos_por_anio$anio_2022[["log-log Exp: Control"]]
mod_exp_2001 <- modelos_por_anio$anio_2001[["log-log Exp: Control"]]
mod_exp_2008 <- modelos_por_anio$anio_2008[["log-log Exp: Control"]]

mod_ind_2022 <- modelos_por_anio$anio_2022[["log-log Ind: Control"]]
mod_ind_2001 <- modelos_por_anio$anio_2001[["log-log Ind: Control"]]
mod_ind_2008 <- modelos_por_anio$anio_2008[["log-log Ind: Control"]]

# ==============================================================================
# TABLA COMPARATIVA: TEST RESET DE RAMSEY (CON Y SIN OUTLIERS)
# ==============================================================================
# Función test_reset_adaptativo cargada desde utils/test_reset_adaptativo.R

# Construcción de la tabla para todos los años y modelos
anios_test <- c(2001, 2008, 2022)
filas_reset <- list()

for (a in anios_test) {
  clave_anio <- paste0("anio_", a)
  
  # Industria: Con y Sin outliers
  mod_ind_con <- modelos_por_anio[[clave_anio]][["log-log Ind: Control"]]
  mod_ind_sin <- modelos_por_anio_sin_outliers[[clave_anio]][["log-log Ind: Control"]]
  
  t_ind_con <- test_reset_adaptativo(mod_ind_con)
  t_ind_sin <- test_reset_adaptativo(mod_ind_sin)
  
  # Exportaciones: Con y Sin outliers
  mod_exp_con <- modelos_por_anio[[clave_anio]][["log-log Exp: Control"]]
  mod_exp_sin <- modelos_por_anio_sin_outliers[[clave_anio]][["log-log Exp: Control"]]
  
  t_exp_con <- test_reset_adaptativo(mod_exp_con)
  t_exp_sin <- test_reset_adaptativo(mod_exp_sin)
  
  filas_reset[[paste0(a, "_ind")]] <- data.frame(
    Año = a,
    Modelo = "Industria (per cápita)",
    Pval_con = t_ind_con$p_valor,
    Pval_sin = t_ind_sin$p_valor
  )
  
  filas_reset[[paste0(a, "_exp")]] <- data.frame(
    Año = a,
    Modelo = "Exportaciones Manufactureras (per cápita)",
    Pval_con = t_exp_con$p_valor,
    Pval_sin = t_exp_sin$p_valor
  )
}

df_tabla_reset <- do.call(rbind, filas_reset)

# Generación de la tabla formateada con gt
tabla_reset_gt <- df_tabla_reset |>
  gt(groupname_col = "Año") |>
  tab_header(
    title = "Test de Especificación de Ramsey (RESET)",
    subtitle = "Comparación de p-valores con y sin observaciones influyentes (corregido por heterocedasticidad con HC1 si BP p < 0.05)"
  ) |>
  tab_spanner(
    label = "Muestra Completa",
    columns = Pval_con
  ) |>
  tab_spanner(
    label = "Sin valores atípicos",
    columns = Pval_sin
  ) |>
  cols_label(
    Modelo   = "Modelo",
    Pval_con = "p-valor",
    Pval_sin = "p-valor"
  ) |>
  fmt_number(
    columns = c(Pval_con, Pval_sin),
    decimals = 4
  ) |>
  cols_align(
    align = "center",
    columns = c(Pval_con, Pval_sin)
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

# Visualizar en la pestaña Viewer
#tabla_reset_gt

# Guardar en archivo PNG y HTML
gtsave(tabla_reset_gt, filename = "output/tablas/tabla_reset_comparativa.png")
gtsave(tabla_reset_gt, filename = "output/tablas/tabla_reset_comparativa.html")
# 2. Dividimos la ventana en 4 (2 filas x 2 columnas)
#par(mfrow = c(2, 2))

# 3. Generamos los gráficos con el color transparente
#plot(mod_exp_2022, pch = 16, col = rgb(0, 0, 1, 0.1))
#plot(mod_exp_2008, pch = 16, col = rgb(0, 0, 1, 0.1))
#plot(mod_exp_2001, pch = 16, col = rgb(0, 0, 1, 0.1))


#plot(mod_ind_2022, pch = 16, col = rgb(0, 0, 1, 0.1))
#plot(mod_ind_2008, pch = 16, col = rgb(0, 0, 1, 0.1))
#plot(mod_ind_2001, pch = 16, col = rgb(0, 0, 1, 0.1))

# 4. Volvemos la ventana a la normalidad (1 gráfico a la vez)
#par(mfrow = c(1, 1))

# ==============================================================================
# DICCIONARIO DE TRADUCCIÓN DE PAÍSES AL ESPAÑOL Y RANKING COOK
# ==============================================================================
# Funciones traducir_pais, top_cook y diccionario paises_espanol cargados desde utils/

#print(top_cook(modelos_por_anio$anio_2022[["log-log Exp: Control"]], wdi_2022))
#print(top_cook(modelos_por_anio$anio_2022[["log-log Ind: Control"]], wdi_2022))

#print(top_cook(modelos_por_anio$anio_2001[["log-log Exp: Control"]], wdi_2001))
#print(top_cook(modelos_por_anio$anio_2001[["log-log Ind: Control"]], wdi_2001))

#print(top_cook(modelos_por_anio$anio_2008[["log-log Exp: Control"]], wdi_2008))
#print(top_cook(modelos_por_anio$anio_2008[["log-log Ind: Control"]], wdi_2008))



# ==============================================================================
# GRÁFICOS: RESIDUALS VS LEVERAGE (FORMATO EXPLORATORIO - OWID)
# ==============================================================================
# Funciones panel_rvsl, graficar_residuos_leverage, juntar_rvsl, theme_owid y pintar
# se cargan automáticamente desde utils/

# Crear carpeta de salida si no existe
dir.create("output/graficos/regresion", recursive = TRUE, showWarnings = FALSE)

# --- CREAR Y GUARDAR EL GRÁFICO COMPLETO DE 6 PANELES (ESTILO EXPLORATORIO) ---
p_ind_2001 <- panel_rvsl(mod_ind_2001, wdi_2001, "Industria (2001)", ind = "Industria")
p_ind_2008 <- panel_rvsl(mod_ind_2008, wdi_2008, "Industria (2008)", ind = "Industria")
p_ind_2022 <- panel_rvsl(mod_ind_2022, wdi_2022, "Industria (2022)", ind = "Industria")

p_exp_2001 <- panel_rvsl(mod_exp_2001, wdi_2001, "Exportaciones (2001)", ind = "Exportaciones")
p_exp_2008 <- panel_rvsl(mod_exp_2008, wdi_2008, "Exportaciones (2008)", ind = "Exportaciones")
p_exp_2022 <- panel_rvsl(mod_exp_2022, wdi_2022, "Exportaciones (2022)", ind = "Exportaciones")

paneles_rvsl <- list(
  p_ind_2001, p_ind_2008, p_ind_2022,
  p_exp_2001, p_exp_2008, p_exp_2022
)

# Función juntar_rvsl cargada desde utils/juntar_rvsl.R

titulo_rvsl <- paste0(
  "Diagnóstico de Residuos vs. Apalancamiento (*Leverage*):<br>",
  "Observaciones influyentes identificadas mediante la ",
  pintar("Distancia de Cook", col[["rojo"]])
)

subtitulo_rvsl <- paste0(
  "Cada punto representa un país en las estimaciones con controles. ",
  "En ", pintar("rojo", col[["rojo"]]), " se identifican los países atípicos.<br>",
  "La curva gris muestra la tendencia estimada y la línea de puntos roja señala el contorno crítico de Cook (D = 0,15)."
)

caption_rvsl <- paste0(
  "Fuente: elaboración propia en base a Banco Mundial (WDI) y Schteingart, D. y Sidicaro, N. (2025). Industria. Argendata. Fundar.\n",
  "Nota: Modelos log-log con controles."
)

grafico_rvsl_exploratorio <- juntar_rvsl(
  paneles = paneles_rvsl,
  titulo = titulo_rvsl,
  subtitulo = subtitulo_rvsl,
  caption = caption_rvsl
)

ggsave("output/graficos/regresion/rvsl_exploratorio_completo.png", plot = grafico_rvsl_exploratorio,
       width = 22, height = 14, units = "cm", dpi = 300)

# ==============================================================================
# TABLA RESUMEN COMPARATIVA: SIMPLE vs CON CONTROLES vs SIN OUTLIERS
# ==============================================================================
# Función armar_modelos_resumen cargada desde utils/armar_modelos_resumen.R

mods_resumen_ind <- armar_modelos_resumen("Ind")
mods_resumen_exp <- armar_modelos_resumen("Exp")

# Generamos la tabla comparativa estructurada en paneles (Industria y Exportaciones)
tabla_resumen_modelos <- modelsummary(
  list(
    "Panel A: Industria (per cápita)" = mods_resumen_ind,
    "Panel B: Exportaciones Manufactureras (per cápita)" = mods_resumen_exp
  ),
  shape = "rbind",
  vcov = "HC1",
  title = "Regresiones Log-Log: Efecto del Nivel de Precios sobre Industria y Exportaciones",
  coef_map = c(
    "log(indice_nivel_precios)" = "log(Índice de precios)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "N", "fmt" = 0),
    list("raw" = "adj.r.squared", "clean" = "R² ajustado", "fmt" = 3)
  ),
  stars = TRUE,
  statistic = "std.error",
  fmt = 3,
  output = "gt"
) |> 
  tab_spanner(label = "2001", columns = 2:4) |> 
  tab_spanner(label = "2008", columns = 5:7) |> 
  tab_spanner(label = "2022", columns = 8:10) |> 
  cols_label(
    `Simple (2001)`       = "Simple",
    `Controles (2001)`    = "Controles",
    `Sin atípicos (2001)` = "Sin atípicos",
    `Simple (2008)`       = "Simple",
    `Controles (2008)`    = "Controles",
    `Sin atípicos (2008)` = "Sin atípicos",
    `Simple (2022)`       = "Simple",
    `Controles (2022)`    = "Controles",
    `Sin atípicos (2022)` = "Sin atípicos"
  ) |> 
  tab_source_note(
    source_note = md(paste0(
      "**Variables de control incluidas en las especificaciones con controles:** ",
      "Rentas de recursos naturales (% PBI), Ahorro bruto (% PBI) y log(PBI per cápita).<br>",
      "**Sin atípicos:** Excluye observaciones influyentes detectadas mediante distancia de Cook (D > 0.15)."
    ))
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  )

# Imprimir en Viewer y guardar en PNG y HTML
tabla_resumen_modelos
gtsave(tabla_resumen_modelos, filename = "output/tablas/tabla_resumen_modelos.png")
gtsave(tabla_resumen_modelos, filename = "output/tablas/tabla_resumen_modelos.html")

# ==============================================================================
# TABLA RESUMEN DE DIAGNÓSTICOS ECONOMÉTRICOS Y TESTS DE ESPECIFICACIÓN
# ==============================================================================

filas_diagnosticos <- list()

for (a in c(2001, 2008, 2022)) {
  clave_anio <- paste0("anio_", a)
  
  for (dep in c("Ind", "Exp")) {
    modelo_nombre <- ifelse(dep == "Ind", "Industria (per cápita)", "Exportaciones Manufactureras (per cápita)")
    mod_con_name  <- paste0("log-log ", dep, ": Control")
    
    mod_con <- modelos_por_anio[[clave_anio]][[mod_con_name]]
    mod_sin <- modelos_por_anio_sin_outliers[[clave_anio]][[mod_con_name]]
    
    # 1. Breusch-Pagan (Heterocedasticidad)
    bp_con <- lmtest::bptest(mod_con)
    
    # 2. Ramsey RESET adaptativo (Forma funcional / especificación)
    reset_con <- test_reset_adaptativo(mod_con)
    reset_sin <- test_reset_adaptativo(mod_sin)
    
    # 3. Multicolinealidad (VIF máximo)
    vif_vals <- car::vif(mod_con)
    vif_max  <- max(vif_vals)
    
    # 4. Observaciones influyentes (Distancia de Cook > 0.15)
    outliers_var <- get(paste0("outliers_", a, "_", tolower(dep)))
    outliers_var_es <- traducir_pais(outliers_var)
    n_outliers   <- length(outliers_var_es)
    paises_out   <- paste(outliers_var_es, collapse = ", ")
    
    filas_diagnosticos[[paste0(a, "_", dep)]] <- data.frame(
      Año          = a,
      Modelo       = modelo_nombre,
      BP_pval      = bp_con$p.value,
      RESET_con    = reset_con$p_valor,
      RESET_sin    = reset_sin$p_valor,
      VIF_max      = vif_max,
      N_outliers   = n_outliers,
      Paises_out   = ifelse(n_outliers > 0, paises_out, "Ninguno")
    )
  }
}

df_diagnosticos <- do.call(rbind, filas_diagnosticos)

# Generación del boceto de tabla con gt
tabla_diagnosticos_gt <- df_diagnosticos |>
  gt(groupname_col = "Año") |>
  tab_header(
    title = "Resumen de Diagnósticos Econométricos y Tests de Especificación",
    subtitle = "Modelos con controles para Industria y Exportaciones (2001, 2008, 2022)"
  ) |>
  tab_spanner(
    label = md("Heterocedasticidad<br>(Breusch-Pagan)"),
    columns = BP_pval
  ) |>
  tab_spanner(
    label = md("Especificación RESET<br>(p-valor)"),
    columns = c(RESET_con, RESET_sin)
  ) |>
  tab_spanner(
    label = md("Multicolinealidad<br>&nbsp;"),
    columns = VIF_max
  ) |>
  tab_spanner(
    label = md("Observaciones Influyentes<br>(Cook > 0.15)"),
    columns = c(N_outliers, Paises_out)
  ) |>
  cols_label(
    Modelo     = "Modelo",
    BP_pval    = "p-valor",
    RESET_con  = "Muestra compl.",
    RESET_sin  = "Sin atípicos",
    VIF_max    = "VIF Máx",
    N_outliers = "N",
    Paises_out = "Países identificados"
  ) |>
  fmt_number(
    columns = VIF_max,
    decimals = 2
  ) |>
  fmt_number(
    columns = c(BP_pval, RESET_con, RESET_sin),
    decimals = 4
  ) |>
  cols_align(
    align = "center",
    columns = c(BP_pval, RESET_con, RESET_sin, VIF_max, N_outliers)
  ) |>
  cols_align(
    align = "left",
    columns = c(Modelo, Paises_out)
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  tab_source_note(
    source_note = md(paste0(
      "**Notas metodológicas:**<br>",
      "- **Breusch-Pagan:** H0 de homocedasticidad.<br>",
      "- **RESET de Ramsey:** H0 de correcta especificación de la forma funcional.<br>",
      "- **Multicolinealidad:** VIF Máx < 5 indica ausencia de multicolinealidad severa entre las covariables.<br>",
      "- **Distancia de Cook:** Se identifican observaciones influyentes con umbral Di > 0.15."
    ))
  )

# Imprimir en Viewer y guardar en PNG y HTML
tabla_diagnosticos_gt
gtsave(tabla_diagnosticos_gt, filename = "output/tablas/tabla_diagnosticos_resumen.png")
gtsave(tabla_diagnosticos_gt, filename = "output/tablas/tabla_diagnosticos_resumen.html")








