# Script Analisis cuantitativo - Regresiones y Clustering
#options(timeout = 600)
# install.packages(c("FactoMineR", "factoextra", "ggrepel", "cluster", "mclust", "lmtest", "sandwich", "car", "modelsummary" ))

library(readxl)
library(tidyverse)
library(factoextra)
library(cluster)      
library(factoextra)
library(ggrepel)
library(mclust)
library(lmtest)       
library(sandwich)     
library(car)           
library(modelsummary)
library(gt)
library(dplyr)
library(lmtest)
library(sandwich)
library(car)
library(tseries)
library(ggplot2)
# ===============
rm(list = ls())


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
gtsave(tablas_por_anio$anio_2001, filename = "output/tablas/tabla_2001.html")
gtsave(tablas_por_anio$anio_2008, filename = "output/tablas/tabla_2008.html")
gtsave(tablas_por_anio$anio_2022, filename = "output/tablas/tabla_2022.html")
# ==============================================================================
# DETECCIÓN AUTOMÁTICA DE OBSERVACIONES INFLUYENTES (DISTANCIA DE COOK)
# ==============================================================================
obtener_outliers_cook <- function(modelo, datos_df, umbral = 0.15) {
  d_cook <- cooks.distance(modelo)
  filas <- names(d_cook[d_cook > umbral & !is.na(d_cook)])
  datos_df[filas, "pais", drop = TRUE]
}

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

# Visualizar en RStudio Viewer y guardar en HTML
#tabla_sin_outliers
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

# Armamos el dataframe juntando los años
tabla_vif <- data.frame(
  Variable = nombres_vars,
  VIF_2001 = vifs_por_anio$anio_2001$VIF_Industria,
  VIF_2008 = vifs_por_anio$anio_2008$VIF_Industria,
  VIF_2022 = vifs_por_anio$anio_2022$VIF_Industria
) |> 
  # Renombramos las variables para que queden presentables
  mutate(
    Variable = case_match(
      Variable,
      "log(indice_nivel_precios)" ~ "log(Índice de nivel de precios)",
      "recursos_naturales" ~ "Rentas rec. naturales (% PBI)",
      "ahorro_bruto_pbi" ~ "Ahorro bruto (% PBI)",
      "log(pbi_per_capita)" ~ "log(PBI pc)",
      .default = Variable
    )
  )

# Generamos la tabla con formato
tabla_vif_final <- tabla_vif |> 
  gt() |> 
  tab_header(
    title = "Factor de Inflación de la Varianza (VIF)",
    subtitle = "Idéntico para los modelos de Industria y Exportaciones"
  ) |> 
  cols_label(
    VIF_2001 = "VIF (2001)",
    VIF_2008 = "VIF (2008)",
    VIF_2022 = "VIF (2022)"
  ) |> 
  fmt_number(
    columns = starts_with("VIF"),
    decimals = 2
  )

# Imprimimos la tabla
#tabla_vif_final
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

# Función para ejecutar el test RESET evaluando heterocedasticidad con Breusch-Pagan:
# Si BP p-valor < 0.05, aplica matriz robusta HC1; caso contrario, usa el test estándar.
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

# Guardar en archivo HTML
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
# DICCIONARIO DE TRADUCCIÓN DE PAÍSES AL ESPAÑOL
# ==============================================================================
paises_espanol <- c(
  "Congo, Rep."          = "Congo",
  "Congo, Dem. Rep."     = "R. D. Congo",
  "Sierra Leone"         = "Sierra Leona",
  "Sudan"                = "Sudán",
  "Hong Kong SAR, China" = "Hong Kong",
  "Iraq"                 = "Irak",
  "Libya"                = "Libia",
  "Gambia, The"          = "Gambia",
  "Yemen, Rep."          = "Yemen",
  "Algeria"              = "Argelia",
  "Cambodia"             = "Camboya",
  "Oman"                 = "Omán",
  "Timor-Leste"          = "Timor Oriental",
  "Botswana"             = "Botsuana",
  "Burundi"              = "Burundi",
  "Paraguay"             = "Paraguay",
  "Nepal"                = "Nepal",
  "Zambia"               = "Zambia",
  "Egypt, Arab Rep."     = "Egipto",
  "Iran, Islamic Rep."   = "Irán",
  "Syrian Arab Republic" = "Siria",
  "Russian Federation"   = "Rusia",
  "United States"        = "Estados Unidos",
  "United Kingdom"       = "Reino Unido",
  "Germany"              = "Alemania",
  "France"               = "Francia",
  "Italy"                = "Italia",
  "Spain"                = "España",
  "Korea, Rep."          = "Corea del Sur",
  "Korea, Dem. People's Rep." = "Corea del Norte",
  "Venezuela, RB"        = "Venezuela"
)

traducir_pais <- function(x) {
  unname(ifelse(x %in% names(paises_espanol), paises_espanol[x], x))
}

# Función auxiliar para ver el ranking de observaciones influyentes por país
top_cook <- function(modelo, datos_df, n = 5) {
  d_cook <- cooks.distance(modelo)
  top <- head(sort(d_cook, decreasing = TRUE), n)
  data.frame(
    pais = traducir_pais(datos_df[names(top), "pais"]),
    cook_distance = round(unname(top), 4)
  )
}

#print(top_cook(modelos_por_anio$anio_2022[["log-log Exp: Control"]], wdi_2022))
#print(top_cook(modelos_por_anio$anio_2022[["log-log Ind: Control"]], wdi_2022))

#print(top_cook(modelos_por_anio$anio_2001[["log-log Exp: Control"]], wdi_2001))
#print(top_cook(modelos_por_anio$anio_2001[["log-log Ind: Control"]], wdi_2001))

#print(top_cook(modelos_por_anio$anio_2008[["log-log Exp: Control"]], wdi_2008))
#print(top_cook(modelos_por_anio$anio_2008[["log-log Ind: Control"]], wdi_2008))



# ==============================================================================
# GRÁFICOS: RESIDUALS VS LEVERAGE (FORMATO EXPLORATORIO - OWID)
# ==============================================================================
suppressPackageStartupMessages({
  library(scales)
  library(ggtext)
  library(patchwork)
  library(showtext)
})

# 1. Configuración tipográfica y tema visual estilo "graficos entrega 3.R"
fuentes_ok <- tryCatch({
  font_add_google("Playfair Display", "playfair", regular.wt = 600, bold.wt = 700)
  font_add_google("Lato", "lato", regular.wt = 400, bold.wt = 700)
  TRUE
}, error = function(e) FALSE)

fuente_titulo <- if (fuentes_ok) "playfair" else "serif"
fuente_texto  <- if (fuentes_ok) "lato" else "sans"

showtext_auto()
showtext_opts(dpi = 96)

col <- c(rojo = "#B13507", azul = "#286BBB", gris = "#BDBDBD")
col_asia    <- "#E0195B"
col_latam   <- "#3919E0"
gris_titulo <- "#363636"
gris_texto  <- "#5B5B5B"
gris_suave  <- "#858585"

theme_owid <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = fuente_texto) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_markdown(family = fuente_titulo, size = rel(2.9), color = gris_titulo,
                                       lineheight = 1.05, margin = margin(b = 10)),
      plot.subtitle = element_text(size = rel(1.75), color = gris_texto,
                                   lineheight = 1.15, margin = margin(b = 20)),
      plot.caption  = element_text(size = rel(0.95), color = gris_suave, hjust = 0,
                                   margin = margin(t = 18)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "#DDDDDD", linetype = "dashed", linewidth = 0.4),
      axis.line.x  = element_line(color = "#999999", linewidth = 0.4),
      axis.ticks.x = element_line(color = "#999999", linewidth = 0.4),
      axis.text    = element_text(color = "#666666", size = rel(1.05)),
      axis.title   = element_blank(),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 22, 15, 20)
    )
}

pintar <- function(texto, color) sprintf("<span style='color:%s'><b>%s</b></span>", color, texto)

# Crear carpeta de salida si no existe
dir.create("output/graficos/regresion", recursive = TRUE, showWarnings = FALSE)

# Función para armar panel ggplot de Residuos vs Leverage
panel_rvsl <- function(modelo, datos_df, titulo, subtitulo = NULL,
                       ind = "Industria", umbral_cook = 0.15, es_individual = FALSE) {
  res_std    <- rstandard(modelo)
  lev        <- hatvalues(modelo)
  cooks_d    <- cooks.distance(modelo)
  paises_raw <- datos_df[names(res_std), "pais"]
  paises     <- traducir_pais(paises_raw)
  p_k        <- length(coef(modelo))
  
  df <- data.frame(
    pais       = paises,
    leverage   = unname(lev),
    res_std    = unname(res_std),
    cook       = unname(cooks_d),
    es_outlier = unname(cooks_d > umbral_cook),
    stringsAsFactors = FALSE
  )
  
  outliers <- df$pais[df$es_outlier]
  if (is.null(subtitulo)) {
    if (length(outliers) > 0) {
      subtitulo <- paste0("Atípicos (D > 0,15): ", paste(outliers, collapse = ", "))
    } else {
      subtitulo <- "Sin atípicos severos (D > 0,15)"
    }
  }
  
  # Curva de contorno crítico de Cook D = 0.15
  max_h  <- max(df$leverage, na.rm = TRUE) * 1.15
  grid_h <- seq(0.005, min(max_h, 0.45), length.out = 150)
  val_r  <- sqrt(p_k * umbral_cook * (1 - grid_h) / grid_h)
  df_cook_line <- data.frame(
    h = c(grid_h, grid_h),
    r = c(val_r, -val_r),
    curva = rep(c("pos", "neg"), each = length(grid_h))
  ) %>% filter(abs(r) <= max(abs(df$res_std), na.rm = TRUE) * 1.25)
  
  tit_y <- if (es_individual) "Residuos estandarizados" else "Residuos estandarizados"
  tit_x <- if (es_individual || ind == "Exportaciones") "Apalancamiento (Leverage)" else NULL
  
  p <- ggplot(df, aes(x = leverage, y = res_std)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#BBBBBB", linewidth = 0.4) +
    # Puntos regulares
    geom_point(data = filter(df, !es_outlier), shape = 21, size = if (es_individual) 1.8 else 1.3,
               stroke = 0.25, color = "#9E9E9E", fill = alpha(col[["gris"]], 0.65)) +
    # Curva loess de tendencia
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                color = gris_titulo, linewidth = if (es_individual) 0.6 else 0.5) +
    # Puntos influyentes en rojo
    geom_point(data = filter(df, es_outlier), shape = 21, size = if (es_individual) 2.6 else 2.0,
               stroke = 0.4, color = "white", fill = col[["rojo"]]) +
    # Línea de isocontorno de Cook D = 0.15 si cae en el rango
    { if(nrow(df_cook_line) > 0)
        geom_line(data = df_cook_line, aes(x = h, y = r, group = curva),
                  linetype = "dotted", color = alpha(col[["rojo"]], 0.7), linewidth = 0.4)
    } +
    # Etiquetas de países atípicos
    geom_label_repel(
      data = filter(df, es_outlier),
      aes(label = paste0(pais, "\n(D = ", round(cook, 3), ")")),
      family = fuente_texto,
      fontface = "bold",
      size = if (es_individual) 4.2 else 3.8,
      fill = alpha("white", 0.85),
      label.size = 0,
      label.padding = unit(1.5, "pt"),
      color = gris_titulo,
      min.segment.length = 0,
      segment.color = "#777777",
      nudge_y = 0.35,
      seed = 1
    ) +
    labs(title = titulo, subtitle = subtitulo, x = tit_x, y = tit_y) +
    theme_owid(base_size = if (es_individual) 11 else 9.5) %+replace%
    theme(
      plot.title    = element_text(family = fuente_texto, face = "bold",
                                   size = if (es_individual) rel(1.5) else rel(3.5),
                                   color = gris_titulo, hjust = 0, margin = margin(b = 3)),
      plot.subtitle = element_text(size = if (es_individual) rel(1.05) else rel(2),
                                   color = gris_texto, hjust = 0,
                                   lineheight = 0.6, margin = margin(b = 14)),
      axis.title.x  = element_text(size = if (es_individual) rel(1.05) else rel(1.75),
                                   color = gris_texto, margin = margin(t = 6)),
      axis.title.y  = element_text(size = if (es_individual) rel(1.05) else rel(1.75),
                                   color = gris_texto, angle = 90, margin = margin(r = 6)),
      panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      axis.line.x = element_blank(), axis.ticks.x = element_blank(),
      plot.background = element_rect(fill = "#FAFAFA", color = "#E3E3E3"),
      plot.margin = margin(8, 10, 8, 8)
    )
  
  p
}

# Función para guardar cada gráfico individual en PNG
graficar_residuos_leverage <- function(modelo, datos_df, titulo, nombre_archivo,
                                      umbral_cook = 0.15, ind = "Industria") {
  p <- panel_rvsl(
    modelo = modelo,
    datos_df = datos_df,
    titulo = titulo,
    ind = ind,
    umbral_cook = umbral_cook,
    es_individual = TRUE
  )
  ruta_archivo <- file.path("output/graficos/regresion", nombre_archivo)
  ggsave(ruta_archivo, plot = p, width = 6.2, height = 4.2, dpi = 300)
  invisible(p)
}

# --- GUARDAR GRÁFICOS INDIVIDUALES ---
graficar_residuos_leverage(
  modelo = mod_exp_2001,
  datos_df = wdi_2001,
  titulo = "Exportaciones Manufactureras (per cápita) 2001",
  nombre_archivo = "rvsl_exportaciones_2001.png",
  ind = "Exportaciones"
)

graficar_residuos_leverage(
  modelo = mod_exp_2008,
  datos_df = wdi_2008,
  titulo = "Exportaciones Manufactureras (per cápita) 2008",
  nombre_archivo = "rvsl_exportaciones_2008.png",
  ind = "Exportaciones"
)

graficar_residuos_leverage(
  modelo = mod_exp_2022,
  datos_df = wdi_2022,
  titulo = "Exportaciones Manufactureras (per cápita) 2022",
  nombre_archivo = "rvsl_exportaciones_2022.png",
  ind = "Exportaciones"
)

graficar_residuos_leverage(
  modelo = mod_ind_2001,
  datos_df = wdi_2001,
  titulo = "Industria (per cápita) 2001",
  nombre_archivo = "rvsl_industria_2001.png",
  ind = "Industria"
)

graficar_residuos_leverage(
  modelo = mod_ind_2008,
  datos_df = wdi_2008,
  titulo = "Industria (per cápita) 2008",
  nombre_archivo = "rvsl_industria_2008.png",
  ind = "Industria"
)

graficar_residuos_leverage(
  modelo = mod_ind_2022,
  datos_df = wdi_2022,
  titulo = "Industria (per cápita) 2022",
  nombre_archivo = "rvsl_industria_2022.png",
  ind = "Industria"
)

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

juntar_rvsl <- function(paneles, titulo, subtitulo, caption) {
  wrap_plots(paneles, ncol = 3) +
    plot_annotation(
      title = titulo, 
      subtitle = subtitulo, 
      caption = caption, 
      theme = theme_owid() +
        theme(
          plot.title    = ggtext::element_markdown(family = fuente_texto, face = "bold",
                                                   size = 45, hjust = 0, halign = 0,
                                                   lineheight = 0.4, margin = margin(b = 6)),
          plot.subtitle = ggtext::element_markdown(size = 30, hjust = 0, halign = 0,
                                                   lineheight = 0.35, margin = margin(b = 12)),
          plot.caption  = element_text(size = 18, hjust = 0, lineheight = 0.4, 
                                       color = gris_texto, margin = margin(t = 10)),
          plot.margin   = margin(5, 5, 5, 5)
        )
    )
}

titulo_rvsl <- paste0(
  "Diagnóstico de Residuos vs. Apalancamiento (*Leverage*):<br>",
  "Observaciones influyentes identificadas mediante la ",
  pintar("Distancia de Cook", col[["rojo"]]), " (2001, 2008 y 2022)"
)

subtitulo_rvsl <- paste0(
  "Cada punto representa un país en las estimaciones con controles. ",
  "En ", pintar("rojo", col[["rojo"]]), " se identifican los países atípicos (D<sub>i</sub> > 0,15).<br>",
  "La curva gris muestra la tendencia local LOESS y la línea de puntos roja señala el contorno crítico de Cook (D = 0,15)."
)

caption_rvsl <- paste0(
  "Fuente: elaboración propia en base a Banco Mundial (WDI) y Argendata (Fundar).\n",
  "Notas: Modelos log-log con controles. Umbral de corte de influencia empírica fijado en Di > 0,15."
)

grafico_rvsl_exploratorio <- juntar_rvsl(
  paneles = paneles_rvsl,
  titulo = titulo_rvsl,
  subtitulo = subtitulo_rvsl,
  caption = caption_rvsl
)

ggsave("output/graficos/regresion/rvsl_exploratorio_completo.jpg", plot = grafico_rvsl_exploratorio,
       width = 22, height = 14, units = "cm", dpi = 300)
ggsave("output/graficos/regresion/rvsl_exploratorio_completo.png", plot = grafico_rvsl_exploratorio,
       width = 22, height = 14, units = "cm", dpi = 300)

# ==============================================================================
# TABLA RESUMEN COMPARATIVA: SIMPLE vs CON CONTROLES vs SIN OUTLIERS
# ==============================================================================

# Función auxiliar para armar la lista de modelos en el orden solicitado
armar_modelos_resumen <- function(dep) {
  res <- list()
  for (a in anios) {
    simple_name  <- paste0("log-log ", dep, ": Simple")
    control_name <- paste0("log-log ", dep, ": Control")
    
    res[[paste0("Simple (", a, ")")]]       <- modelos_por_anio[[paste0("anio_", a)]][[simple_name]]
    res[[paste0("Controles (", a, ")")]]    <- modelos_por_anio[[paste0("anio_", a)]][[control_name]]
    res[[paste0("Sin atípicos (", a, ")")]] <- modelos_por_anio_sin_outliers[[paste0("anio_", a)]][[control_name]]
  }
  return(res)
}

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

# Imprimir en Viewer y guardar en HTML
tabla_resumen_modelos
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
    label = "Heterocedasticidad (Breusch-Pagan)",
    columns = BP_pval
  ) |>
  tab_spanner(
    label = "Especificación RESET (p-valor)",
    columns = c(RESET_con, RESET_sin)
  ) |>
  tab_spanner(
    label = "Multicolinealidad",
    columns = VIF_max
  ) |>
  tab_spanner(
    label = "Observaciones Influyentes (Cook > 0.15)",
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
      "- **Multicolinealidad:** VIF Máx < 5 indica ausencia de multicolinealidad severa entre las covariables.<br>",
      "- **Distancia de Cook:** Se identifican observaciones influyentes con umbral Di > 0.15."
    ))
  )

# Imprimir en Viewer y guardar en HTML
tabla_diagnosticos_gt
gtsave(tabla_diagnosticos_gt, filename = "output/tablas/tabla_diagnosticos_resumen.html")






