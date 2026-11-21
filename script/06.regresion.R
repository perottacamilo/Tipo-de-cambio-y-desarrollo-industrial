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
    "Ind: Simple"  = lm(log(industria_per_capita) ~ log(indice_nivel_precios), data = df_sub),
    
    "Ind: Control" = lm(log(industria_per_capita) ~ log(indice_nivel_precios) + recursos_naturales + 
                          ahorro_bruto_pbi + log(pbi_per_capita), data = df_sub),
    
    # --- EXPORTACIONES MANUFACTURERAS ---
    "Exp: Simple"  = lm(log(exportacion_manufactura_pc) ~ log(indice_nivel_precios), data = df_sub),
    
    "Exp: Control" = lm(log(exportacion_manufactura_pc) ~ log(indice_nivel_precios) + recursos_naturales + 
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
  )
}
tablas_por_anio$anio_2001
tablas_por_anio$anio_2008
tablas_por_anio$anio_2022
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
outliers_2001_ind <- obtener_outliers_cook(modelos_por_anio$anio_2001[["Ind: Control"]], wdi_2001, umbral = 0.15)
outliers_2008_ind <- obtener_outliers_cook(modelos_por_anio$anio_2008[["Ind: Control"]], wdi_2008, umbral = 0.15)
outliers_2022_ind <- obtener_outliers_cook(modelos_por_anio$anio_2022[["Ind: Control"]], wdi_2022, umbral = 0.15)

# Exportaciones
outliers_2001_exp <- obtener_outliers_cook(modelos_por_anio$anio_2001[["Exp: Control"]], wdi_2001, umbral = 0.15)
outliers_2008_exp <- obtener_outliers_cook(modelos_por_anio$anio_2008[["Exp: Control"]], wdi_2008, umbral = 0.15)
outliers_2022_exp <- obtener_outliers_cook(modelos_por_anio$anio_2022[["Exp: Control"]], wdi_2022, umbral = 0.15)
modelos_por_anio_sin_outliers <- list()
tablas_por_anio_2 <- list()
for (a in anios) {
  
  # 1. Filtrar datos del año
  df_sub <- wdi_regresion |> filter(anio == a)
  
  # 1b. Excluir outliers solo para la estimación de 2022
  if (a == 2022) {
  df_sub <- df_sub |> filter(!pais %in% outliers_2022_ind)
  }
  if (a == 2001) {
    df_sub <- df_sub |> filter(!pais %in% outliers_2001_ind)
  }
  if (a == 2008) {
    df_sub <- df_sub |> filter(!pais %in% outliers_2008_ind)
  }
  mods <- list(
    # --- INDUSTRIA (per cápita) ---
    "Ind: Control" = lm(log(industria_per_capita) ~ log(indice_nivel_precios) + recursos_naturales + 
                          ahorro_bruto_pbi + log(pbi_per_capita), data = df_sub)
  )
  
  # Guardar modelos
  modelos_por_anio_sin_outliers[[paste0("anio_", a)]] <- mods
  
  # 3. Generar la tabla comparativa para el Viewer
  tablas_por_anio_2[[paste0("anio_", a)]] <- modelsummary(
    mods,
    vcov = "HC1",
    title = paste0("Regresiones para Industria (", a, ")"),
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
  )
}
tablas_por_anio_2$anio_2022
tablas_por_anio_2$anio_2001
tablas_por_anio_2$anio_2008
gtsave(tablas_por_anio_2$anio_2001, filename = "output/tablas/tabla_2001_outlier.html")
gtsave(tablas_por_anio_2$anio_2008, filename = "output/tablas/tabla_2008_outlier.html")
gtsave(tablas_por_anio_2$anio_2022, filename = "output/tablas/tabla_2022_outlier.html")

# 2. Estimación de los modelos controlados para Exportaciones sin outliers
# --- 2001 ---
df_sub_2001_exp <- wdi_regresion |> 
  filter(anio == 2001, !pais %in% outliers_2001_exp)

mods_exp_2001 <- list(
  "Exp: Control" = lm(
    log(exportacion_manufactura_pc) ~ log(indice_nivel_precios) + recursos_naturales + 
      ahorro_bruto_pbi + log(pbi_per_capita), 
    data = df_sub_2001_exp
  )
)
modelos_por_anio_sin_outliers$anio_2001[["Exp: Control"]] <- mods_exp_2001[["Exp: Control"]]

tabla_2001_exp <- modelsummary(
  mods_exp_2001,
  vcov = "HC1",
  title = "Regresión para Exportaciones (2001)",
  stars = TRUE,
  statistic = "std.error",
  fmt = 3,
  coef_rename = c(
    "(Intercept)"          = "Constante",
    "indice_nivel_precios" = "Índice de nivel de precios",
    "recursos_naturales"   = "Rentas rec. naturales (% PBI)",
    "ahorro_bruto_pbi"     = "Ahorro bruto (% PBI)",
    "log(pbi_per_capita)"  = "log(PBI pc)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "N", "fmt" = 0),
    list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
    list("raw" = "adj.r.squared", "clean" = "R² ajustado", "fmt" = 3)
  ),
  output = "gt"
)
gtsave(tabla_2001_exp, filename = "output/tablas/tabla_2001_exp_outlier.html")

# --- 2008 ---
df_sub_2008_exp <- wdi_regresion |> 
  filter(anio == 2008, !pais %in% outliers_2008_exp)

mods_exp_2008 <- list(
  "Exp: Control" = lm(
    log(exportacion_manufactura_pc) ~ log(indice_nivel_precios) + recursos_naturales + 
      ahorro_bruto_pbi + log(pbi_per_capita), 
    data = df_sub_2008_exp
  )
)
modelos_por_anio_sin_outliers$anio_2008[["Exp: Control"]] <- mods_exp_2008[["Exp: Control"]]

tabla_2008_exp <- modelsummary(
  mods_exp_2008,
  vcov = "HC1",
  title = "Regresión para Exportaciones (2008)",
  stars = TRUE,
  statistic = "std.error",
  fmt = 3,
  coef_rename = c(
    "(Intercept)"          = "Constante",
    "indice_nivel_precios" = "Índice de nivel de precios",
    "recursos_naturales"   = "Rentas rec. naturales (% PBI)",
    "ahorro_bruto_pbi"     = "Ahorro bruto (% PBI)",
    "log(pbi_per_capita)"  = "log(PBI pc)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "N", "fmt" = 0),
    list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
    list("raw" = "adj.r.squared", "clean" = "R² ajustado", "fmt" = 3)
  ),
  output = "gt"
)
gtsave(tabla_2008_exp, filename = "output/tablas/tabla_2008_exp_outlier.html")

# --- 2022 ---
df_sub_2022_exp <- wdi_regresion |> 
  filter(anio == 2022, !pais %in% outliers_2022_exp)

mods_exp_2022 <- list(
  "Exp: Control" = lm(
    log(exportacion_manufactura_pc) ~ log(indice_nivel_precios) + recursos_naturales + 
      ahorro_bruto_pbi + log(pbi_per_capita), 
    data = df_sub_2022_exp
  )
)
modelos_por_anio_sin_outliers$anio_2022[["Exp: Control"]] <- mods_exp_2022[["Exp: Control"]]

# 3. Generar la tabla con modelsummary
tabla_2022_exp <- modelsummary(
  mods_exp_2022,
  vcov = "HC1",
  title = "Regresión para Exportaciones (2022)",
  stars = TRUE,
  statistic = "std.error",
  fmt = 3,
  coef_rename = c(
    "(Intercept)"          = "Constante",
    "indice_nivel_precios" = "Índice de nivel de precios",
    "recursos_naturales"   = "Rentas rec. naturales (% PBI)",
    "ahorro_bruto_pbi"     = "Ahorro bruto (% PBI)",
    "log(pbi_per_capita)"  = "log(PBI pc)"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "N", "fmt" = 0),
    list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
    list("raw" = "adj.r.squared", "clean" = "R² ajustado", "fmt" = 3)
  ),
  output = "gt"
)

# Visualizar en RStudio Viewer y guardar en HTML
tabla_2022_exp
gtsave(tabla_2022_exp, filename = "output/tablas/tabla_2022_exp_outlier.html")
#####tests de Breusch-Pagan#####
# 1. Creamos un data.frame vacío para ir guardando los resultados
resultados_bp <- data.frame()

# 2. Bucle para calcular el test en cada año y modelo
for (a in c(2001, 2008, 2022)) {
  
  # Extraemos los modelos
  mod_ind <- modelos_por_anio[[paste0("anio_", a)]][["Ind: Control"]]
  mod_exp <- modelos_por_anio[[paste0("anio_", a)]][["Exp: Control"]]
  
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
tabla_bp_final
#Regresiones
#*/


library(lmtest)
library(sandwich)
library(car)
library(tseries)
library(modelsummary)
library(ggplot2)
library(dplyr)
# ==============================================================================
# DIAGNÓSTICOS ECONOMÉTRICOS PARA CADA AÑO
# ==============================================================================

# ==============================================================================
# 1. MULTICOLINEALIDAD (VIF) PARA TODOS LOS AÑOS Y MODELOS
# ==============================================================================
library(car)

# Creamos una lista para almacenar los resultados
vifs_por_anio <- list()

for (a in c(2001, 2008, 2022)) {
  
  # Extraemos los modelos de control para cada año
  mod_ind <- modelos_por_anio[[paste0("anio_", a)]][["Ind: Control"]]
  mod_exp <- modelos_por_anio[[paste0("anio_", a)]][["Exp: Control"]]
  
  # Calculamos el VIF
  vif_ind <- car::vif(mod_ind)
  vif_exp <- car::vif(mod_exp)
  
  # Guardamos en la lista
  vifs_por_anio[[paste0("anio_", a)]] <- list(
    VIF_Industria = vif_ind,
    VIF_Exportaciones = vif_exp
  )
}

# Consultar los resultados
print("--- VIF Año 2001 ---")
print(vifs_por_anio$anio_2001)

print("--- VIF Año 2008 ---")
print(vifs_por_anio$anio_2008)

print("--- VIF Año 2022 ---")
print(vifs_por_anio$anio_2022)

library(dplyr)
library(gt)

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
      "indice_nivel_precios" ~ "Índice de nivel de precios",
      "recursos_naturales" ~ "Rentas rec. naturales (% PBI)",
      "ahorro_bruto_pbi" ~ "Ahorro bruto (% PBI)",
      "log(pbi_per_capita)" ~ "log(PBI pc)",
      .default = Variable
    )
  )

# Generamos la tabla con formato
tabla_vif |> 
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
mod_exp_2022 <- modelos_por_anio$anio_2022[["Exp: Control"]]
mod_exp_2001 <- modelos_por_anio$anio_2001[["Exp: Control"]]
mod_exp_2008 <- modelos_por_anio$anio_2008[["Exp: Control"]]

mod_ind_2022 <- modelos_por_anio$anio_2022[["Ind: Control"]]
mod_ind_2001 <- modelos_por_anio$anio_2001[["Ind: Control"]]
mod_ind_2008 <- modelos_por_anio$anio_2008[["Ind: Control"]]

mod_exp_2022_so <- modelos_por_anio_sin_outliers$anio_2022[["Exp: Control"]]
mod_ind_2022_so <- modelos_por_anio_sin_outliers$anio_2022[["Ind: Control"]]
mod_ind_2008_so <- modelos_por_anio_sin_outliers$anio_2008[["Ind: Control"]]
mod_ind_2001_so <- modelos_por_anio_sin_outliers$anio_2001[["Ind: Control"]]
resettest(mod_exp_2022_so)#sin outliers
resettest(mod_exp_2022, vcov = function(x) vcovHC(x, type = "HC1"))
resettest(mod_exp_2022)
resettest(mod_exp_2008)
resettest(mod_exp_2008, vcov = function(x) vcovHC(x, type = "HC1"))

resettest(mod_exp_2001)

resettest(mod_ind_2022)
resettest(mod_ind_2008)
resettest(mod_ind_2001)
# Guardar a PNG (descomentar si se define tabla_grafica)
# ggsave("tabla_reset_test.png", tabla_grafica, width = 7, height = 3, dpi = 300)
# 2. Dividimos la ventana en 4 (2 filas x 2 columnas)
par(mfrow = c(2, 2))

# 3. Generamos los gráficos con el color transparente
plot(mod_exp_2022, pch = 16, col = rgb(0, 0, 1, 0.1))
plot(mod_exp_2008, pch = 16, col = rgb(0, 0, 1, 0.1))
plot(mod_exp_2001, pch = 16, col = rgb(0, 0, 1, 0.1))


plot(mod_ind_2022, pch = 16, col = rgb(0, 0, 1, 0.1))
plot(mod_ind_2008, pch = 16, col = rgb(0, 0, 1, 0.1))
plot(mod_ind_2001, pch = 16, col = rgb(0, 0, 1, 0.1))

# 4. Volvemos la ventana a la normalidad (1 gráfico a la vez)
par(mfrow = c(1, 1))

# Función auxiliar para ver el ranking de observaciones influyentes por país
top_cook <- function(modelo, datos_df, n = 5) {
  d_cook <- cooks.distance(modelo)
  top <- head(sort(d_cook, decreasing = TRUE), n)
  data.frame(
    pais = datos_df[names(top), "pais"],
    cook_distance = round(unname(top), 4)
  )
}

print(top_cook(modelos_por_anio$anio_2022[["Exp: Control"]], wdi_2022))
print(top_cook(modelos_por_anio$anio_2022[["Ind: Control"]], wdi_2022))

print(top_cook(modelos_por_anio$anio_2001[["Exp: Control"]], wdi_2001))
print(top_cook(modelos_por_anio$anio_2001[["Ind: Control"]], wdi_2001))

print(top_cook(modelos_por_anio$anio_2008[["Exp: Control"]], wdi_2008))
print(top_cook(modelos_por_anio$anio_2008[["Ind: Control"]], wdi_2008))
#eliminamos ukraine(141) por estar en guerra y ser outlier 
#a el congo(28) por ser un pais particular con una estructura productiva rara
#lebanon lo eliminamos por estar en una crisis macroeconomica fuerte en ese año

#eliminamos a Zimbabwe por su situacion macro
#

# ==============================================================================
# GRÁFICOS: RESIDUALS VS LEVERAGE (DIAGNÓSTICO DE VALORES INFLUYENTES)
# ==============================================================================
# Crear carpeta de salida si no existe
dir.create("output/graficos/regresion", recursive = TRUE, showWarnings = FALSE)

# Función para graficar en pantalla y guardar en PNG con ejes en español y subtítulo
graficar_residuos_leverage <- function(modelo, datos_df, titulo, nombre_archivo, umbral_cook = 0.15, id.n = 3) {
  paises <- datos_df[names(residuals(modelo)), "pais"]
  cooks_d <- cooks.distance(modelo)
  colores <- ifelse(cooks_d > umbral_cook, "firebrick", rgb(0, 0, 1, 0.2))
  etiquetas <- paste0(paises, " (D = ", round(cooks_d, 3), ")")
  
  ruta_archivo <- file.path("output/graficos/regresion", nombre_archivo)
  
  # 1. Guardar a archivo PNG con letras grandes y legibles
  png(ruta_archivo, width = 5, height = 3.7, units = "in", res = 300, pointsize = 11)
  par(
    mar = c(4.0, 4.1, 3.2, 1.9),  # Márgenes ajustados (abajo, izq, arriba, der)
    mgp = c(2.6, 0.8, 0),         # Acerca nombres y números a los ejes
    las = 0                       # Mantiene los números del eje Y en vertical
  )
  plot(
    modelo,
    which = 5,
    ann = FALSE,
    sub.caption = "",             # Elimina el texto de la fórmula lm(...) debajo del eje x
    labels.id = etiquetas,
    id.n = id.n,
    col = colores,
    pch = 16,
    cex = 0.95,                   # Puntos más chicos y proporcionados al texto
    cex.id = 1.05,
    cex.caption = 1.1,
    cex.axis = 1.05,
    caption = list("", "", "", "", "Residuals vs Leverage")
  )
  title(main = titulo, line = 1.65, cex.main = 1.35, font.main = 2)
  title(
    xlab = "Apalancamiento (Leverage)",
    ylab = "Residuos estandarizados",
    cex.lab = 1.15
  )
  dev.off()
  
  # 2. Mostrar también en la ventana gráfica activa de RStudio (Plots) con letras grandes
  par(mar = c(4.8, 5.0, 3.8, 2.2))
  plot(
    modelo,
    which = 5,
    ann = FALSE,
    sub.caption = "",               # Elimina el texto de la fórmula lm(...) debajo del eje x
    labels.id = etiquetas,
    id.n = id.n,
    col = colores,
    pch = 16,
    cex = 1.1,
    cex.id = 1.05,
    cex.caption = 1.2,
    cex.axis = 1.1,
    caption = list("", "", "", "", "")
  )
  title(
    main = titulo,
    xlab = "Apalancamiento",
    ylab = "Residuos estandarizados",
    cex.main = 1.35,
    cex.lab = 1.2
  )
}

# --- EXPORTACIONES MANUFACTURERAS ---
graficar_residuos_leverage(
  modelo = mod_exp_2001,
  datos_df = wdi_2001,
  titulo = "Exportaciones Manufactureras 2001",
  nombre_archivo = "rvsl_exportaciones_2001.png",
  id.n = 1
)

graficar_residuos_leverage(
  modelo = mod_exp_2008,
  datos_df = wdi_2008,
  titulo = "Exportaciones Manufactureras 2008",
  nombre_archivo = "rvsl_exportaciones_2008.png",
  id.n = 2
)

graficar_residuos_leverage(
  modelo = mod_exp_2022,
  datos_df = wdi_2022,
  titulo = "Exportaciones Manufactureras 2022",
  nombre_archivo = "rvsl_exportaciones_2022.png",
  id.n = 2
)

# --- INDUSTRIA ---
graficar_residuos_leverage(
  modelo = mod_ind_2001,
  datos_df = wdi_2001,
  titulo = "Industria 2001",
  nombre_archivo = "rvsl_industria_2001.png",
  id.n = 2
)

graficar_residuos_leverage(
  modelo = mod_ind_2008,
  datos_df = wdi_2008,
  titulo = "Industria 2008",
  nombre_archivo = "rvsl_industria_2008.png",
  id.n = 1
)

graficar_residuos_leverage(
  modelo = mod_ind_2022,
  datos_df = wdi_2022,
  titulo = "Industria 2022",
  nombre_archivo = "rvsl_industria_2022.png",
  id.n = 1
)


