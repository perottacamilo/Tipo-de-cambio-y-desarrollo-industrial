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
  
  # 1b. Excluir outliers solo para la estimación de 2022
 # if (a == 2022) {
  #  df_sub <- df_sub |> filter(!pais %in% outliers_2022)
  #}
  
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
tablas_por_anio$anio_2022
tablas_por_anio$anio_2001
tablas_por_anio$anio_2008
gtsave(tablas_por_anio$anio_2001, filename = "output/tablas/tabla_2001.html")
gtsave(tablas_por_anio$anio_2008, filename = "output/tablas/tabla_2008.html")
gtsave(tablas_por_anio$anio_2022, filename = "output/tablas/tabla_2022.html")
# Industria: Congo Rep., Libia y Guinea superan el umbral (D_i > 0.15)
outliers_2001_ind <- c("Congo, Rep.", "Libya", "Guinea")
# ==============================================================================
# AÑO 2008
# ==============================================================================
# Industria: Lesotho, Hong Kong y Burundi forman el bloque influyente (D_i > 0.20)
outliers_2008_ind <- c("Lesotho", "Hong Kong SAR, China", "Burundi")
# ==============================================================================
# AÑO 2022
# ==============================================================================
# Industria: Zimbabue y Libia (D_i > 0.16)
outliers_2022_ind <- c("Zimbabwe", "Libya", "Zambia")
# Exportaciones: Líbano y R.D. del Congo (D_i > 0.14)
outliers_2022_exp <- c("Lebanon", "Congo, Dem. Rep.")
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

df_sub_2022_exp <- wdi_regresion |> 
  filter(anio == 2022, !pais %in% outliers_2022_exp)

# 2. Estimación del modelo controlado para Exportaciones
mods_exp_2022 <- list(
  "Exp: Control" = lm(
    log(exportacion_manufactura_pc) ~ log(indice_nivel_precios) + recursos_naturales + 
      ahorro_bruto_pbi + log(pbi_per_capita), 
    data = df_sub_2022_exp
  )
)

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
    "gasto_gobierno_pbi"   = "Gasto público (% PBI)",
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
# 1. Definir lista de modelos


# Guardar a PNG
ggsave("tabla_reset_test.png", tabla_grafica, width = 7, height = 3, dpi = 300)
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

head(sort(cooks.distance(modelos_por_anio$anio_2022[["Exp: Control"]]), decreasing = TRUE), 5)
head(sort(cooks.distance(modelos_por_anio$anio_2022[["Ind: Control"]]), decreasing = TRUE), 5)

head(sort(cooks.distance(modelos_por_anio$anio_2001[["Exp: Control"]]), decreasing = TRUE), 5)
head(sort(cooks.distance(modelos_por_anio$anio_2001[["Ind: Control"]]), decreasing = TRUE), 7)

head(sort(cooks.distance(modelos_por_anio$anio_2008[["Exp: Control"]]), decreasing = TRUE), 5)
head(sort(cooks.distance(modelos_por_anio$anio_2008[["Ind: Control"]]), decreasing = TRUE), 5)


# Cambiá "pais" por el nombre exacto de la columna que identifica a los países en tu base
paises_grafico_2022 <- wdi_2022[c(77, 28, 141,151,80,150), c("pais", "exportacion_manufactura_pc","industria_per_capita")]
paises_grafico_2008 <- wdi_2008[c(77, 57, 19, 145), c("pais", "exportacion_manufactura_pc")]
paises_grafico_2001 <- wdi_2001[c(28,76, 50, 64,54), c("pais", "exportacion_manufactura_pc")]
print(paises_grafico_2022)
print(paises_grafico_2008)
print(paises_grafico_2001)
#eliminamos ukraine(141) por estar en guerra y ser outlier 
#a el congo(28) por ser un pais particular con una estructura productiva rara
#lebanon lo eliminamos por estar en una crisis macroeconomica fuerte en ese año

#eliminamos a Zimbabwe por su situacion macro
#

# 1. Extraer los nombres de países respetando las observaciones usadas en el modelo (sin NAs)
paises_2022_exp <- df_sub[names(residuals(mod_exp_2022)), "pais"]

# 2. Definir un vector de colores: rojo para observaciones influyentes (Cook > 0.1), azul transparente para el resto
cooks_dist <- cooks.distance(mod_exp_2022)
colores_puntos <- ifelse(cooks_dist > 0.1, "firebrick", rgb(0, 0, 1, 0.2))
cooks_2022_exp <- round(cooks.distance(mod_exp_2022), 3)

# 2. Crear un vector de etiquetas que combine el país y la distancia de Cook
etiquetas_2022_exp <- paste0(paises_2022_exp, " (D = ", cooks_2022_exp, ")")
# 3. Graficar únicamente el gráfico 5 (Residuals vs Leverage)
plot(
  mod_exp_2022, 
  which = 5,                    # Muestra solo el gráfico de Residuals vs Leverage
  labels.id = etiquetas_2022_exp,    # Reemplaza los números (28, 77, 141) por los nombres
  id.n = 2,                     # Etiqueta los 5 casos más extremos
  col = colores_puntos,         # Aplica color rojo a los outliers
  pch = 16,
  main = "Exportaciones Manufactureras 2022",
  sub = ""
)
# ==============================================================================
# 1. INDUSTRIA 2001
# ==============================================================================
mod_ind_2001   <- modelos_por_anio$anio_2001[["Ind: Control"]]
df_2001        <- wdi_regresion |> filter(anio == 2001)
paises_2001    <- df_2001[names(residuals(mod_ind_2001)), "pais"]
colores_2001   <- ifelse(cooks.distance(mod_ind_2001) > 0.15, "firebrick", rgb(0, 0, 1, 0.2))
# 1. Obtener la distancia de Cook redondeada a 3 decimales
cooks_2001 <- round(cooks.distance(mod_ind_2001), 3)

# 2. Crear un vector de etiquetas que combine el país y la distancia de Cook
etiquetas_2001 <- paste0(paises_2001, " (D = ", cooks_2001, ")")
plot(
  mod_ind_2001,
  which = 5,
  labels.id = etiquetas_2001,
  id.n = 3,
  col = colores_2001,
  pch = 16,
  main = "Industria 2001",
  sub = ""
)
# ==============================================================================
# 2. INDUSTRIA 2008
# ==============================================================================
mod_ind_2008   <- modelos_por_anio$anio_2008[["Ind: Control"]]
df_2008        <- wdi_regresion |> filter(anio == 2008)
paises_2008    <- df_2008[names(residuals(mod_ind_2008)), "pais"]
colores_2008   <- ifelse(cooks.distance(mod_ind_2008) > 0.15, "firebrick", rgb(0, 0, 1, 0.2))
cooks_2008 <- round(cooks.distance(mod_ind_2008), 3)

# 2. Crear un vector de etiquetas que combine el país y la distancia de Cook
etiquetas_2008 <- paste0(paises_2008, " (D = ", cooks_2008, ")")
plot(
  mod_ind_2008,
  which = 5,
  labels.id = etiquetas_2008,
  id.n = 3,
  col = colores_2008,
  pch = 16,
  main = "Industria 2008",
  sub = ""
)

# ==============================================================================
# 3. INDUSTRIA 2022
# ==============================================================================
mod_ind_2022   <- modelos_por_anio$anio_2022[["Ind: Control"]]
df_2022        <- wdi_regresion |> filter(anio == 2022)
paises_2022    <- df_2022[names(residuals(mod_ind_2022)), "pais"]
colores_2022   <- ifelse(cooks.distance(mod_ind_2022) > 0.15, "firebrick", rgb(0, 0, 1, 0.2))
cooks_2022 <- round(cooks.distance(mod_ind_2022), 3)

# 2. Crear un vector de etiquetas que combine el país y la distancia de Cook
etiquetas_2022 <- paste0(paises_2022, " (D = ", cooks_2022, ")")
plot(
  mod_ind_2022,
  which = 5,
  labels.id = etiquetas_2022,
  id.n = 2,
  col = colores_2022,
  pch = 16,
  main = "Industria 2022",
  sub = ""
)

library(modelsummary)
library(ggplot2)

# Gráfico de coeficientes para el modelo de Exportaciones 2022
modelplot(
  modelos_por_anio$anio_2022[["Exp: Control"]], 
  vcov = "HC1", 
  coef_omit = "(Intercept)"
) +
  geom_vline(xintercept = 0, linetype = 2, color = "firebrick") +
  labs(
    x = "Efecto estimado (Cambio porcentual aprox.)",
    title = "Determinantes de las Exportaciones Manufactureras (2022)",
    subtitle = "Intervalos de confianza al 95% con errores robustos (HC1)"
  ) +
  theme_minimal()


#Pegue los codigos que estaban al principio de 04.graficos






anios <- c(2001, 2008, 2022)

# Crear la lista asignando nombres según el año
tablas_anios <- setNames(
  lapply(anios, function(a) {
    df_anio <- wdi2 |> filter(anio == a)
    
    mod1 <- lm(industria_per_capita ~ indice_nivel_precios, data = df_anio)
    mod2 <- lm(log(exportacion_manufactura_pc) ~ indice_nivel_precios, data = df_anio)
    
    modelsummary(
      list(
        "Industria (per cápita)" = mod1,
        "exportacion manufactura" = mod2
      ),
      title = paste0("Tabla: Regresiones simples para el nivel de precios (", a, ")"),
      stars = TRUE,
      statistic = "std.error",
      fmt = 3,
      coef_rename = c(
        "(Intercept)" = "Constante",
        "indice_nivel_precios" = "Índice de nivel de precios"
      ),
      gof_map = list(
        list("raw" = "nobs", "clean" = "N", "fmt" = 0),
        list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
        list("raw" = "adj.r.squared", "clean" = "R² ajustado", "fmt" = 3)
      ),
      output = "gt"
    )
  }),
  nm = paste0("anio_", anios)
)

# Ver la tabla del año 2001
tablas_anios$anio_2001

# Ver la tabla del año 2008
tablas_anios$anio_2008

# Ver la tabla del año 2022
tablas_anios$anio_2022

wdi_2001 <- wdi2 |>
  filter(anio == 2001)
wdi_2008 <- wdi2 |>
  filter(anio == 2008)
wdi_2022<- wdi2 |>
  filter(anio == 2022)


# Estimación de todos los modelos
mod1_2001 <- lm(industria_per_capita ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2001))
mod2_2001 <- lm(log(exportacion_manufactura_pc) ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2001))

mod1_2008 <- lm(industria_per_capita ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2008))
mod2_2008 <- lm(log(exportacion_manufactura_pc) ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2008))

mod1_2022 <- lm(industria_per_capita ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2022))
mod2_2022 <- lm(log(exportacion_manufactura_pc) ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2022))

# Muestra la tabla visualmente en la pestaña Viewer (al costado)
modelsummary(
  list(
    "Ind 2001" = mod1_2001, "Exp 2001" = mod2_2001,
    "Ind 2008" = mod1_2008, "Exp 2008" = mod2_2008,
    "Ind 2022" = mod1_2022, "Exp 2022" = mod2_2022
  ),
  title = "Tabla Comparativa: Regresiones simples (2001, 2008, 2022)",
  stars = TRUE,
  statistic = "std.error",
  fmt = 3,
  coef_rename = c(
    "(Intercept)" = "Constante",
    "indice_nivel_precios" = "Índice de nivel de precios"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "N", "fmt" = 0),
    list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
    list("raw" = "adj.r.squared", "clean" = "R² ajustado", "fmt" = 3)
  ),
  output = "gt" # <--- Se renderiza visualmente en la pestaña Viewer
)






