# ==============================================================================
# 0. CONFIGURACIÓN Y LIBRERÍAS
# ==============================================================================
options(scipen = 999)

library(tidyverse)
library(gt)
library(here)

source(here::here("utils", "traducir_pais_wdi.R"))
# ==============================================================================
# 1. CARGA, LIMPIEZA Y TRANSFORMACIÓN DE LA BASE COMPLETA (WDI + ARGENDATA)
# ==============================================================================
wdi <- read_csv("raw/wdi_tabla_completa.csv")

wdi_limpio <- wdi |>
  filter(region != "Aggregates", !is.na(region), region != "") |>
  rename(
    pais         = country,
    anio         = year,
    ingreso      = income,
    pbi_millones = pbi
  ) |>
  select(-iso2c, -lending, -status, -lastupdated, -latitude, -longitude, -capital) |>
  mutate(
    pbi_millones   = pbi_millones / 1000000,
    poblacion_mill = poblacion / 1000000,
      region = factor(
      region,
      levels = c(
        "East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean",
        "Middle East, North Africa, Afghanistan & Pakistan", "North America",
        "South Asia", "Sub-Saharan Africa"),
      labels = c(
        "Este de Asia y Pacífico", "Europa y Asia central", "América Latina y el Caribe",
        "Medio Oriente, Norte de África, Afganistán y Pakistán", "América del Norte",
        "Sur de Asia", "África subsahariana")),
    ingreso = factor(
      ingreso,
      levels = c("Low income", "Lower middle income", "Upper middle income", "High income", "Not classified"),
      labels = c("Ingreso bajo", "Ingreso medio-bajo", "Ingreso medio-alto", "Ingreso alto", "Sin clasificar")),
    ingreso2 = factor(
      ingreso,
      levels = c("Ingreso bajo", "Ingreso medio-bajo", "Ingreso medio-alto", "Ingreso alto", "Sin clasificar"),
      labels = c("Bajo", "Medio-bajo", "Medio-alto", "Alto", "Sin clasificar")),
    
        across(where(is.numeric), ~ round(.x, 3)),
    
    # Construcción de variables principales per cápita
    # Industria: prioriza Argendata (industria_pc) y completa faltantes con WDI
    industria_pc_wdi           = (industria_pbi * pbi_per_capita) / 100,
    industria_per_capita       = coalesce(industria_pc, industria_pc_wdi),
    exportacion_manufactura_pc = ((exportacion_manufactura * exportaciones_totales) / 100) / poblacion) |> 
  mutate(pais = traducir_pais_wdi(pais))


write.csv(wdi_limpio, "input/wdi_limpio.csv", row.names = FALSE)


# ==============================================================================
# 2. INSPECCIÓN DE TIPOS Y DICCIONARIO DE VARIABLES
# ==============================================================================
tipos <- tibble(
  variable = names(wdi_limpio),
  tipo_r   = sapply(wdi_limpio, function(x) class(x)[1])
)
print(tipos)

diccionario <- tribble(
  ~variable,                    ~descripcion,                                                                                        ~tipo,        ~unidad,                   ~rango_teorico,  ~na_totales, ~na_2001, ~na_2008, ~na_2022,
  "pais",                       "Nombre del país",                                                                                   "Categórica", "-",                       "-",             0,           0,        0,        0,
  "iso3c",                      "Código ISO de 3 letras (identificador único)",                                                      "Categórica", "-",                       "-",             0,           0,        0,        0,
  "anio",                       "Año de la observación",                                                                             "Numérica",   "Anual",                   "66 años",       0,           0,        0,        0,
  "crecimiento_industria",      "Variación anual del sector industrial",                                                             "Numérica",   "% de crecimiento",        "Sin límite",    6232,        48,       35,       24,
  "industria_pbi",              "Valor agregado de la industria manufacturera como porcentaje del PBI",                              "Numérica",   "% del PBI",               "0 a 100",       6069,        47,       29,       28,
  "tipo_cambio_real_efectivo",  "Nivel de competitividad con base = 2010",                                                           "Numérica",   "Índice",                  "> 0",           10219,       123,      123,      126,
  "empleo_industrial",          "Empleo en el sector industrial",                                                                    "Numérica",   "% del total de empleados","0 a 100",       7791,        30,       30,       31,
  "exportacion_manufactura",    "Porcentaje de exportaciones manufactureras sobre el total de mercancías",                           "Numérica",   "% del total",             "0 a 100",       6282,        53,       50,       53,
  "gasto_gobierno_pbi",         "Gasto de consumo final del gobierno",                                                               "Numérica",   "% del PBI",               "0 a 100",       5593,        58,       46,       41,
  "gastos_totales_gob_pbi",     "Gasto total del gobierno como % del PBI (incluye subsidios)",                                       "Numérica",   "% del PBI",               "0 a 100",       9870,        126,      100,      106,
  "valor_agregado_trabajador",  "Valor agregado por trabajador industrial",                                                          "Numérica",   "Dólares constantes 2015", "> 0",           8735,        61,       46,       44,
  "ahorro_bruto_pbi",           "Ahorro bruto nacional",                                                                             "Numérica",   "% del PBI",               "0 a 100",       7845,        89,       61,       58,
  "credito_privado_pbi",        "Crédito interno al sector privado",                                                                 "Numérica",   "% del PBI",               "0 a 100",       6648,        76,       54,       55,
  "pbi_per_capita",             "PBI per cápita",                                                                                    "Numérica",   "Dólares constantes 2015", "> 0",           2789,        18,       9,        9,
  "pbi_millones",               "PBI del país en millones de dólares",                                                               "Numérica",   "Dólares constantes 2015", "> 0",           2789,        18,       9,        9,
  "indice_nivel_precios",       "Factor de Paridad de Poder Adquisitivo (PPA) sobre el tipo de cambio nominal multiplicado por 100", "Numérica",   "Índice (EE.UU. = 100)",   "> 0",           7299,        23,       17,       18,
  "recursos_naturales",         "Rentas económicas provenientes de los recursos naturales",                                          "Numérica",   "% del PBI",               "0 a 100",       5087,        17,       10,       217,
  "region",                     "Región geográfica del Banco Mundial",                                                               "Categórica", "-",                       "7 categorías",  0,           0,        0,        0,
  "ingreso",                    "Clasificación de ingreso per cápita del país",                                                      "Categórica", "-",                       "5 categorías",  0,           0,        0,        0,
  "industria_pc",               "PBI industrial per cápita (serie Argendata)",                                                       "Numérica",   "Dólares constantes 2015", "> 0",           3882,        15,       11,       11,
  "poblacion_mill",             "Población del país en millones de habitantes",                                                      "Numérica",   "Millones de habitantes",  "> 0",           0,           0,        0,        0,
  "industria_pc_wdi",           "PBI industrial per cápita estimado con indicadores WDI",                                            "Numérica",   "Dólares constantes 2015", "> 0",           6167,        50,       30,       29,
  "industria_per_capita",       "PBI industrial per cápita",                                                                         "Numérica",   "Dólares constantes 2015", "> 0",           3047,        12,       8,        6,
  "exportacion_manufactura_pc", "Exportaciones de manufacturas per cápita",                                                          "Numérica",   "Dólares corrientes",      ">= 0",          6406,        57,       53,       54)

diccionario <- diccionario |>
  gt() |>
  tab_header(
    title    = md("**Diccionario de Variables**"),
    subtitle = "Base de datos WDI (Banco Mundial) y Argendata (Fundar)"
  ) |>
  opt_stylize(style = 1, color = "blue") |>
  cols_align(align = "center")

print(diccionario)
gtsave(diccionario, "output/tablas/diccionario.png")


# ==============================================================================
# 3. RECORTES DE LA BASE PARA ANÁLISIS E INTERPOLACIÓN
# ==============================================================================

# 3.1 Base de los 3 cortes principales sin filtrar población (651 obs.)
# Se usa para análisis descriptivo previo, conteo de faltantes y comparación antes/después
wdi2 <- wdi_limpio |>
  filter(anio %in% c(2001, 2008, 2022))

write.csv(wdi2, "input/wdi2.csv", row.names = FALSE)

# 3.2 Base con años adyacentes
# Incluye los años ±1 de cada corte para poder interpolar datos faltantes en el script siguiente
wdi3 <- wdi_limpio |>
  filter(
    anio %in% c(2000, 2001, 2002, 2007, 2008, 2009, 2021, 2022, 2023),
    poblacion_mill >= 1
  ) |>
  arrange(pais, anio)

write.csv(wdi3, "input/wdi3.csv", row.names = FALSE)
