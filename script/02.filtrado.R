# install.packages(c("skimr", "dplyr", "tidyr", "ggplot2", "gt"))

library(tidyverse)
library(readxl)
library(gt) 


rm(list = ls())

options(scipen = 999)

wdi <- read_csv("raw/wdi_tabla_completa.csv")

wdi_limpio <- wdi |>
  filter(region != "Aggregates", !is.na(region), region != "") |>
  rename(pais = country, anio = year, ingreso = income, pbi_millones = pbi) |>
  select(-iso2c, -lending, -status, -lastupdated, -latitude, -longitude, -capital) |>
  mutate(pbi_millones = pbi_millones/1000000,
         poblacion_mill = poblacion/1000000,
         region = factor(region,
                         levels = c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean",
                                    "Middle East, North Africa, Afghanistan & Pakistan", "North America", "South Asia",
                                    "Sub-Saharan Africa"),
                         labels = c("Este de Asia y Pacífico", "Europa y Asia central", "América Latina y el Caribe",
                                    "Medio Oriente, Norte de África, Afganistán y Pakistán", "América del Norte", "Sur de Asia",
                                    "África subsahariana")),
         ingreso = factor(ingreso,
                          levels = c("Low income", "Lower middle income", "Upper middle income",
                                     "High income", "Not classified"),
                          labels = c("Ingreso bajo", "Ingreso medio-bajo", "Ingreso medio-alto",
                                     "Ingreso alto", "Sin clasificar")),
         across(where(is.numeric), ~ round(.x, 3)),
         ingreso2 = factor(ingreso,
                           levels = c("Ingreso bajo", "Ingreso medio-bajo", "Ingreso medio-alto",
                                      "Ingreso alto", "Sin clasificar"),
                           labels = c("Bajo", "Medio-bajo", "Medio-alto",
                                      "Alto", "Sin clasificar")), 
         industria_pc_wdi = (industria_pbi * pbi_per_capita) / 100,
         industria_per_capita = coalesce(industria_pc, industria_pc_wdi))


write.csv(wdi_limpio, "input/wdi_limpio.csv")


#Tabla que explica a que categoria corresponde cada variable
tipos <- tibble(
  variable = names(wdi_limpio),
  tipo_r   = sapply(wdi_limpio, function(x) class(x)[1]))

tipos

diccionario <- tribble(
  ~variable,                    ~descripcion,                                                                                             ~tipo,                 ~unidad,                          ~rango_teorico,   ~na_totales,      ~na_2001, ~na_2008, ~na_2022,
  "pais",                       "Nombre del país",                                                                                        "Categórica",          "-",                              "-",              0,                 0,        0,        0,
  "iso3c",                      "Código ISO de 3 letras (identificador único)",                                                           "Categórica",          "-",                              "-",              0,                 0,        0,        0,
  "region",                     "Región geográfica del Banco Mundial",                                                                    "Categórica",          "-",                              "7 categorías",   0,                 0,        0,        0,
  "anio",                       "Año de la observacion",                                                                                  "Numérica",            "Anual",                          "66 categorías",  0,                 0,        0,        0,
  "pbi_millones",               "PBI del país en millones de dólares",                                                                    "Numérica",            "Dolares constanes 2015",         "> 0",            2789,              18,       9,        9,
  "pbi_per_capita",             "PBI per capita ",                                                                                        "Numérica",            "Dolares constanes 2015",         "> 0",            2789,              18,       9,        9,
  "indice_nivel_precios",       "Factor de Paridad de Poder Adquisitivo (PPA) sobre el tipo de cambio nominal multiplicado por 100",      "Numérica",            "Indice",                         "> 0",            7299,              23,       17,       18,
  "industria_per_capita",       "PBI Industrial per capita",                                                                              "Numérica",            "Dolares constantes 2015",        "0 a 100",        5267,              36,       21,       19,
  "crecimiento_industria",      "Crecimiento de la industria",                                                                            "Numérica",            "% de crecimiento",               "> 0",            6232,              48,       35,       24,
  "tipo_cambio_real_efectivo",  "Nivel de Competitividad con base = 2010",                                                                "Numérica",            "Indice",                         "> 0",            10172,             122,      122,      125,
  "recursos_naturales",         "Rentas Económicas provenientes de los recursos naturales ",                                              "Numérica",            "% del PBI",                      "0 a 100",        5087,              17,       10,       217,
  "empleo_industrial",          "Cantidad de empleados en el sector industrial",                                                          "Numérica",            "% del total de empleados",       "0 a 100",        7791,              30,       30,       31,
  "ingreso",                    "Tipo de ingreso per capita del pais",                                                                    "Categórica",          "-",                              "5 categorías",   0,                 0,        0,        0,
  "exportacion_manufactura",    "Porcentaje de la exportacion en manufacturas del total de las mercancias",                                                           "Numérica",            "% del total",                    "0 a 100",        6383,              53,       50,       54,
  "gasto_gobierno_pbi",         "Gasto Total del Gobierno",                                                                               "Numérica",            "% del PBI",                      "0 a 100",        5593,              58,       46,       41,
  "gastos_totales_gob_pbi",     "Gasto total del gobierno como % PBI (inculye subsidios)",                                                "Numérica",            "% del PBI",                      "0 a 100",        9870,              58,       46,       41,
  "valor_agregado_trabajador",  "Valor agregado del trabajador de la industria",                                                          "Numérica",            "Dolares constantes 2015",        "> 0",            8735,              61,       46,       44,
  "ied_pbi",                    "Inversion extranjera directa",                                                                           "Numérica",            "% del PBI",                      "0 a 100",        5016,              30,       23,       22,
  "ahorro_bruto_pbi",           "Ahorro Bruto Nacional",                                                                                  "Numérica",            "% del PBI",                      "0 a 100",        7892,              89,       61,       59,
  "credito_privado_pbi",        "Credito al sector privado",                                                                              "Numérica",            "% del PBI",                      "0 a 100",        6685,              76,       54,       58,
  "poblacion_mill",             "Poblacion del pais en millones de habitantes",                                                           "Numérica",            "Anual",                          "> 0",            0,                 0,        0,        0)

tabla_imagen <- diccionario |> 
  gt() |> 
  tab_header(
    title = md("**Diccionario de Variables**"),
    subtitle = "Base de datos WDI - Banco Mundial"
  ) |> 
  opt_stylize(style = 1, color = "blue") |>  
  cols_align(align = "center")               

print(tabla_imagen)

gtsave(tabla_imagen, "output/tablas/diccionario_imagen.png")



#Filtramos la base en los anios elegidos
#Se podria elegir otro anio mas entre 2008 y 2022, por ejemplo 2015 (quedaria cada 7 anios)
#Filtramos por anio y por poblacion
#Los paises con menos de un millon de habitantes no son significativos para el trabajo
#En principio creemos que su sistema productivo no presenta una correlacion con las variables a analizar


wdi2 <- wdi_limpio |> 
  filter(anio %in% c(2001, 2008, 2022))
  
write.csv(wdi2, "input/wdi2.csv")

  wdi3 <- wdi_limpio |>
    filter(anio %in% c(2000, 2001, 2002, 2007, 2008, 2009, 2021, 2022, 2023), poblacion_mill >= 1) |> 
    arrange(pais, anio)

write.csv(wdi3, "input/wdi3.csv")
  