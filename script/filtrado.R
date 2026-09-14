# install.packages(c("tidyverse", "WDI", "skimr"))

library(tidyverse)
library(WDI)
library(skimr)
library(readxl)

rm(list = ls())

getwd()

wdi <- read_csv("~/GitHub/Trabajo-Practico-Ciencia-de-Datos/raw/wdi_tabla_completa.csv")

view(wdi_tabla_completa.csv)

wdi_limpio <- wdi |>
  filter(region != "Aggregates", !is.na(region), region != "") |> 
  rename(pais = country, anio = year, ingreso = income) |> 
  select(-iso2c, -status, -lastupdated, -status, -lending, -latitude, -longitude, -capital) |> 
  mutate(ingreso = factor(ingreso, 
                   levels = c("Low income", "Lower middle income", "Upper middle income", 
                              "High income", "Not classified"),
                   labels = c("Ingreso bajo", "Ingreso medio-bajo", "Ingreso medio-alto",
                              "Ingreso alto", "Sin clasificar")),
         mutate(across(where(is.numeric), ~ round(.x, 3)))
  )



view(wdi_limpio)

diccionario <- tribble(
  ~variable,        ~descripcion,                                          ~tipo,        ~unidad,          ~rango_teorico,
  "pais",           "Nombre del país",                                     "Categórica", "-",              "-",
  "iso3c",          "Código ISO de 3 letras (identificador único)",        "Categórica", "-",              "-",
  "region",         "Región geográfica del Banco Mundial",                 "Categórica", "-",              "7 categorías",
  "anio",        "Año de la observacion",                  "Numérica",    "Anual",              "66 categorías",
  "crecimiento_industria",       "Crecimiento de la industria",                          "Numérica",   "% de crecimiento", "> 0",
  "industria_pbi",          "Crecimient0 de la industria como % PBI",                     "Numérica",   "% del PBI",  "0 a 100",
  "tipo_cambio_oficial",    "Tipo de cambio ofical de cada pais",                    "Numérica",   "Moneda local",  "> 0",
  "tipo_cambio_real",   "Tipo de cambio real de cada pais",                 "Numérica",   "Moneda local", "> 0",
  "empleo_industrial",         "Cantidad de empleados en el sector industrial",                 "Numérica",   "% del total de empleados", "0 a 100",
  "exportacion_manufacturas",     "Porcentaje de la exportacion de manufacturas",           "Numérica",   "% del total",              "0 a 100",
  "gasto_gobierno",      "Gasto general del gobierno anual",          "Numérica",   "% del gasto",   "0 a 100",
  "gasto_gobierno_pbi",      "Gasto total del gobierno como % PBI",                                     "Numérica",   "% del PBI",       "0 a 100",
  "gastos_totales_gob_pbi",      "Gasto fiscal corriente del Estado",                                     "Numérica",   "% del PBI",       "0 a 100",
  "valor_agregado_trabajador",      "Valor agregado del trabajador de la industria",                                     "Numérica",   "Dolares constantes 2015",       "> 0",
  "ied_pbi",      "Inversion extranjera directa como % PBI",                                     "Numérica",   "% del PBI",       "0 a 100",
  "ahorro_bruto_pbi",      "Ahorro bruto nacional",                                     "Numérica",   "% del PBI",       "0 a 100",
  "ahorro_interno_bruto",      "Ahorro domestico bruto", "Numérica",   "% del PBI",       "0 a 100",
  "credito_privado_pbi",      "Credito al sector privado como % PBI",                                     "Numérica",   "% del PBI",       "0 a 100",
  "credito_privados_banco_pbi",      "Credito al sector privado de los bancos como % PBI",                                     "Numérica",   "% del PBI",       "0 a 100",
  "ingreso",      "Tipo de ingreso per capita del pais",                                     "Categórica",       "-", "5 categorías")




