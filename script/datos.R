# install.packages(c("WDI","readxl"))
library(tidyverse)
library(WDI)
library(readxl)

rm(list = ls())

getwd()

wdi <- WDI(indicator = c(crecimiento_industria = "NV.IND.TOTL.KD.ZG",
  industria_pbi = "NV.IND.MANF.ZS", 
  tipo_cambio_real_efectivo = "PX.REX.REER",
  empleo_industrial = "SL.IND.EMPL.ZS",
  exportacion_manufactura = "TX.VAL.MANF.ZS.UN",
  gasto_gobierno_pbi = "NE.CON.GOVT.ZS",
  gastos_totales_gob_pbi = "GC.XPN.TOTL.GD.ZS",
  valor_agregado_trabajador = "NV.IND.EMPL.KD",
  ied_pbi = "BX.KLT.DINV.WD.GD.ZS",
  ahorro_bruto_pbi = "NY.GNS.ICTR.ZS",
  credito_privado_pbi = "FS.AST.PRVT.GD.ZS",
  pbi_per_capita = "NY.GDP.PCAP.KD",
  pbi = "NY.GDP.MKTP.KD",
  indice_nivel_precios = "PA.NUS.GDP.PLI",
  recursos_naturales = "NY.GDP.TOTL.RT.ZS",
  poblacion = "SP.POP.TOTL"),
  country = "all",
  start = 1960,
  end = 2025,
  extra = TRUE)

wdi <- wdi |> 
  # 1. Eliminamos las columnas administrativas que separan las filas 
  # (así R deja de pensar que son registros distintos por la fecha)
  select(-status, -lastupdated) |> 
  
  # 2. Le decimos a R: "mirá la base agrupando país por país y año por año"
  # (Ej: mirá las dos filas de Afganistán 2024 juntas)
  group_by(iso2c, iso3c, year) |> 
  
  # 3. EL "SUPER JOIN" (Aplastamiento):
  # across(everything(), ...): Recorre todas las columnas (crecimiento, manufactura, etc).
  # na.omit(x): Borra los vacíos (NA) de esa columna en ese bloque de filas.
  # first(): Agarra el primer dato real que sobrevivió.
  summarise(across(everything(), \(x) if(all(is.na(x))) NA else first(na.omit(x))), 
            .groups = "drop")








argendata <- read_csv("raw/pib_indust_per_capita_comparado.csv") |> 
  select(
    iso3c = geocodigoFundar, 
    year = anio, 
    industria_pc = gdp_indust_pc # Valor agregado manufacturero per cápita
  )

wdi <- wdi |> 
  left_join(argendata, by = c("iso3c", "year"))




write.csv(wdi, "raw/wdi_tabla_completa.csv")
