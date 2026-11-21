# install.packages(c("WDI","readxl", "tidyverse"))

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
  ahorro_bruto_pbi = "NY.GNS.ICTR.ZS",
  credito_privado_pbi = "FS.AST.PRVT.GD.ZS",
  pbi_per_capita = "NY.GDP.PCAP.KD",
  pbi = "NY.GDP.MKTP.KD",
  indice_nivel_precios = "PA.NUS.GDP.PLI",
  recursos_naturales = "NY.GDP.TOTL.RT.ZS",
  poblacion = "SP.POP.TOTL",
  exportaciones_totales = "TX.VAL.MRCH.CD.WT"),
  country = "all",
  start = 1960,
  end = 2025,
  extra = TRUE)

argendata <- read_csv("auxiliar/pib_indust_per_capita_comparado.csv") |> 
  select(
    iso3c = geocodigoFundar, 
    year = anio, 
    industria_pc = gdp_indust_pc)

wdi <- wdi |> 
  left_join(argendata, by = c("iso3c", "year"))

write.csv(wdi, "raw/wdi_tabla_completa.csv")


