#install.packages(c("WDI","readxl"))

library(WDI)
library(readxl)

rm(list = ls())

getwd()

wdi <- WDI(indicator = c(
  crecimiento_industria = "NV.IND.TOTL.KD.ZG",
  industria_pbi = "NV.IND.TOTL.ZS",
  tipo_cambio_oficial = "PA.NUS.FCRF",
  tipo_cambio_real = "PX.REX.REER",
  empleo_industrial = "SL.IND.EMPL.ZS",
  exportacion_manufacturas = "TX.VAL.MANF.ZS.UN",
  gasto_gobierno_pbi = "NE.CON.GOVT.ZS",
  gastos_totales_gob_pbi = "GC.XPN.TOTL.GD.ZS",
  valor_agregado_trabajador = "NV.IND.EMPL.KD",
  ied_pbi = "BX.KLT.DINV.WD.GD.ZS",
  ahorro_bruto_pbi = "NY.GNS.ICTR.ZS",
  credito_privado_pbi = "FS.AST.PRVT.GD.ZS",
  factor_ppp = "PA.NUS.PPP"),
  country = "all",
  start = 1960,
  end = 2025,
  extra = TRUE)

write.csv(wdi, "raw/wdi_tabla_completa.csv")
