# install.packages(c("tidyverse", "WDI", "skimr"))

library(tidyverse)
library(WDI)
library(skimr)
library(readxl)

rm(list = ls())

options(scipen = 999)

getwd()

wdi <- read_csv("~/GitHub/Trabajo-Practico-Ciencia-de-Datos/raw/wdi_tabla_completa.csv")

wdi_limpio <- wdi |>
  filter(region != "Aggregates", !is.na(region), region != "") |>
  rename(pais = country, anio = year, ingreso = income) |>
  select(-iso2c, -status, -lastupdated, -status, -lending, -latitude, -longitude, -capital) |>
  mutate(indice_precios_relativos = tipo_cambio_oficial/factor_ppp,
         ingreso = factor(ingreso,
                   levels = c("Low income", "Lower middle income", "Upper middle income",
                              "High income", "Not classified"),
                   labels = c("Ingreso bajo", "Ingreso medio-bajo", "Ingreso medio-alto",
                              "Ingreso alto", "Sin clasificar")),
         mutate(across(where(is.numeric), ~ round(.x, 3))))


diccionario <- tribble(
  ~variable,                    ~descripcion,                                             ~tipo,        ~unidad,                          ~rango_teorico, ~na_totales,      ~na_2001, ~na_2008, ~na_2022,
  "pais",                       "Nombre del país",                                        "Categórica", "-",                              "-",              0,           0,        0,        0,
  "iso3c",                      "Código ISO de 3 letras (identificador único)",           "Categórica", "-",                              "-",              0,           0,        0,        0,
  "region",                     "Región geográfica del Banco Mundial",                    "Categórica", "-",                              "7 categorías",   0,           0,        0,        0,
  "anio",                       "Año de la observacion",                                  "Numérica",   "Anual",                          "66 categorías",  0,           0,        0,        0,
  "crecimiento_industria",      "Crecimiento de la industria",                            "Numérica",   "% de crecimiento",               "> 0",            107,         48,       35,       24,
  "industria_pbi",              "Participacion de la industria en el PBI",                "Numérica",   "% del PBI",                      "0 a 100",        76,          36,       21,       19,
  "indice_precios_relativos",   "Diferencia entre los precios generales del pais con respecto a EEUU",    "Numérica",   "Indice",          "> 0",           77,          29,       24,       24,
  "factor_ppp",                 "Factor de conversion de Paridad de Poder Adquisitivo",    "Numérica",   "Conversión en dólares",          "> 0",           58,          23,       17,       18,
  "tipo_cambio_oficial",        "Tipo de cambio oficial de cada pais",                     "Numérica",   "Moneda local",                   "> 0",           33,          12,       11,       10,
  "tipo_cambio_real",           "Tipo de cambio real de cada pais",                        "Numérica",   "Moneda local",                   "> 0",           369,         122,      122,      125,
  "empleo_industrial",          "Cantidad de empleados en el sector industrial",           "Numérica",   "% del total de empleados",       "0 a 100",       91,          30,       30,       31,
  "exportacion_manufacturas",   "Porcentaje de la exportacion de manufacturas",            "Numérica",   "% del total",                    "0 a 100",       157,         53,       50,       54,
  "gasto_gobierno_pbi",         "Gasto total del gobierno como % PBI",                     "Numérica",   "% del PBI",                      "0 a 100",       145,         58,       46,       41,
  "gastos_totales_gob_pbi",     "Gasto total del gobierno como % PBI (inculye subsidios)", "Numérica",   "% del PBI",                      "0 a 100",       332,         58,       46,       41,
  "valor_agregado_trabajador",  "Valor agregado del trabajador de la industria",           "Numérica",   "Dolares constantes 2015",        "> 0",           151,         61,       46,       44,
  "ied_pbi",                    "Inversion extranjera directa como % PBI",                 "Numérica",   "% del PBI",                      "0 a 100",       75,          30,       23,       22,
  "ahorro_bruto_pbi",           "Ahorro bruto nacional",                                   "Numérica",   "% del PBI",                      "0 a 100",       209,         89,       61,       59,
  "credito_privado_pbi",        "Credito al sector privado como % PBI",                    "Numérica",   "% del PBI",                      "0 a 100",       188,         76,       54,       58,
  "ingreso",                    "Tipo de ingreso per capita del pais",                     "Categórica", "-",                              "5 categorías",  0,            0,        0,        0)

# write_csv(diccionario, "output/tablas/diccionario.csv")



wdi2 <- wdi_limpio |>
  filter(anio %in% c(2001, 2008, 2022)) |> 
         arrange(pais, anio)

wdi_2001 <- wdi2 |>
  filter(anio == 2001)
wdi_2008 <- wdi2 |>
  filter(anio == 2008)
wdi_2022 <- wdi2 |>
  filter(anio == 2022)


tipos <- tibble(
  variable = names(wdi2),
  tipo_r   = sapply(wdi2, function(x) class(x)[1]))

tipos

colSums(is.na(wdi2))
colSums(is.na(wdi_2001))
colSums(is.na(wdi_2008))
colSums(is.na(wdi_2022))


# ¿Cuántos países tienen la información completa en las variables numéricas?
vars_num <- c("crecimiento_industria", "industria_pbi", "tipo_cambio_oficial", "tipo_cambio_real", 
              "indice_precios_relativos", "empleo_industrial", "exportacion_manufacturas", 
              "gasto_gobierno_pbi","valor_agregado_trabajador", "valor_agregado_trabajador",
              "ahorro_bruto_pbi", "credito_privado_pbi", "factor_ppp")

wdi2 |>
  mutate(n_na = rowSums(is.na(across(all_of(vars_num))))) |>
  count(n_na)
wdi_2001 |>
  mutate(n_na = rowSums(is.na(across(all_of(vars_num))))) |>
  count(n_na)
wdi_2008 |>
  mutate(n_na = rowSums(is.na(across(all_of(vars_num))))) |>
  count(n_na)
wdi_2022 |>
  mutate(n_na = rowSums(is.na(across(all_of(vars_num))))) |>
  count(n_na)

# ¿Los faltantes se reparten al azar o se concentran en algún grupo?
# Esta pregunta es clave: si los NA se concentran en los países de ingreso bajo,
# cualquier promedio que calculemos va a estar sesgado hacia arriba.
wdi2 |>
  group_by(ingreso, anio) |>
  summarise(
    n = n(),
    na_industria_pbi    = sum(is.na(industria_pbi)),
    .groups = "drop"
  )

wdi_analisis <- wdi2 |> filter(!is.na(industria_pbi))


nrow(wdi_analisis)   # cuántos países quedan
setdiff(wdi2$pais, wdi_analisis$pais)   # cuáles quedaron afuera
#¿deberiamos borrar los paises que tengan NA en uno de los tres periodos y quedarnos con los que tienen el dato en todos?


summary(wdi_analisis)

summary(wdi_analisis$industria_pbi)

skim(wdi_analisis)


wdi_analisis |>
  group_by(region) |>
  skim(industria_pbi, ingreso)

tabla_skim <- wdi_analisis |>
  select(all_of(vars_num)) |>
  skim() |>
  as_tibble() |>
  select(variable = skim_variable,
         n_faltantes = n_missing,
         completitud = complete_rate,
         media = numeric.mean,
         sd = numeric.sd,
         p0 = numeric.p0, p25 = numeric.p25, p50 = numeric.p50,
         p75 = numeric.p75, p100 = numeric.p100) |>
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# write_csv(tabla_skim, "output/tablas/descriptivas_wdi.csv")
view(tabla_skim)


#Grafico indusrtia
ggplot(wdi_analisis, aes(x = industria_pbi)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.8) +
  facet_wrap(~ anio, ncol = 1) + # Separa el gráfico por año
  labs(title = "Distribución global de la participación industrial",
       subtitle = "Frecuencia de países según el peso de su industria en el PBI",
       x = "Industria (% del PBI)", 
       y = "Cantidad de países") +
  theme_minimal()


#Tabla diccionario
# install.packages("gt")
library(gt)

tabla_imagen <- diccionario |> 
  gt() |> 
  tab_header(
    title = md("**Diccionario de Variables**"),
    subtitle = "Base de datos WDI - Banco Mundial"
  ) |> 
  opt_stylize(style = 1, color = "blue") |>  
  cols_align(align = "center")               

gtsave(tabla_imagen, "output/tablas/diccionario_imagen.png")




















#ANOVA


df_anova_1980 <- wdi2 %>%
  filter(anio == 1980, !is.na(industria_pbi), !is.na(region)) %>%
  select(iso3c, pais, region, industria_pbi) %>%
  mutate(region  = factor(region),
         log_pbi = log(industria_pbi))

view(df_anova_1980)

df_anova_2001 <- wdi2 %>%
  filter(anio == 2001, !is.na(industria_pbi), !is.na(region)) %>%
  select(iso3c, pais, region, industria_pbi) %>%
  mutate(region  = factor(region),
         log_pbi = log(industria_pbi))

df_anova_2023 <- wdi2 %>%
  filter(anio == 2023, !is.na(industria_pbi), !is.na(region)) %>%
  select(iso3c, pais, region, industria_pbi) %>%
  mutate(region  = factor(region),
         log_pbi = log(industria_pbi))

#Ver escala logaritmica

         ggplot(df_anova_1980, aes(x = reorder(region, industria_pbi, FUN = median), y = industria_pbi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10(labels = scales::label_dollar()) +
  coord_flip() +
  labs(title = "Participacion de industria PBI por region 1980", subtitle = "Escala logarítmica",
       x = NULL, y = "% del PBI")


         ggplot(df_anova_2001, aes(x = reorder(region, industria_pbi, FUN = median), y = industria_pbi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10(labels = scales::label_dollar()) +
  coord_flip() +
  labs(title = "Participacion de industria PBI por region 2001", subtitle = "Escala logarítmica",
       x = NULL, y = "% del PBI")

         ggplot(df_anova_2023, aes(x = reorder(region, industria_pbi, FUN = median), y = industria_pbi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10(labels = scales::label_dollar()) +
  coord_flip() +
  labs(title = "Participacion de industria PBI por region 2023", subtitle = "Escala logarítmica",
       x = NULL, y = "% del PBI")



       oneway.test(industria_pbi ~ region, data = df_anova_1980)


       oneway.test(industria_pbi ~ region, data = df_anova_2001)

  
       oneway.test(industria_pbi ~ region, data = df_anova_2023)




tukey <- TukeyHSD(df_anova_1980)


tukey <- TukeyHSD(df_anova_2001)


tukey <- TukeyHSD(df_anova_2023)



#Chi cuadrado

v_cramer <- function(prueba) {
  tabla <- prueba$observed
  sqrt(unname(prueba$statistic) / (sum(tabla) * (min(dim(tabla)) - 1)))









