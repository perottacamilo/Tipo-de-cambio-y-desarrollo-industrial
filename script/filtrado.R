# install.packages(c("tidyverse", "WDI", "skimr"))

library(tidyverse)
library(WDI)
library(skimr)
library(readxl)

rm(list = ls())

getwd()

wdi <- read_csv("~/GitHub/Trabajo-Practico-Ciencia-de-Datos/raw/wdi_tabla_completa.csv")

view(wdi)

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
  "industria_pbi",          "Participacion de la industria en el PBI",                     "Numérica",   "% del PBI",  "0 a 100",
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
  "credito_privado_bancos_pbi",      "Credito al sector privado de los bancos como % PBI",                                     "Numérica",   "% del PBI",       "0 a 100",
  "ingreso",      "Tipo de ingreso per capita del pais",                                     "Categórica",       "-", "5 categorías",
  "fator_ppp",      "Factor de conversion de Paridad de Poder Adquisitivo",                                     "Numérica",       "Conversión en dólares", "> 0")



wdi2 <- wdi_limpio |>
  filter(anio %in% c(1980, 2004, 2025)) |> 
         arrange(pais, anio)
wdi_1980 <- wdi2 |>
  filter(anio == 1980)
wdi_2004 <- wdi2 |>
  filter(anio == 2004)
wdi_2025 <- wdi2 |>
  filter(anio == 2025)


tipos <- tibble(
  variable = names(wdi2),
  tipo_r   = sapply(wdi2, function(x) class(x)[1])
)
tipos

colSums(is.na(wdi2))
colSums(is.na(wdi_1980))
colSums(is.na(wdi_2004))
colSums(is.na(wdi_2025))

# ¿Cuántos países tienen la información completa en las variables numéricas?
vars_num <- c("crecimiento_industria", "industria_pbi", "tipo_cambio_oficial", "tipo_cambio_real",
              "empleo_industrial", "exportacion_manufacturas", "gasto_gobierno", "gasto_gobierno_pbi","valor_agregado_trabajador",
               "valor_agregado_trabajador", "valor_agregado_trabajador", "ahorro_bruto_pbi", "ahorro_interno_bruto", 
               "gastos_totales_gob_pbi", "credito_privado_pbi", "credito_privado_bancos_pbi", "factor_ppp")

wdi2 |>
  mutate(n_na = rowSums(is.na(across(all_of(vars_num))))) |>
  count(n_na)
wdi_1980 |>
  mutate(n_na = rowSums(is.na(across(all_of(vars_num))))) |>
  count(n_na)
wdi_2004 |>
  mutate(n_na = rowSums(is.na(across(all_of(vars_num))))) |>
  count(n_na)
wdi_2025 |>
  mutate(n_na = rowSums(is.na(across(all_of(vars_num))))) |>
  count(n_na)
## 2025 parece tener mas na que 2004, deberiamos probar con 2024 o 2023, capaz que hay mas datos




# ¿Los faltantes se reparten al azar o se concentran en algún grupo?
# Esta pregunta es clave: si los NA se concentran en los países de ingreso bajo,
# cualquier promedio que calculemos va a estar sesgado hacia arriba.
wdi2 |>
  group_by(ingreso, anio) |>
  summarise(
    n = n(),
    na_industria_pbi    = sum(is.na(industria_pbi)),
    ##na_banda_ancha = sum(is.na(banda_ancha)),
   ## na_secundaria  = sum(is.na(secundaria)),
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


#ANOVA


df_anova_1980 <- wdi2 %>%
  filter(anio == 1980, !is.na(industria_pbi), !is.na(region)) %>%
  select(iso3c, pais, region, industria_pbi) %>%
  mutate(region  = factor(region),
         log_pbi = log(industria_pbi))

view(df_anova_1980)

df_anova_2004 <- wdi2 %>%
  filter(anio == 2004, !is.na(industria_pbi), !is.na(region)) %>%
  select(iso3c, pais, region, industria_pbi) %>%
  mutate(region  = factor(region),
         log_pbi = log(industria_pbi))

df_anova_2025 <- wdi2 %>%
  filter(anio == 2025, !is.na(industria_pbi), !is.na(region)) %>%
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


         ggplot(df_anova_2004, aes(x = reorder(region, industria_pbi, FUN = median), y = industria_pbi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10(labels = scales::label_dollar()) +
  coord_flip() +
  labs(title = "Participacion de industria PBI por region 2004", subtitle = "Escala logarítmica",
       x = NULL, y = "% del PBI")

         ggplot(df_anova_2025, aes(x = reorder(region, industria_pbi, FUN = median), y = industria_pbi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  scale_y_log10(labels = scales::label_dollar()) +
  coord_flip() +
  labs(title = "Participacion de industria PBI por region 2025", subtitle = "Escala logarítmica",
       x = NULL, y = "% del PBI")



       oneway.test(industria_pbi ~ region, data = df_anova_1980)


       oneway.test(industria_pbi ~ region, data = df_anova_2004)

  
       oneway.test(industria_pbi ~ region, data = df_anova_2025)




tukey <- TukeyHSD(df_anova_1980)


tukey <- TukeyHSD(df_anova_2004)


tukey <- TukeyHSD(df_anova_2025)



#Chi cuadrado

v_cramer <- function(prueba) {
  tabla <- prueba$observed
  sqrt(unname(prueba$statistic) / (sum(tabla) * (min(dim(tabla)) - 1)))









