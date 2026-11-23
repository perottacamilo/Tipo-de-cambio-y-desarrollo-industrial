# ==============================================================================
# 0. CONFIGURACIÓN Y LIBRERÍAS
# ==============================================================================
# ==============================================================================

library(dplyr)
library(tidyr)

# ==============================================================================
# 1. CARGA DE BASES Y FILTRO POBLACIONAL (> 1 MILLÓN DE HABITANTES)
# ==============================================================================
# wdi2: cortes principales (2001, 2008, 2022)
# wdi3: cortes principales + años adyacentes (2000-2002, 2007-2009, 2021-2023)
wdi2 <- read.csv("input/wdi2.csv")
wdi3 <- read.csv("input/wdi3.csv")

# Se excluyen microestados (< 1 millón de hab.) por su escala reducida: suelen
# especializarse en turismo o servicios financieros sin una base manufacturera
# comparable, generan valores extremos per cápita y concentran faltantes.
wdi2 <- wdi2 |>
  filter(poblacion_mill > 1)

base_2001 <- wdi2 |> filter(anio == 2001)
base_2008 <- wdi2 |> filter(anio == 2008)
base_2022 <- wdi2 |> filter(anio == 2022)

# Bases por bloques de tres años (para evaluar y realizar la interpolación)
wdi3 <- wdi3 |>
  filter(poblacion_mill > 1)

na_2001 <- wdi3 |> filter(anio %in% c(2000, 2001, 2002))
na_2008 <- wdi3 |> filter(anio %in% c(2007, 2008, 2009))
na_2022 <- wdi3 |> filter(anio %in% c(2021, 2022, 2023))

# Listado inicial de variables numéricas a auditar
variables <- c(
  "crecimiento_industria",
  "industria_per_capita",
  "tipo_cambio_real_efectivo",
  "indice_nivel_precios",
  "empleo_industrial",
  "exportacion_manufactura_pc",
  "gasto_gobierno_pbi",
  "gastos_totales_gob_pbi",
  "valor_agregado_trabajador",
  "ahorro_bruto_pbi",
  "credito_privado_pbi",
  "recursos_naturales",
  "pbi_per_capita",
  "pbi_millones"
)


# ==============================================================================
# 2. DIAGNÓSTICO DE FALTANTES POR VARIABLE (TOTAL Y POR AÑO)
# ==============================================================================

# Porcentaje de NA por variable en el total de los tres cortes
faltantes <- wdi2 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))

# Porcentaje de NA por variable desagregado por año
faltantes_2001 <- base_2001 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))

faltantes_2008 <- base_2008 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))

faltantes_2022 <- base_2022 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))

# Conclusión: dos variables superan el 40% de faltantes en los tres cortes:
# - tipo_cambio_real_efectivo (51.2%): difícil de interpolar sin distorsión y
#   no es esencial para el análisis principal -> se elimina.
# - gastos_totales_gob_pbi (41.9%): puede ser reemplazada por gasto_gobierno_pbi,
#   que presenta muchos menos faltantes -> se elimina.


# ==============================================================================
# 3. PAÍSES SIN DATOS EN LAS VARIABLES CENTRALES (INDUSTRIA Y PRECIOS)
# ==============================================================================
wdi_analisis        <- wdi2 |> filter(!is.na(industria_per_capita))
wdi_analisis2       <- wdi2 |> filter(!is.na(indice_nivel_precios))
wdi_analisis_3_ipbi <- wdi3 |> filter(!is.na(industria_per_capita))
wdi_analisis_3_inp  <- wdi3 |> filter(!is.na(indice_nivel_precios))

# Países sin industria_per_capita en ninguno de los 3 cortes
setdiff(wdi2$pais, wdi_analisis$pais)

# Países sin indice_nivel_precios en ninguno de los 3 cortes
setdiff(wdi2$pais, wdi_analisis2$pais)

# Países sin industria_per_capita en ninguno de los 9 años (imposible interpolar)
setdiff(wdi3$pais, wdi_analisis_3_ipbi$pais)

# Países sin indice_nivel_precios en ninguno de los 9 años (imposible interpolar)
setdiff(wdi3$pais, wdi_analisis_3_inp$pais)

# Conclusión: Corea del Norte no tiene datos de industria_per_capita ni de
# indice_nivel_precios en los 9 años, y Cuba no tiene datos de indice_nivel_precios.
# Como no pueden interpolarse, ambos países se excluyen de la base.


# ==============================================================================
# 4. PRIMERA DEPURACIÓN (2 VARIABLES Y 2 PAÍSES) Y BASES INTERMEDIAS (.1)
# ==============================================================================
wdi2.1 <- wdi2 |>
  select(-tipo_cambio_real_efectivo, -gastos_totales_gob_pbi) |>
  filter(!pais %in% c("Korea, Dem. People's Rep.", "Cuba"))

wdi3.1 <- wdi3 |>
  select(-tipo_cambio_real_efectivo, -gastos_totales_gob_pbi) |>
  filter(!pais %in% c("Korea, Dem. People's Rep.", "Cuba"))

base_2001.1 <- wdi2.1 |> filter(anio == 2001)
base_2008.1 <- wdi2.1 |> filter(anio == 2008)
base_2022.1 <- wdi2.1 |> filter(anio == 2022)

na_2001.1 <- wdi3.1 |> filter(anio %in% c(2000, 2001, 2002))
na_2008.1 <- wdi3.1 |> filter(anio %in% c(2007, 2008, 2009))
na_2022.1 <- wdi3.1 |> filter(anio %in% c(2021, 2022, 2023))

# Listado de las 12 variables numéricas restantes
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
  "pbi_millones"
)


# ==============================================================================
# 5. DIAGNÓSTICO DE FALTANTES POR PAÍS Y AÑO (>= 50% DE VARIABLES FALTANTES)
# ==============================================================================
faltantes_pais_2001 <- base_2001.1 |>
  group_by(pais) |>
  summarise(
    ingreso       = first(ingreso2),
    observaciones = n() * length(variables2),
    faltantes     = sum(across(all_of(variables2), ~ sum(is.na(.x)))),
    pct_faltantes = 100 * faltantes / observaciones,
    .groups       = "drop"
  ) |>
  arrange(desc(pct_faltantes))

print(faltantes_pais_2001, n = Inf)

faltantes_pais_2008 <- base_2008.1 |>
  group_by(pais) |>
  summarise(
    ingreso       = first(ingreso2),
    observaciones = n() * length(variables2),
    faltantes     = sum(across(all_of(variables2), ~ sum(is.na(.x)))),
    pct_faltantes = 100 * faltantes / observaciones,
    .groups       = "drop"
  ) |>
  arrange(desc(pct_faltantes))

print(faltantes_pais_2008, n = Inf)

faltantes_pais_2022 <- base_2022.1 |>
  group_by(pais) |>
  summarise(
    ingreso       = first(ingreso2),
    observaciones = n() * length(variables2),
    faltantes     = sum(across(all_of(variables2), ~ sum(is.na(.x)))),
    pct_faltantes = 100 * faltantes / observaciones,
    .groups       = "drop"
  ) |>
  arrange(desc(pct_faltantes))

print(faltantes_pais_2022, n = Inf)

head(faltantes_pais_2001)
head(faltantes_pais_2008)
head(faltantes_pais_2022)

# Conclusión: se excluyen países con 50% o más de variables faltantes en algún corte
# (interpolar la mitad de un país generaría observaciones artificiales):
# - 2001: Kosovo, Sudán del Sur, Afganistán, Somalia y Angola.
# - 2008: Somalia.
# - 2022: Eritrea, Yemen, Sudán del Sur y Venezuela.
# Nota: Puerto Rico se conserva porque en 2022 falta recursos_naturales en toda la
# base WDI, por lo que su porcentaje real de faltantes es menor al 50%.


# ==============================================================================
# 6. SEGUNDA DEPURACIÓN (PAÍSES CON >= 50% DE FALTANTES) -> BASES (.2)
# ==============================================================================
wdi2.2 <- wdi2.1 |>
  filter(!pais %in% c(
    "Kosovo", "South Sudan", "Afghanistan", "Angola",
    "Yemen", "Eritrea", "Venezuela, RB", "Somalia, Fed. Rep."
  ))

wdi3.2 <- wdi3.1 |>
  filter(!pais %in% c(
    "Kosovo", "South Sudan", "Afghanistan", "Angola",
    "Yemen", "Eritrea", "Venezuela, RB", "Somalia, Fed. Rep."
  ))


# ==============================================================================
# 7. INTERPOLACIÓN CON AÑOS ADYACENTES -> wdi_filtrado.csv
# ==============================================================================
# Promedio de años adyacentes (±1); si falta uno, usa el otro; si faltan ambos, NA.
# Excepción: para recursos_naturales en 2022 (sin datos en WDI), se usa 2021.
source("utils/imputar_vecinos.R")

wdi_filtrado <- wdi3.2 |>
  group_by(pais) |>
  arrange(pais, anio) |>
  mutate(
    across(
      .cols = c(
        crecimiento_industria,
        industria_per_capita,
        indice_nivel_precios,
        empleo_industrial,
        exportacion_manufactura_pc,
        gasto_gobierno_pbi,
        valor_agregado_trabajador,
        ahorro_bruto_pbi,
        credito_privado_pbi,
        pbi_per_capita,
        pbi_millones,
        industria_pc
      ),
      .fns = ~ if_else(
        condition = is.na(.x) & anio %in% c(2001, 2008, 2022),
        true      = imputar_vecinos(.x, anio),
        false     = .x
      )
    ),
    recursos_naturales = case_when(
      anio == 2022 ~ if_else(lag(anio) == anio - 1, lag(recursos_naturales), NA_real_),
      is.na(recursos_naturales) & anio %in% c(2001, 2008) ~ imputar_vecinos(recursos_naturales, anio),
      TRUE ~ recursos_naturales
    )
  ) |>
  ungroup() |>
  filter(anio %in% c(2001, 2008, 2022))

# Base interpolada (conserva NAs remanentes; útil para regresiones con mayor N)
write.csv(wdi_filtrado, "input/wdi_filtrado.csv")


# ==============================================================================
# 8. BASE COMPLETA SIN NAs -> wdi_final.csv (PARA PCA Y CLUSTERING)
# ==============================================================================
wdi_final <- wdi_filtrado |>
  drop_na(
    crecimiento_industria,
    industria_per_capita,
    indice_nivel_precios,
    empleo_industrial,
    exportacion_manufactura_pc,
    gasto_gobierno_pbi,
    valor_agregado_trabajador,
    ahorro_bruto_pbi,
    credito_privado_pbi,
    recursos_naturales,
    pbi_per_capita,
    pbi_millones
  )

write.csv(wdi_final, "input/wdi_final.csv")

# Reporte de observaciones descartadas al exigir casos completos sin NAs
reporte_eliminados <- anti_join(wdi_filtrado, wdi_final, by = c("pais", "anio")) |>
  select(
    pais, anio,
    crecimiento_industria,
    industria_per_capita,
    indice_nivel_precios,
    empleo_industrial,
    exportacion_manufactura_pc,
    gasto_gobierno_pbi,
    valor_agregado_trabajador,
    ahorro_bruto_pbi,
    credito_privado_pbi,
    recursos_naturales,
    pbi_per_capita,
    pbi_millones,
    region,
    ingreso
  ) |>
  arrange(pais, anio)

print(reporte_eliminados, n = Inf)

# Control final de NAs en ambas bases exportadas
colSums(is.na(wdi_filtrado))
colSums(is.na(wdi_final))