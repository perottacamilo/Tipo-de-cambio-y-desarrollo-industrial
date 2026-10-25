#install.packages(c("tidyverse", "readxl", "WDI", "skimr", "dplyr", "tidyr", "ggplot2", "gt"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(WDI)
library(skimr)
library(readxl)
library(gt) 

rm(list = ls())

options(scipen = 999)

wdi <- read_csv("~/GitHub/Trabajo-Practico-Ciencia-de-Datos/raw/wdi_tabla_completa.csv")

wdi <- read_csv("raw/wdi_tabla_completa.csv")

wdi_limpio <- wdi |>
  filter(region != "Aggregates", !is.na(region), region != "") |>
  rename(pais = country, anio = year, ingreso = income, pbi_millones = pbi) |>
  select(-iso2c, -status, -lastupdated, -status, -lending, -latitude, -longitude, -capital) |>
  mutate(pbi_millones = pbi_millones/1000000,
         poblacion_mill = poblacion/1000000,
         ingreso = factor(ingreso,
                          levels = c("Low income", "Lower middle income", "Upper middle income",
                                     "High income", "Not classified"),
                          labels = c("Ingreso bajo", "Ingreso medio-bajo", "Ingreso medio-alto",
                                     "Ingreso alto", "Sin clasificar")),
         mutate(across(where(is.numeric), ~ round(.x, 3))),
         ingreso2 = ingreso,
         ingreso2 = factor(ingreso2,
                           levels = c("Ingreso bajo", "Ingreso medio-bajo", "Ingreso medio-alto",
                                      "Ingreso alto", "Sin clasificar"),
                           labels = c("Bajo", "Medio-bajo", "Medio-alto",
                                      "Alto", "Sin clasificar")),)
write.csv(wdi_limpio, "output/data/wdi_limpio.csv")


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
  "industria_pbi",              "Participacion de la industria en el PBI",                                                                "Numérica",            "% del PBI",                      "0 a 100",        5267,              36,       21,       19,
  "crecimiento_industria",      "Crecimiento de la industria",                                                                            "Numérica",            "% de crecimiento",               "> 0",            6232,              48,       35,       24,
  "tipo_cambio_real_efectivo",  "Nivel de Competitividad con base = 2010",                                                                "Numérica",            "Indice",                         "> 0",            10172,             122,      122,      125,
  "recursos_naturales",         "Rentas Económicas provenientes de los recursos naturales ",                                              "Numérica",            "% del PBI",                      "0 a 100",        5087,              17,       10,       217,
  "empleo_industrial",          "Cantidad de empleados en el sector industrial",                                                          "Numérica",            "% del total de empleados",       "0 a 100",        7791,              30,       30,       31,
  "ingreso",                    "Tipo de ingreso per capita del pais",                                                                    "Categórica",          "-",                              "5 categorías",   0,                 0,        0,        0,
  "exportacion_manufactura",    "Porcentaje de la exportacion en manufacturas",                                                           "Numérica",            "% del total",                    "0 a 100",        6383,              53,       50,       54,
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
  
write.csv(wdi2, "output/data/wdi2.csv")

  wdi3 <- wdi_limpio |>
    filter(anio %in% c(2000, 2001, 2002, 2007, 2008, 2009, 2021, 2022, 2023), poblacion_mill >= 1) |> 
    arrange(pais, anio)

  write.csv(wdi3, "output/data/wdi3.csv")
  
  
  
  


#Creamos una base para cada anio
wdi_2001 <- wdi2 |>
  filter(anio == 2001)
wdi_2008 <- wdi2 |>
  filter(anio == 2008)
wdi_2022<- wdi2 |>
  filter(anio == 2022)

#Hasta aca haria yo el primer Script
#Luego pasaria a "Script Datos Faltantes"






#Cantidad de observaciones faltanes (NA) por variable de cada base
colSums(is.na(wdi2))
colSums(is.na(wdi_2001))
colSums(is.na(wdi_2008))
colSums(is.na(wdi_2022))


# ¿Cuántos países tienen la información completa en las variables numéricas?
#Vector de todas las variables
vars_num <- c("crecimiento_industria", "industria_pbi", "tipo_cambio_real_efectivo", 
              "indice_nivel_precios", "empleo_industrial", "exportacion_manufactura", 
              "gasto_gobierno_pbi", "gastos_totales_gob_pbi", "valor_agregado_trabajador", "valor_agregado_trabajador",
              "ahorro_bruto_pbi", "credito_privado_pbi", "recursos_naturales", "pbi_per_capita",
              "pbi_millones", "poblacion_mill")



#Mide cantidad de paises (n) y cantidad de variables con dato faltante (n_na)
#Por ejemplo n_na=2 y n=129, hay 129 paises que les falta el valor de dos variables

wdi2 |>
  mutate(n_na = rowSums(is.na(across(all_of(vars_num))))) |>
  count(n_na)

wdi_limpio |> 
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

#Ver la cantidad de datos faltantes en una variable especifica, y que porcentaje representa del total de datos
#Separado por los tres anios elegidos, y por tipo de ingreso
wdi2 |>
  group_by(ingreso, anio) |>
  summarise(
    n = n(),
    na_industria_pbi  = sum(is.na(industria_pbi)),
    pct_na_ind = na_industria_pbi/n,
    na_price_level = sum(is.na(indice_nivel_precios)),
    pct_na_price_level = na_price_level/n,
    .groups = "drop")

wdi_2001 |>
  group_by(ingreso, anio) |>
  summarise(
    n = n(),
    na_industria_pbi  = sum(is.na(industria_pbi)),
    pct_na_ind = na_industria_pbi/n,
    na_price_level = sum(is.na(indice_nivel_precios)),
    pct_na_price_level = na_price_level/n,
    .groups = "drop")


wdi_2008 |>
  group_by(ingreso, anio) |>
  summarise(
    n = n(),
    na_industria_pbi  = sum(is.na(industria_pbi)),
    pct_na_ind = na_industria_pbi/n,
    na_price_level = sum(is.na(indice_nivel_precios)),
    pct_na_price_level = na_price_level/n,
    .groups = "drop")

wdi_2022 |>
  group_by(ingreso, anio) |>
  summarise(
    n = n(),
    na_industria_pbi  = sum(is.na(industria_pbi)),
    pct_na_ind = na_industria_pbi/n,
    na_price_level = sum(is.na(indice_nivel_precios)),
    pct_na_price_level = na_price_level/n,
    .groups = "drop")


#Que porcentaje de NA tiene cada varible (en los tres anios elegidos)

faltantes <- wdi2 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))
view(faltantes)



#Proceentaje de datos faltantes por variable por cada anio
faltantes_2001 <- wdi_2001 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))
view(faltantes_2001)

faltantes_2008 <- wdi_2008 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))
view(faltantes_2008)

faltantes_2022 <- wdi_2022 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))
view(faltantes_2022)



wdi_analisis <- wdi2 |> filter(!is.na(industria_pbi))
wdi_analisis2 <- wdi2 |> filter(!is.na(indice_nivel_precios))

# Cuántos países quedan si eliminamos aquellos aquellos que no tienen industria_pbi
nrow(wdi_analisis)

#Cuales son aquellos paises que se no tienen industria_pbi en ninguno de los tres anios
setdiff(wdi2$pais, wdi_analisis$pais)

#¿deberiamos borrar los paises que tengan NA en uno de los tres periodos y quedarnos con los que tienen el dato en todos?

#Lo mismo para la variable indice_nivel_precios

# Cuántos países quedan si eliminamos aquellos aquellos que no tienen indice_nivel_precios
nrow(wdi_analisis2) 

#Cuales son aquellos paises que se no tienen indice_nivel_precios en ninguno de los tres anios
setdiff(wdi2$pais, wdi_analisis2$pais)   


wdi_analisis_2001 <- wdi_2001 |> filter(!is.na(industria_pbi))
wdi_analisis2001 <- wdi_2001 |> filter(!is.na(indice_nivel_precios))

nrow(wdi_2001)   # cuántos países quedan
setdiff(wdi_2001$pais, wdi_analisis_2001$pais)   # cuáles quedaron afuera


wdi_analisis_2008 <- wdi_2008 |> filter(!is.na(industria_pbi))
wdi_analisis2008 <- wdi_2008 |> filter(!is.na(indice_nivel_precios))

nrow(wdi_2008)   # cuántos países quedan
setdiff(wdi_2008$pais, wdi_analisis_2008$pais)   # cuáles quedaron afuera


wdi_analisis_2022 <- wdi_2022 |> filter(!is.na(industria_pbi))
wdi_analisis2022 <- wdi_2022 |> filter(!is.na(indice_nivel_precios))

nrow(wdi_2022)   # cuántos países quedan
setdiff(wdi_2022$pais, wdi_analisis_2022$pais)   # cuáles quedaron afuera



#Visualizacion de la distribucion de cada variable
skim(wdi_2001)
skim(wdi_2008)
skim(wdi_2022)
skim(wdi2)


summary(wdi_analisis)

summary(wdi_analisis$industria_pbi)

skim(wdi_analisis)

#Tabla que muestra la cantidad de paises que pertenecen a cada tipo de nivel de ingreso por regiones
tabla_cruzada <- table(wdi_analisis$region, wdi_analisis$ingreso)
tabla_cruzada

# Con totales de fila y columna
addmargins(tabla_cruzada)

#Ver como se comporta una variable (industria_pbi) por region e ingreso
wdi_analisis |>
  group_by(region) |>
  skim(industria_pbi, ingreso)

#Tabla de la distribucion de todas las variables
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
print(tabla_skim)

#Grafico industria
grafico1 <- ggplot(wdi_analisis, aes(x = industria_pbi)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.8) +
  facet_wrap(~ anio, ncol = 1) + # Separa el gráfico por año
  labs(title = "Distribución global de la participación industrial",
       subtitle = "Frecuencia de países según el peso de su industria en el PBI",
       x = "Industria (% del PBI)", 
       y = "Cantidad de países") +
  theme_minimal()
print(grafico1)

#Guardar grafico
ggsave(file.path("output/graficos", "histograma_industria.png"), plot = grafico1)


#Tabla de variables faltantes por ingreso (igual que linea 146)
na_por_ingreso <- wdi2 |>
  group_by(ingreso2, anio) |>
  summarise(
    n = n(),
    na_industria_pbi = sum(is.na(industria_pbi)),
    pct_na_ind = na_industria_pbi / n,
    na_price_level = sum(is.na(indice_nivel_precios)),
    pct_na_price_level = na_price_level / n,
    .groups = "drop")

# Pasar a formato largo: una fila por ingreso-año-variable
#Creo la variable ingreso2, que contiene menos palabras para que el grafico quede bien
#La forma anterior esta al final del script pero creo que queda muy cargado de "ingreso" 
na_largo <- na_por_ingreso |>
  select(ingreso2, anio, pct_na_ind, pct_na_price_level) |>
  pivot_longer(
    cols = c(pct_na_ind, pct_na_price_level),
    names_to = "variable",
    values_to = "pct_na") |>
  mutate(variable = recode(variable,
                           pct_na_ind = "industria_pbi",
                           pct_na_price_level = "indice_nivel_precios"), 
         anio = factor(anio))

#Grafico datos faltantes por ingreso
grafico2 <- ggplot(na_largo, aes(x = ingreso2, y = pct_na, fill = anio)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(~variable) +
  scale_x_discrete(limits = c("Bajo", "Medio-bajo", "Medio-alto", "Alto")) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 0.4),
    breaks = seq(0, 1, 0.1)
  ) +
  labs(
    title = "Porcentaje de valores faltantes por nivel de ingreso",
    subtitle = "Comparación 2001 - 2008 - 2022",
    x = "Nivel de ingreso",
    y = "% de países sin dato",
    fill = "Año"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

print(grafico2)

ggsave(file.path("output/graficos", "datos_faltanes_ingreso.png"), 
       plot = grafico2, width = 12, height = 7, units = "in", dpi = 300)


#Graficos plots industria_pbi y nivel de precios para 2001
#Plot 
#Correr de 318 a 331 para guardar en la carpeta graficos)
png("output/graficos/plot_2001.png", width = 8, height = 6, units = "in", res = 300)

grafico3 <- plot(
  wdi_2001$indice_nivel_precios, wdi_2001$industria_pbi,
  pch = 19, col = rgb(0.2, 0.4, 0.7, 0.6),
  main = "Relacion entre industria y el nivel de precios (2001)",
  ylab = "% industria en el PBI",
  xlab = "Indice de nivel de precios"
)
grafico3 <- abline(lm(industria_pbi ~ indice_nivel_precios, data = wdi_2001), col = "darkred", lwd = 2)
r <- cor(wdi_2001$industria_pbi, wdi_2001$indice_nivel_precios, use = "complete.obs")
grafico3 <- legend("topleft", legend = paste("r =", round(r, 2)), bty = "n")

dev.off()


#Graficos plots industria_pbi y nivel de precios para 2008

png("output/graficos/plot_2008.png", width = 8, height = 6, units = "in", res = 300)

grafico3.1 <- plot(
  wdi_2008$indice_nivel_precios, wdi_2008$industria_pbi,
  pch = 19, col = rgb(0.2, 0.4, 0.7, 0.6),
  main = "Relacion entre industria y el nivel de precios (2008)",
  ylab = "% industria en el PBI",
  xlab = "Indice de nivel de precios"
)
grafico3.1 <- abline(lm(industria_pbi ~ indice_nivel_precios, data = wdi_2008), col = "darkred", lwd = 2)
r.1 <- cor(wdi_2008$industria_pbi, wdi_2008$indice_nivel_precios, use = "complete.obs")
grafico3.1 <- legend("topleft", legend = paste("r.1 =", round(r.1, 2)), bty = "n")

dev.off()

#Plots industria_pbi y nivel de precios para 2008

png("output/graficos/plot_2022.png", width = 8, height = 6, units = "in", res = 300)

grafico3.2 <- plot(
  wdi_2022$indice_nivel_precios, wdi_2022$industria_pbi,
  pch = 19, col = rgb(0.2, 0.4, 0.7, 0.6),
  main = "Relacion entre industria y el nivel de precios (2022)",
  ylab = "% industria en el PBI",
  xlab = "Indice de nivel de precios"
)
grafico3.2 <- abline(lm(industria_pbi ~ indice_nivel_precios, data = wdi_2022), col = "darkred", lwd = 2)
r.2 <- cor(wdi_2022$industria_pbi, wdi_2022$indice_nivel_precios, use = "complete.obs")
grafico3.2 <- legend("topleft", legend = paste("r.2 =", round(r.2, 2)), bty = "n")

dev.off()

#Boxplot ingreso-industria

hist(
  wdi_2001$industria_pbi,
  breaks = 20, freq = FALSE,
  main = "Distribución del uso de internet",
  xlab = "% de la población que usa internet", ylab = "Densidad",
  col = "grey90", border = "white")

lines(density(wdi_analisis$industria_pbi, na.rm = TRUE), lwd = 2, col = "darkred")
rug(wdi_2001$industria_pbi)   # marca cada observación sobre el eje
abline(v = mean(wdi_2001$industria_pbi), col = "blue", lwd = 2, lty = 2)
abline(v = median(wdi_2001$industria_pbi), col = "darkgreen", lwd = 2, lty = 3)
legend("topleft",
       legend = c("Densidad", "Media", "Mediana"),
       col = c("darkred", "blue", "darkgreen"),
       lwd = 2, lty = c(1, 2, 3), bty = "n")





wdi_mediana <- wdi_limpio |> 
  filter(anio %in% 2001:2022)

wdi_latam <- wdi_mediana |> 
  filter(region == "Latin America & Caribbean") |> 
  select(anio, region, indice_nivel_precios)


wdi_asia <- wdi_mediana |> 
  filter(region == "East Asia & Pacific") |> 
  select(anio, region, indice_nivel_precios)

wdi_china <- wdi_mediana |>
  filter(pais == "China",
         anio %in% 2001:2022) |>
  mutate(region = "China") 

wdi_comparativo <- bind_rows(wdi_latam, wdi_asia, wdi_china)

medianas_evolucion <- wdi_comparativo |>
  group_by(anio, region) |>
  summarize(
    mediana_precios = median(indice_nivel_precios, na.rm = TRUE), 
    .groups = "drop"
  )

ggplot(medianas_evolucion, aes(x = anio, y = mediana_precios, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("America Latina y Caribe" = "#D9534F", 
                                "Este de Asia y Pacifico" = "#5BC0DE",
                                "China" = "#27F52A")) +
  scale_x_continuous(limits = c(2001, 2022), breaks = seq(2001, 2022, by = 3)) +
  labs(
    title = "Evolución del Índice de Nivel de Precios (Medianas)",
    subtitle = "América Latina vs. Este de Asia y el Pacífico",
    x = "Año",
    y = "Índice de Nivel de Precios (Mediana)",
    color = "Región"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )


cor(wdi_2001$industria_pbi, wdi_2001$indice_nivel_precios,
    use = "complete.obs")

# cor.test() agrega el test de significatividad y el intervalo de confianza
cor.test(wdi_2001$industria_pbi, wdi_2001$indice_nivel_precios)
#La correlacion es significativa al 1%



alta  <- wdi_2001$industria_pbi[wdi_2001$region == "East Asia & Pacific"]
baja  <- wdi_2001$industria_pbi[wdi_2001$region %in%
                                  c("Latin America & Caribbean", "")]

hist(baja, breaks = 15, col = rgb(1, 0, 0, 0.5), border = "white",
     xlim = c(0, 100), ylim = c(0, 12),
     main = "2001",
     xlab = "% industria en el PBI", ylab = "Cantidad de países")
hist(alta, breaks = 15, col = rgb(0, 0, 1, 0.5), border = "white", add = TRUE)



alta1  <- wdi_2022$industria_pbi[wdi_2022$region == "East Asia & Pacific"]
baja1  <- wdi_2022$industria_pbi[wdi_2022$region %in%
                                   c("Latin America & Caribbean", "")]

hist(baja1, breaks = 15, col = rgb(1, 0, 0, 0.5), border = "white",
     xlim = c(0, 100), ylim = c(0, 12),
     main = "2022",
     xlab = "% industria en el PBI", ylab = "Cantidad de países")
hist(alta1, breaks = 15, col = rgb(0, 0, 1, 0.5), border = "white", add = TRUE)



alta2  <- wdi_2008$industria_pbi[wdi_2008$region == "East Asia & Pacific"]
baja2  <- wdi_2008$industria_pbi[wdi_2008$region %in%
                                   c("Latin America & Caribbean", "")]

hist(baja2, breaks = 15, col = rgb(1, 0, 0, 0.5), border = "white",
     xlim = c(0, 100), ylim = c(0, 12),
     main = "2008",
     xlab = "% industria en el PBI", ylab = "Cantidad de países")
hist(alta2, breaks = 15, col = rgb(0, 0, 1, 0.5), border = "white", add = TRUE)



# Todo lo que hicimos acá es, punto por punto, lo que pide la instancia 1:
#
#  [ ] Dimensiones de la base            -> dim(), nrow(), ncol()          (2.1)
#  [ ] Unidad de observación             -> duplicated(), n_distinct()     (2.1)
#  [ ] Descripción de cada variable      -> diccionario                    (1.3)
#  [ ] Tipo de cada variable             -> glimpse(), tipos               (2.2)
#  [ ] Valores faltantes                 -> colSums(is.na()), faltantes    (2.3)
#  [ ] Estadísticas descriptivas         -> summary(), skim(), descriptivas (3)
#  [ ] Distribución de las categóricas   -> table(), prop.table()          (4)
#  [ ] Forma de las distribuciones       -> hist()                         (5)
#  [ ] Dispersión y atípicos             -> boxplot(), boxplot.stats()     (6)
#  [ ] Medidas de posición y desigualdad -> quantile(), ratios             (7)
#  [ ] Relación entre variables          -> cor(), pairs()                 (8)
#  [ ] Periodicidad de la fuente         -> anual (WDI se actualiza ~2 veces al año)
#
# Tres advertencias finales, que son las que más se ven en las entregas:
#
# 1. Ninguna tabla ni gráfico se presenta sin una frase que lo interprete.
#    El output de R es el insumo, no el resultado.
# 2. Los NA y los outliers se explican, no se borran en silencio. Decir cuántos
#    son, dónde se concentran y qué se hizo con ellos.
# 3. Correlación no es causalidad, y el TP no es causal. Las relaciones que
#    encontramos acá describen asociaciones entre países en un año, nada más.








  
  #Tabla de variables faltantes por ingreso (igual que linea 146)
  na_por_ingreso2 <- wdi2 |>
    group_by(ingreso, anio) |>
    summarise(
      n = n(),
      na_industria_pbi = sum(is.na(industria_pbi)),
      pct_na_ind = na_industria_pbi / n,
      na_price_level = sum(is.na(indice_nivel_precios)),
      pct_na_price_level = na_price_level / n,
      .groups = "drop")
  
  # Pasar a formato largo: una fila por ingreso-año-variable
  #Creo la variable ingreso2, que contiene menos palabras para que el grafico quede bien
  na_largo2 <- na_por_ingreso2 |>
    select(ingreso, anio, pct_na_ind, pct_na_price_level) |>
    pivot_longer(
      cols = c(pct_na_ind, pct_na_price_level),
      names_to = "variable",
      values_to = "pct_na") |>
    mutate(variable = recode(variable,
                             pct_na_ind = "industria_pbi",
                             pct_na_price_level = "indice_nivel_precios"), 
           anio = factor(anio))
  
  #Grafico datos faltantes por ingreso
  grafico22 <- ggplot(na_largo2, aes(x = ingreso, y = pct_na, fill = anio)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    facet_wrap(~variable) +
    scale_y_continuous(
      labels = scales::percent,
      limits = c(0, 0.4),
      breaks = seq(0, 1, 0.1)
    ) +
    labs(
      title = "Porcentaje de valores faltantes por nivel de ingreso",
      subtitle = "Comparación 2001 - 2008 - 2022",
      x = "Nivel de ingreso",
      y = "% de países sin dato",
      fill = "Año"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "top")
  
  print(grafico22)
  
  ggsave(file.path("output/graficos", "datos_faltanes_ingreso2.png"), 
         plot = grafico22, width = 12, height = 7, units = "in", dpi = 300)
  
  
  
  
  
  

  
  
  
  
  
  
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # 1. Variables a analizar
  variables <- c(
    "crecimiento_industria",
    "industria_pbi",
    "tipo_cambio_real_efectivo",
    "indice_nivel_precios",
    "empleo_industrial",
    "exportacion_manufactura",
    "gasto_gobierno_pbi",
    "gastos_totales_gob_pbi",
    "valor_agregado_trabajador",
    "ahorro_bruto_pbi",
    "credito_privado_pbi",
    "recursos_naturales",
    "pbi_per_capita",
    "pbi_millones",
    "ingreso2",
    "poblacion_mill"
  )
  
  anios <- c(2001, 2008, 2022)
  
  # 2. Seleccionar años y variables disponibles
  base <- wdi2 %>%
    mutate(anio = as.integer(as.character(anio))) %>%
    filter(anio %in% anios)
  
  variables <- intersect(variables, names(base))
  
  if (nrow(base) == 0 || length(variables) == 0) {
    stop("No hay observaciones o variables disponibles.")
  }
  
  # 3. Calcular faltantes por variable y año
  faltantes <- base %>%
    group_by(anio) %>%
    summarise(
      across(all_of(variables), ~sum(is.na(.x))),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = -anio,
      names_to = "variable",
      values_to = "n_faltantes"
    ) %>%
    complete(
      variable = variables,
      anio = anios,
      fill = list(n_faltantes = 0)
    ) %>%
    mutate(
      porcentaje = 100 * n_faltantes / nrow(base)
    )
  
  # 4. Porcentaje total de faltantes por variable
  totales <- faltantes %>%
    group_by(variable) %>%
    summarise(
      porcentaje_total = sum(porcentaje),
      .groups = "drop"
    ) %>%
    filter(porcentaje_total > 0) %>%
    arrange(porcentaje_total)
  
  # 5. Mantener solamente variables con faltantes
  grafico_df <- faltantes %>%
    semi_join(totales, by = "variable") %>%
    mutate(
      variable = factor(variable, levels = totales$variable),
      anio = factor(anio, levels = anios)
    )
  
  totales <- totales %>%
    mutate(variable = factor(variable, levels = variable))
  
  # 6. Gráfico de barras horizontales apiladas
 grafico4 <-  ggplot(
    grafico_df,
    aes(x = porcentaje, y = variable, fill = anio)
  ) +
    geom_col(
      position = position_stack(reverse = TRUE),
      width = 0.7,
      color = "white",
      linewidth = 0.4
    ) +
    
    # Porcentaje de cada segmento
    geom_text(
      aes(label = ifelse(
        porcentaje >= 3,
        paste0(round(porcentaje, 1), "%"),
        ""
      )),
      position = position_stack(
        vjust = 0.5,
        reverse = TRUE
      ),
      size = 3.2
    ) +
    
    # Porcentaje total al final de cada barra
    geom_text(
      data = totales,
      aes(
        x = porcentaje_total,
        y = variable,
        label = paste0(round(porcentaje_total, 1), "%")
      ),
      inherit.aes = FALSE,
      hjust = -0.2,
      size = 3.5
    ) +
    
    scale_fill_manual(
      values = c(
        "2001" = "#FFE89C",
        "2008" = "#A8E7BC",
        "2022" = "#A8D5F7"
      )
    ) +
    
    scale_x_continuous(
      limits = c(0, 108),
      breaks = seq(0, 100, 20),
      labels = function(x) paste0(x, "%"),
      expand = c(0, 0)
    ) +
    
    labs(
      title = "Porcentaje de datos faltantes por variable",
      subtitle = "Distribución de faltantes entre 2001, 2008 y 2022",
      x = "Porcentaje de datos faltantes",
      y = NULL,
      fill = "Año"
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.major.y = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold"),
      axis.text.y = element_text(size = 10)
    )
  print(grafico4)
  ggsave(file.path("output/graficos", "datos_faltanes_vars.png"), 
         plot = grafico4, width = 12, height = 7, units = "in", dpi = 300)


  

  
  
  
  
  
  
  
  #Grafico para ver la distribucion del ingreso en la totalidad de los paises
  library(dplyr)
  library(ggplot2)
  
  # 1. Calculamos la cantidad y el porcentaje de cada categoría
  dist_ingreso <- wdi2 |>
    # Si wdi2 tiene datos panel (múltiples años por país), podrías querer 
    # quedarte con un solo año o países únicos antes usando: distinct(pais, ingreso) |>
    count(ingreso) |>
    mutate(pct = n / sum(n))
  
  # 2. Creamos el gráfico de barras
 

  grafico11 <- ggplot(dist_ingreso, aes(x = ingreso, y = pct)) +
    geom_col(fill = "steelblue", color = "black", alpha = 0.8) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1) # Formatea el eje Y como 10%, 20%, etc.
    ) +
    labs(
      title = "Distribución global por nivel de ingreso",
      subtitle = "Porcentaje de países en cada categoría sobre el total",
      x = "Nivel de ingreso",
      y = "Porcentaje del total de países"
    ) +
    theme_minimal(base_size = 18) +
    theme(
      # Inclinamos el texto del eje X por si los nombres de los ingresos son muy largos
      axis.text.x = element_text(angle = 45, hjust = 1) 
    )
  
  print(grafico11)
  
  
  
 
  
  
  
  
  
  
   
  
  
  
  
  
  
  
  

  
  
  
  
  library(ggplot2)
  library(dplyr)
  library(scales)
  
  # 1. Calculamos los totales por variable y año para la etiqueta final (el total al final de la barra)
  totales_na <- na_largo |>
    group_by(anio, variable) |>
    summarise(total_pct = sum(pct_na), .groups = "drop")
  
  # 2. Creamos una función que genera el gráfico individual para un año específico
  crear_grafico_anio <- function(anio_filtro) {
    
    # Filtramos los datos para el año que toca
    datos_anio <- na_largo |> filter(anio == anio_filtro)
    totales_anio <- totales_na |> filter(anio == anio_filtro)
    
    # Definimos un límite x dinámico para que el total no quede cortado por el margen
    limite_x <- max(totales_anio$total_pct) * 1.15
    
    ggplot(datos_anio, aes(y = reorder(variable, pct_na, sum), x = pct_na, fill = ingreso2)) +
      
      # Barras apiladas horizontalmente
      geom_col(color = "white", width = 0.6) +
      
      # Etiquetas de porcentaje dentro de cada segmento (oculta los < 1.5% para evitar superposición)
      geom_text(aes(label = ifelse(pct_na > 0.015, percent(pct_na, accuracy = 0.1), "")),
                position = position_stack(vjust = 0.5), size = 3.5) +
      
      # Etiqueta del total al final de la barra
      geom_text(data = totales_anio, 
                aes(x = total_pct, y = variable, label = percent(total_pct, accuracy = 0.1), fill = NULL),
                hjust = -0.2, size = 4) +
      
      # Escalas y formato (usando una paleta parecida a tu imagen de referencia)
      scale_x_continuous(labels = percent, limits = c(0, limite_x)) +
      scale_fill_manual(values = c("#fde293", "#a8e6b5", "#a9d0f5", "#dcdcdc")) + # Ajusta los colores según la cantidad de niveles de ingreso
      
      labs(
        title = paste("Porcentaje de datos faltantes por variable - Año", anio_filtro),
        subtitle = "Distribución de faltantes por nivel de ingreso",
        x = "Porcentaje de datos faltantes",
        y = "",
        fill = "Nivel de ingreso"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(face = "bold")
      )
  }
  
  # 3. Aplicamos la función para generar una lista con los gráficos separados
  años_unicos <- unique(na_largo$anio)
  graficos_separados <- lapply(años_unicos, crear_grafico_anio)
  names(graficos_separados) <- años_unicos
  
  # Para visualizar cada gráfico, simplemente llámalos por su año o índice:
  print(graficos_separados[["2001"]])
  print(graficos_separados[["2008"]])
  print(graficos_separados[["2022"]])
  
  
  
  
  
  
  
  
  
  #Datos faltantes por pais
  
  library(dplyr)
  library(tidyr)
  
 
  
  
  faltantes_pais <- wdi2 |>
    group_by(pais) |>
    summarise(ingreso = first(ingreso2),
      observaciones = n() * length(variables),
      faltantes = sum(
        across(all_of(variables), ~ sum(is.na(.x)))
      ),
      pct_faltantes = 100 * faltantes / observaciones,
      .groups = "drop"
    ) |>
    arrange(desc(pct_faltantes))
  
  print(faltantes_pais, n = Inf)
  
  
  
  
  faltantes_pais_2001 <- wdi_2001 |>
    group_by(pais) |>
    summarise(ingreso = first(ingreso2),
      observaciones = n() * length(variables),
      faltantes = sum(
        across(all_of(variables), ~ sum(is.na(.x)))
      ),
      pct_faltantes = 100 * faltantes / observaciones,
      .groups = "drop"
    ) |>
    arrange(desc(pct_faltantes))
  
  print(faltantes_pais_2001, n = Inf)
  
  
  faltantes_pais_2008 <- wdi_2008 |>
    group_by(pais) |>
    summarise(ingreso = first(ingreso2),
      observaciones = n() * length(variables),
      faltantes = sum(
        across(all_of(variables), ~ sum(is.na(.x)))
      ),
      pct_faltantes = 100 * faltantes / observaciones,
      .groups = "drop"
    ) |>
    arrange(desc(pct_faltantes))
  
  print(faltantes_pais_2008, n = Inf)
  
  
  
  
  faltantes_pais_2022 <- wdi_2022 |>
    group_by(pais) |>
    summarise(ingreso = first(ingreso2),
      observaciones = n() * length(variables),
      faltantes = sum(
        across(all_of(variables), ~ sum(is.na(.x)))
      ),
      pct_faltantes = 100 * faltantes / observaciones,
      .groups = "drop"
    ) |>
    arrange(desc(pct_faltantes))
  
  print(faltantes_pais_2022, n = Inf)
  
  

 
 
 
 
 
 
  rm(wdi_final, wdi_final_2001, wdi_final_2008, wdi_final_2022, wdi_filtrado, reporte_eliminados)
 
 
 paises_eliminados <- (c("Korea, Dem. People's Rep.", "South Sudan", "Somalia",
             "Cuba", "Venezuela, RB", "Yemen", "Eritrea", "Kuwait", "Central African Republic"))
 
 rm(wdi_filtrada_2001, wdi_filtrada_2008, wdi_filtrada_2022)
 library(dplyr)
 library(tidyr)
 
 # 1. Definir los países irrecuperables (Agujeros negros)
 paises_eliminados <- c(
   "Korea, Dem. People's Rep.", "Somalia, Fed. Rep.", 
   "South Sudan", "Eritrea", "Afghanistan"
 )
 
 # 2. Limpieza e imputación general
 wdi_filtrado <- wdi3 |> 
   # Paso A: Eliminar países sin estructura de reporte
   filter(!pais %in% paises_eliminados) |> 
   
   # Paso B: Eliminar variables irrecuperables (>40% NAs)
   select(-tipo_cambio_real_efectivo, -gastos_totales_gob_pbi) |> 
   
   group_by(pais) |> 
   arrange(anio) |> 
   mutate(
     # Paso C: Interpolación masiva (lag + lead) para el bloque de variables estables
     across(
       .cols = c(
         crecimiento_industria,
         industria_pbi,
         indice_nivel_precios,
         empleo_industrial,
         exportacion_manufactura,
         gasto_gobierno_pbi,
         valor_agregado_trabajador,
         ahorro_bruto_pbi,
         credito_privado_pbi,
         recursos_naturales,
         pbi_per_capita,
         pbi_millones
       ),
       .fns = ~ if_else(
         condition = is.na(.x) & anio %in% c(2001, 2008, 2022), 
         true = (lag(.x) + lead(.x)) / 2, 
         false = .x
       )
     ),
     
     # Paso D: Rescate asimétrico único para la variable de recursos naturales (2021 -> 2022)
     recursos_naturales = if_else(
       condition = is.na(recursos_naturales) & anio == 2022, 
       true = lag(recursos_naturales, n = 1), 
       false = recursos_naturales
     )
   ) |> 
   ungroup() |> 
   
   # Paso E: Recortar la base a la ventana de estudio
   filter(anio %in% c(2001, 2008, 2022))
 
 # 3. Aplicar la guillotina final a todas las variables sensibles (<5% NAs o de interés)
 wdi_final <- wdi_filtrado |> 
   drop_na(
     crecimiento_industria,
     industria_pbi,
     indice_nivel_precios,
     empleo_industrial,
     exportacion_manufactura,
     gasto_gobierno_pbi,
     valor_agregado_trabajador,
     ahorro_bruto_pbi,
     credito_privado_pbi,
     recursos_naturales,
     pbi_per_capita,
     pbi_millones)
 
 
 # 4. Auditoría de bajas (¿Quién cayó y por culpa de qué variable?)
 reporte_eliminados <- anti_join(wdi_filtrado, wdi_final, by = c("pais", "anio")) |> 
   select(pais, anio, crecimiento_industria,
          industria_pbi,
          indice_nivel_precios,
          empleo_industrial,
          exportacion_manufactura,
          gasto_gobierno_pbi,
          valor_agregado_trabajador,
          ahorro_bruto_pbi,
          credito_privado_pbi,
          recursos_naturales,
          pbi_per_capita,
          pbi_millones, region, ingreso) |> 
   arrange(pais, anio)
 
 print(reporte_eliminados, n = Inf)
 show(reporte_eliminados)

  wdi_final_2001 <- wdi_final |> 
   filter(anio == 2001)
 
 wdi_final_2008 <- wdi_final |> 
   filter(anio == 2008)
 
 wdi_final_2022 <- wdi_final |> 
   filter(anio == 2022)
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 #Comparar distribucion por ingreso antes y despues de filtrar
 #Antes de limpiar NAs
 dist_ingreso <- wdi2 |>
   count(ingreso) |>
   mutate(pct = n / sum(n))
 
 grafico11 <- ggplot(dist_ingreso, aes(x = ingreso, y = pct)) +
   geom_col(fill = "steelblue", color = "black", alpha = 0.8) +
   scale_y_continuous(
     labels = scales::percent_format(accuracy = 1) # Formatea el eje Y como 10%, 20%, etc.
   ) +
   labs(
     title = "Distribución global por nivel de ingreso",
     subtitle = "Porcentaje de países en cada categoría sobre el total",
     x = "Nivel de ingreso",
     y = "Porcentaje del total de países"
   ) +
   theme_minimal(base_size = 18) +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1) 
   )
 
 print(grafico11)
 
 
 #Despues de filtrar NAs
 
 dist_ingreso_filtr <- wdi_final |>
   count(ingreso) |>
   mutate(pct = n / sum(n))
 
 grafico12 <- ggplot(dist_ingreso_filtr, aes(x = ingreso, y = pct)) +
   geom_col(fill = "steelblue", color = "black", alpha = 0.8) +
   scale_y_continuous(
     labels = scales::percent_format(accuracy = 1) # Formatea el eje Y como 10%, 20%, etc.
   ) +
   labs(
     title = "Distribución global por nivel de ingreso (Base filtrada)",
     subtitle = "Porcentaje de países en cada categoría sobre el total",
     x = "Nivel de ingreso",
     y = "Porcentaje del total de países"
   ) +
   theme_minimal(base_size = 18) +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1) 
   )
 
 print(grafico12)
#Se ve un menor porcentaje de paises de menor ingreso (sesgo) 
 
 
 
 
 #Lo mismo pero por regiones
#Antes de filtrar 
 
 dist_regiones <- wdi2 |>
   count(region) |>
   mutate(pct = n / sum(n))
 
 grafico13 <- ggplot(dist_regiones, aes(x = region, y = pct)) +
   geom_col(fill = "steelblue", color = "black", alpha = 0.8) +
   scale_y_continuous(
     labels = scales::percent_format(accuracy = 1) # Formatea el eje Y como 10%, 20%, etc.
   ) +
   labs(
     title = "Distribución global por region",
     subtitle = "Porcentaje de países en cada categoría sobre el total",
     x = "Region",
     y = "Porcentaje del total de países"
   ) +
   theme_minimal(base_size = 18) +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1) 
   )
 
 print(grafico13)
 
 
 #Despues de filtrar
 
 dist_regiones_filt <- wdi_final |>
   count(region) |>
   mutate(pct = n / sum(n))
 
 grafico14 <- ggplot(dist_regiones, aes(x = region, y = pct)) +
   geom_col(fill = "steelblue", color = "black", alpha = 0.8) +
   scale_y_continuous(
     labels = scales::percent_format(accuracy = 1) # Formatea el eje Y como 10%, 20%, etc.
   ) +
   labs(
     title = "Distribución global por regiones (base filtrada)",
     subtitle = "Porcentaje de países en cada categoría sobre el total",
     x = "Region",
     y = "Porcentaje del total de países"
   ) +
   theme_minimal(base_size = 18) +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1) 
   )
 
 print(grafico14)
 
 
 
 
 
 
 
 
 
