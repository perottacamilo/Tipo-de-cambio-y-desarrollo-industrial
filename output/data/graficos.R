#Script Graficos

# install.packages("scales")
#install.packages(c("gt", "webshot2"))

library(readxl)
library(ggplot2)
library(tidyverse)
library(scales)
library(modelsummary)
library(dplyr)

rm(list = ls())

wdi_limpio <- read.csv("output/data/wdi_limpio.csv")

wdi2 <- read.csv("output/data/wdi2.csv")

wdi_final <- read.csv("output/data/wdi_final.csv")

wdi_analisis <- wdi2 |> 
  filter(!is.na(industria_pbi))

anios <- c(2001, 2008, 2022)

# Crear la lista asignando nombres según el año
tablas_anios <- setNames(
  lapply(anios, function(a) {
    df_anio <- wdi2 |> filter(anio == a)
    
    mod1 <- lm(industria_pbi ~ indice_nivel_precios, data = df_anio)
    mod2 <- lm(exportacion_manufactura ~ indice_nivel_precios, data = df_anio)
    
    modelsummary(
      list(
        "Industria (% PBI)" = mod1,
        "exportacion manufactura" = mod2
      ),
      title = paste0("Tabla: Regresiones simples para el nivel de precios (", a, ")"),
      stars = TRUE,
      statistic = "std.error",
      fmt = 3,
      coef_rename = c(
        "(Intercept)" = "Constante",
        "indice_nivel_precios" = "Índice de nivel de precios"
      ),
      gof_map = list(
        list("raw" = "nobs", "clean" = "N", "fmt" = 0),
        list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
        list("raw" = "adj.r.squared", "clean" = "R² ajustado", "fmt" = 3)
      ),
      output = "gt"
    )
  }),
  nm = paste0("anio_", anios)
)

# Ver la tabla del año 2001
tablas_anios$anio_2001

# Ver la tabla del año 2008
tablas_anios$anio_2008

# Ver la tabla del año 2022
tablas_anios$anio_2022

wdi_2001 <- wdi2 |>
  filter(anio == 2001)
wdi_2008 <- wdi2 |>
  filter(anio == 2008)
wdi_2022<- wdi2 |>
  filter(anio == 2022)

library(modelsummary)
library(dplyr)

# Estimación de todos los modelos
mod1_2001 <- lm(industria_pbi ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2001))
mod2_2001 <- lm(exportacion_manufactura ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2001))

mod1_2008 <- lm(industria_pbi ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2008))
mod2_2008 <- lm(exportacion_manufactura ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2008))

mod1_2022 <- lm(industria_pbi ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2022))
mod2_2022 <- lm(exportacion_manufactura ~ indice_nivel_precios, data = wdi2 |> filter(anio == 2022))

# Muestra la tabla visualmente en la pestaña Viewer (al costado)
modelsummary(
  list(
    "Ind 2001" = mod1_2001, "Exp 2001" = mod2_2001,
    "Ind 2008" = mod1_2008, "Exp 2008" = mod2_2008,
    "Ind 2022" = mod1_2022, "Exp 2022" = mod2_2022
  ),
  title = "Tabla Comparativa: Regresiones simples (2001, 2008, 2022)",
  stars = TRUE,
  statistic = "std.error",
  fmt = 3,
  coef_rename = c(
    "(Intercept)" = "Constante",
    "indice_nivel_precios" = "Índice de nivel de precios"
  ),
  gof_map = list(
    list("raw" = "nobs", "clean" = "N", "fmt" = 0),
    list("raw" = "r.squared", "clean" = "R²", "fmt" = 3),
    list("raw" = "adj.r.squared", "clean" = "R² ajustado", "fmt" = 3)
  ),
  output = "gt" # <--- Se renderiza visualmente en la pestaña Viewer
)

#Tabla de variables faltantes por ingreso 
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
grafico1 <- ggplot(na_largo, aes(x = ingreso2, y = pct_na, fill = anio)) +
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

print(grafico1)

ggsave(file.path("output/graficos", "datos_faltanes_ingreso.png"), 
       plot = grafico1, width = 12, height = 7, units = "in", dpi = 300)




#Grafico distribucion industria
grafico2 <- ggplot(wdi_analisis, aes(x = industria_pbi)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.8) +
  facet_wrap(~ anio, ncol = 1) + # Separa el gráfico por año
  labs(title = "Distribución global de la participación industrial",
       subtitle = "Frecuencia de países según el peso de su industria en el PBI",
       x = "Industria (% del PBI)", 
       y = "Cantidad de países") +
  theme_minimal()
print(grafico2)

ggsave(file.path("output/graficos", "histograma_industria.png"), plot = grafico2)


#Graficos plots industria_pbi y nivel de precios para 2001
#Plot 
#Correr de 90 a 103 para guardar en la carpeta graficos)
# png("output/graficos/plot_2001.png", width = 8, height = 6, units = "in", res = 300)

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

# dev.off()


#Graficos plots industria_pbi y nivel de precios para 2008

# png("output/graficos/plot_2008.png", width = 8, height = 6, units = "in", res = 300)

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

# dev.off()

#Plots industria_pbi y nivel de precios para 2008

# png("output/graficos/plot_2022.png", width = 8, height = 6, units = "in", res = 300)

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

summary(lm(industria_pbi ~ indice_nivel_precios, data = wdi_2022))
# dev.off()





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
  summarize(mediana_precios = median(indice_nivel_precios, na.rm = TRUE), 
    .groups = "drop")

grafico4 <- ggplot(medianas_evolucion, aes(x = anio, y = mediana_precios, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("Latin America & Caribbean" = "#D9534F", 
                                "East Asia & Pacific" = "#5BC0DE",
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

print(grafico4)

ggsave(file.path("output/graficos", "Evolucion_Indice_nivel_precios.png"), plot = grafico4)

cor(wdi_2001$industria_pbi, wdi_2001$indice_nivel_precios,
    use = "complete.obs")

# cor.test() agrega el test de significatividad y el intervalo de confianza
cor.test(wdi_2001$industria_pbi, wdi_2001$indice_nivel_precios)
#La correlacion es significativa al 1%









alta  <- wdi_2001$industria_pbi[wdi_2001$region == "East Asia & Pacific"]
baja  <- wdi_2001$industria_pbi[wdi_2001$region %in% c("Latin America & Caribbean", "")]

# png("output/graficos/histograma_industria_regiones_2001.png", width = 8, height = 6, units = "in", res = 300)

hist(baja, breaks = 15, col = rgb(1, 0, 0, 0.5), border = "white",
     xlim = c(0, 100), ylim = c(0, 12),
     main = "2001",
     xlab = "% industria en el PBI", ylab = "Cantidad de países")
hist(alta, breaks = 15, col = rgb(0, 0, 1, 0.5), border = "white", add = TRUE)

# dev.off()





alta1  <- wdi_2008$industria_pbi[wdi_2008$region == "East Asia & Pacific"]
baja1  <- wdi_2008$industria_pbi[wdi_2008$region %in% c("Latin America & Caribbean", "")]

# png("output/graficos/histograma_industria_regiones_2008.png", width = 8, height = 6, units = "in", res = 300)

hist(baja1, breaks = 15, col = rgb(1, 0, 0, 0.5), border = "white",
     xlim = c(0, 100), ylim = c(0, 12),
     main = "2008",
     xlab = "% industria en el PBI", ylab = "Cantidad de países")
hist(alta1, breaks = 15, col = rgb(0, 0, 1, 0.5), border = "white", add = TRUE)

# dev.off()


alta2  <- wdi_2022$industria_pbi[wdi_2022$region == "East Asia & Pacific"]
baja2  <- wdi_2022$industria_pbi[wdi_2022$region %in% c("Latin America & Caribbean", "")]

# png("output/graficos/histograma_industria_regiones_2022.png", width = 8, height = 6, units = "in", res = 300)

hist(baja2, breaks = 15, col = rgb(1, 0, 0, 0.5), border = "white",
     xlim = c(0, 100), ylim = c(0, 12),
     main = "2022",
     xlab = "% industria en el PBI", ylab = "Cantidad de países")
hist(alta2, breaks = 15, col = rgb(0, 0, 1, 0.5), border = "white", add = TRUE)

# dev.off()



 
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
  "poblacion_mill")

wdi_2 <- wdi2 |> 
  filter(poblacion_mill > 1)

variables <- intersect(variables, names(wdi_2))

if (nrow(wdi_2) == 0 || length(variables) == 0) {
  stop("No hay observaciones o variables disponibles.")
}

faltantes <- wdi_2 %>%
  group_by(anio) %>%
  summarise(
    across(all_of(variables), ~sum(is.na(.x))),
    .groups = "drop") %>%
  pivot_longer(
    cols = -anio,
    names_to = "variable",
    values_to = "n_faltantes") %>%
  complete(
    variable = variables,
    fill = list(n_faltantes = 0)) %>%
  mutate(
    porcentaje = 100 * n_faltantes / nrow(wdi_2))

totales <- faltantes %>%
  group_by(variable) %>%
  summarise(
    porcentaje_total = sum(porcentaje),
    .groups = "drop") %>%
  filter(porcentaje_total > 0) %>%
  arrange(porcentaje_total)

grafico_df <- faltantes %>%
  semi_join(totales, by = "variable") %>%
  mutate(
    variable = factor(variable, levels = totales$variable))

totales <- totales %>%
  mutate(variable = factor(variable, levels = variable))



grafico5 <-  ggplot(
  grafico_df,
  aes(x = porcentaje, y = variable, fill = factor(anio))) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.7,
    color = "white",
    linewidth = 0.4) +
  geom_text(
    aes(label = ifelse(
      porcentaje >= 3,
      paste0(round(porcentaje, 1), "%"),"")),
    position = position_stack(
      vjust = 0.5,
      reverse = TRUE),
    size = 3.2) +
    geom_text(
    data = totales,
    aes(
      x = porcentaje_total,
      y = variable,
      label = paste0(round(porcentaje_total, 1), "%")),
    inherit.aes = FALSE,
    hjust = -0.2,
    size = 3.5) +
  scale_fill_manual(
    values = c(
      "2001" = "#FFE89C",
      "2008" = "#A8E7BC",
      "2022" = "#A8D5F7")) +
  scale_x_continuous(
    limits = c(0, 108),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)) +
  labs(
    title = "Porcentaje de datos faltantes por variable",
    subtitle = "Distribución de faltantes entre 2001, 2008 y 2022",
    x = "Porcentaje de datos faltantes",
    y = NULL,
    fill = "Año") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10))

print(grafico5)

ggsave(file.path("output/graficos", "datos_faltanes_vars.png"), 
       plot = grafico5, width = 12, height = 7, units = "in", dpi = 300)





















# 1. Calculamos los totales por variable y año para la etiqueta final (el total al final de la barra)
#Es necesario correr desde 34 a 56
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
      plot.title = element_text(face = "bold"))}

# 3. Aplicamos la función para generar una lista con los gráficos separados
años_unicos <- unique(na_largo$anio)
graficos_separados <- lapply(años_unicos, crear_grafico_anio)
names(graficos_separados) <- años_unicos

# Para visualizar cada gráfico, simplemente llámalos por su año o índice:
print(graficos_separados[["2001"]])
print(graficos_separados[["2008"]])
print(graficos_separados[["2022"]])


#Creo que no va a ser muy util este graph








#Comparar distribucion por ingreso antes y despues de filtrar
#Antes de limpiar NAs
dist_ingreso <- wdi2 |>
  count(ingreso) |>
  mutate(pct = n / sum(n))

grafico6 <- ggplot(dist_ingreso, aes(x = reorder(ingreso, pct), y = pct)) +
  geom_col(fill = "steelblue", color = "black", alpha = 0.8) +
  scale_y_continuous(limits = c(0, 0.4),
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
    axis.text.x = element_text(angle = 45, hjust = 1))

print(grafico6)


#Despues de filtrar NAs

dist_ingreso_filtr <- wdi_final |>
  count(ingreso) |>
  mutate(pct = n / sum(n))

grafico7 <- ggplot(dist_ingreso_filtr, aes(x = reorder(ingreso, pct), y = pct)) +
  geom_col(fill = "steelblue", color = "black", alpha = 0.8) +
  scale_y_continuous(limits = c(0, 0.4),
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

print(grafico7)
#Se ve un menor porcentaje de paises de menor ingreso (sesgo) 




#Lo mismo pero por regiones
#Antes de filtrar 

dist_regiones <- wdi2 |>
  count(region) |>
  mutate(pct = n / sum(n))

grafico8 <- ggplot(dist_regiones, aes(x = reorder(region, pct), y = pct)) +
  geom_col(fill = "steelblue", color = "black", alpha = 0.8) +
  scale_y_continuous(limits = c(0, 0.35),
    labels = scales::percent_format(accuracy = 1) 
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

print(grafico8)


#Despues de filtrar

dist_regiones_filt <- wdi_final |>
  count(region) |>
  mutate(pct = n / sum(n))

grafico9 <- ggplot(dist_regiones_filt, aes(x = reorder(region, pct), y = pct)) +
  geom_col(fill = "steelblue", color = "black", alpha = 0.8) +
  scale_y_continuous(limits = c(0, 0.35),
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

print(grafico9)

ggsave(file.path("output/graficos", "distribucion_ingresos_crudo.png"), 
       plot = grafico6, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos", "distribucion_ingreso_filtro.png"), 
       plot = grafico7, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos", "distribucion_region_crudo.png"), 
       plot = grafico8, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos", "distribucion_region_filtro.png"), 
       plot = grafico9, width = 12, height = 7, units = "in", dpi = 300)





alta  <- wdi_2001$industria_pbi[wdi_2001$region == "East Asia & Pacific"]
baja  <- wdi_2001$industria_pbi[wdi_2001$region %in% c("Latin America & Caribbean", "")]

# png("output/graficos/histograma_industria_regiones_2001.png", width = 8, height = 6, units = "in", res = 300)

hist(baja, breaks = 15, col = rgb(1, 0, 0, 0.5), border = "white",
     xlim = c(0, 100), ylim = c(0, 12),
     main = "2001",
     xlab = "% industria en el PBI", ylab = "Cantidad de países")
hist(alta, breaks = 15, col = rgb(0, 0, 1, 0.5), border = "white", add = TRUE)




library(dplyr)
library(ggplot2)
# library(scales) # Opcional, para dar formato de porcentaje al eje Y

# 1. Calcular porcentajes para la primera base (ej: wdi_2001)
final <- wdi2 %>%
  count(region) %>%
  mutate(porcentaje = (n / sum(n)) * 100,
         base = "Antes de filtrar") # Creamos una etiqueta para identificar estos datos

# 2. Calcular porcentajes para la segunda base (ej: wdi_2023)
filtrado <- wdi_final %>%
  count(region) %>%
  mutate(porcentaje = (n / sum(n)) * 100,
         base = "Base filtrada") # Etiqueta para la segunda base

# 3. Unir ambos dataframes verticalmente
datos_combinados <- bind_rows(final, filtrado)

# 4. Graficar comparando ambas bases
ggplot(datos_combinados, aes(x = reorder(region, -porcentaje), y = porcentaje, fill = base)) +
  # position = "dodge" es la clave: pone las barras lado a lado en lugar de apilarlas
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  
  # Estética general replicando tu imagen
  labs(title = "Distribución global por región",
       subtitle = "Porcentaje de países en cada categoría sobre el total",
       x = "Región",
       y = "Porcentaje del total de países",
       fill = "Año de la base") + # Título de la leyenda
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, size=1))







ingreso1 <- wdi2 %>%
  count(ingreso) %>%
  mutate(porcentaje = (n / sum(n)) * 100,
         base = "Antes de filtrar") # Creamos una etiqueta para identificar estos datos

# 2. Calcular porcentajes para la segunda base (ej: wdi_2023)
ingreso2 <- wdi_final %>%
  count(ingreso) %>%
  mutate(porcentaje = (n / sum(n)) * 100,
         base = "Base filtrada") # Etiqueta para la segunda base

# 3. Unir ambos dataframes verticalmente
datos_combinados <- bind_rows(ingreso1, ingreso2)

# 4. Graficar comparando ambas bases
ggplot(datos_combinados, aes(x = reorder(ingreso, -porcentaje), y = porcentaje, fill = base)) +
  # position = "dodge" es la clave: pone las barras lado a lado en lugar de apilarlas
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  
  # Estética general replicando tu imagen
  labs(title = "Distribución de los ingresos",
       subtitle = "Porcentaje de países en cada categoría sobre el total",
       x = "Región",
       y = "Porcentaje del total de países",
       fill = "Año de la base") + # Título de la leyenda
  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "black", fill=NA, size=1))













library(dplyr)
library(tidyr)
library(ggplot2)

# Asumiendo que tu base se llama 'wdi' y tienes columnas como 'pais', 'region', 'pbi', 'pct_industria', etc.

# 1. Transformar los datos de formato "ancho" a "largo"
datos_largos <- wdi2 %>%
  # Selecciona las variables que quieres graficar (además de país o región si las necesitas para agrupar)
  select(pais, region, crecimiento_industria,
         industria_pbi,
         tipo_cambio_real_efectivo,
         indice_nivel_precios,
         empleo_industrial,
         exportacion_manufactura,
         gasto_gobierno_pbi,
         gastos_totales_gob_pbi,
         valor_agregado_trabajador,
         ahorro_bruto_pbi,
         credito_privado_pbi,
         recursos_naturales,
         pbi_per_capita,
         pbi_millones) %>%
  # pivot_longer apila todas las variables en dos columnas: una con el nombre y otra con el valor
  pivot_longer(
    cols = c(crecimiento_industria,
             industria_pbi,
             tipo_cambio_real_efectivo,
             indice_nivel_precios,
             empleo_industrial,
             exportacion_manufactura,
             gasto_gobierno_pbi,
             gastos_totales_gob_pbi,
             valor_agregado_trabajador,
             ahorro_bruto_pbi,
             credito_privado_pbi,
             recursos_naturales,
             pbi_per_capita,
             pbi_millones), # Las columnas numéricas a apilar
    names_to = "variable",                       # Nombre de la nueva columna identificadora
    values_to = "valor"                          # Nombre de la nueva columna numérica
  ) %>%
  # Filtrar NAs si es necesario para que no molesten en el gráfico
  filter(!is.na(valor))

# 2. Crear el gráfico con paneles (facet_wrap)
library(dplyr)
library(tidyr)
library(ggplot2)

# Gráfico global sin agrupar por región
grafico_macro_global <- ggplot(datos_largos, aes(x = valor, y = "")) +
  # Boxplot general
  geom_boxplot(fill = "steelblue", alpha = 0.5, color = "black", outlier.shape = NA) +
  # Puntos de todos los países
  geom_jitter(color = "black", alpha = 0.2, height = 0.2, size = 1) +
  
  # Dividir por variable en 3 columnas
  facet_wrap(~ variable, scales = "free_x", ncol = 3) + 
  
  labs(
    title = "Distribución global de indicadores macroeconómicos",
    subtitle = "Todos los países agregados",
    x = "Valor (escala propia de cada variable)",
    y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    # Ocultar las marcas y el texto del eje Y ya que no hay categorías
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

# Guardar con dimensiones adecuadas
ggsave("output/graficos/distribucion_global_macro.png", 
       plot = grafico_macro_global, 
       width = 12,   # Al no tener múltiples regiones, requiere menos altura
       height = 8, 
       dpi = 300)

print(grafico_macro_global)































#Entrega 3: GRAFICOS
rm(list = ls())


wdi_limpio <- read.csv("output/data/wdi_limpio.csv")

wdi_limpio_2 <- wdi_limpio |> 
  filter(region %in% c("East Asia & Pacific", "Latin America & Caribbean"), poblacion_mill > 1, anio >= 1990, 
         !pais %in% c("Cuba", "Korea, Dem. People's Rep.", "Venezuela, RB")) |> 
  select(pais, anio, industria_pbi, exportacion_manufactura, region, pbi_per_capita, indice_nivel_precios) |> 
  arrange(pais, anio)


wdi_limpio_2 <- wdi_limpio_2 |>
  filter(region %in% c("East Asia & Pacific", "Latin America & Caribbean"),
    !pais %in% c("Cuba", "Korea, Dem. People's Rep.", "Venezuela, RB")) |> 
  mutate(industria_pc = (industria_pbi*pbi_per_capita)/100) 
 
fuente <-  "Fuente: Elaboración propia en base a WDI (Banco Mundial)."
  
  
  
  
  
  
  
  
  imputar_vecinos <- function(x, anio) {
    ant <- if_else(lag(anio) == anio - 1, lag(x), NA_real_)
    sig <- if_else(lead(anio) == anio + 1, lead(x), NA_real_)
    case_when(
      !is.na(ant) & !is.na(sig) ~ (ant + sig) / 2,
      TRUE                      ~ NA_real_
    )}

wdi_filtrado_2 <- wdi_limpio_2 |> 
  group_by(pais) |> 
  arrange(pais, anio) |> 
  mutate(
    across(
      .cols = c(industria_pbi, exportacion_manufactura, industria_pc),
      .fns = ~ if_else(
        condition = is.na(.x),  # Imputa para cualquier año si falta el dato y hay vecinos
        true = imputar_vecinos(.x, anio),
        false = .x
      )
    )
  ) |> 
  ungroup() 

write.csv(wdi_filtrado_2, "output/data/wdi_filtrado_2.csv")



#Grafico fidedignus


library(tidyverse)
library(scales)
library(ggtext)

# 2) Definimos tus colores exactos asignados a los nombres en español (sin Asia sin China)
col_personalizados <- c(
  "Asia Oriental y Pacífico"     = "#E0195B", 
  "América Latina y el Caribe" = "#3919E0", 
  "Argentina"                             = "#19E09E", 
  "China"                                 = "#FFAC47"
)

# 3) Promedio de Asia Oriental y Pacífico (Total)
df_eap <- wdi_limpio_2 %>%
  filter(region == "East Asia & Pacific") %>%
  group_by(anio) %>%
  summarise(industria_pc = mean(industria_pc, na.rm = TRUE), .groups = "drop") %>%
  mutate(pais = "Asia Oriental y Pacífico") %>%
  select(year = anio, pais, industria_pc)

# 4) Promedio de América Latina y el Caribe
df_lac <- wdi_limpio_2 %>%
  filter(region == "Latin America & Caribbean") %>%
  group_by(anio) %>%
  summarise(industria_pc = mean(industria_pc, na.rm = TRUE), .groups = "drop") %>%
  mutate(pais = "América Latina y el Caribe") %>%
  select(year = anio, pais, industria_pc)

# 5) Datos individuales para Argentina y China
df_paises <- wdi_limpio_2 %>%
  filter(pais %in% c("Argentina", "China")) %>%
  mutate(pais = if_else(pais == "China", "China", "Argentina")) %>%
  select(year = anio, pais, industria_pc)

# 6) Unimos todo en un solo data frame largo y filtramos el período
df_industrializacion <- bind_rows(df_eap, df_lac, df_paises) %>%
  filter(year >= 1990, year <= 2024)

# 7) Último año de cada serie para las etiquetas directas al final de las líneas
ult_ind_pc <- df_industrializacion %>% 
  group_by(pais) %>% 
  filter(year == max(year)) %>%
  ungroup()

# 8) Gráfico final
g_ind_pc_esp <- ggplot(df_industrializacion, aes(x = year, y = industria_pc, color = pais, group = pais)) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(data = ult_ind_pc, size = 3) +
  geom_text(data = ult_ind_pc,
            aes(x = 2024.2, label = pais),   # misma x para todas
            hjust = 0,                     # alineadas a la izquierda
            fontface = "bold", size = 3.5, show.legend = FALSE) +
  # Línea punteada para el ingreso de China a la OMC en 2001
  geom_vline(xintercept = 2001, linetype = "dotted", color = "grey60") +
  # 1) Texto con HTML usando geom_richtext (para que pinte de rojo la palabra China)
  geom_richtext(
    data = data.frame(x = 2001.8, y = max(df_industrializacion$industria_pc, na.rm = TRUE) * 0.08),
    aes(x = x, y = y, label = "2001: Ingreso de <span style='color:#ffac47'>China</span><br>a la OMC"),
    hjust = 0, size = 3.5, color = "grey35",
    fill = NA, label.color = NA, # Para que sea transparente y sin borde tipo etiqueta
    inherit.aes = FALSE
  ) +
  # 2) La flecha curva apuntando a la línea
  annotate("curve", x = 2002.1, xend = 2001.05, 
           y = max(df_industrializacion$industria_pc, na.rm = TRUE) * 0.08, 
           yend = max(df_industrializacion$industria_pc, na.rm = TRUE) * 0.08,
           curvature = 0.3, 
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"), 
           color = "grey35", linewidth = 0.6) +
  # 1) Texto de la crisis corrido a la izquierda
  geom_richtext(
    data = data.frame(x = 2009.1, y = max(df_industrializacion$industria_pc, na.rm = TRUE) * 0.92),
    aes(x = x, y = y, label = "Crisis del 2008"),
    hjust = 0, size = 3.5, color = "grey35",
    fill = NA, label.color = NA,
    inherit.aes = FALSE
  ) +
  
  # 2) Línea punteada vertical en el año 2008
  geom_vline(xintercept = 2008, linetype = "dotted", color = "grey60") +
  
  # 3) Flecha curva horizontal apuntando desde el final del texto hacia la línea 2008
  annotate("curve", x = 2008, xend = 2009, 
           y = max(df_industrializacion$industria_pc, na.rm = TRUE) * 0.92, 
           yend = max(df_industrializacion$industria_pc, na.rm = TRUE) * 0.92,
           curvature = 0.3, 
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"), 
           color = "grey35", linewidth = 0.6) +
  scale_color_manual(values = col_personalizados) +
  scale_x_continuous(breaks = c(seq(1990, 2020, by = 10), 2024),
                     limits = c(1990, 2030),
                     expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(labels = label_dollar(prefix = "$", big.mark = ".", decimal.mark = ","),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "La evolución <span style='color:#e0195b'>Asiática</span> liderada por <span style='color:#ffac47'>China</span> vs el estancamiento <span style='color:#2b5c8f'>Latinoamericano</span>",
    subtitle = "Evolución de la Producción Industrial Por Habitante (USD): 1990-2024",
    caption  = "Fuente: Elaboración propia en base a WDI (Banco Mundial).",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position        = "none",
    plot.title.position    = "plot",
    plot.title             = element_markdown(family = "serif", face = "bold",
                                              size = 16, lineheight = 1.1),
    plot.subtitle          = element_text(color = "grey35", margin = margin(b = 10)),
    plot.caption           = element_text(color = "grey45", hjust = 0, size = 9),
    panel.grid.minor       = element_blank(),
    panel.grid.major.x     = element_blank(),
    panel.grid.major.y     = element_line(color = "grey80", linetype = "dashed"),
    axis.text              = element_text(color = "grey40"),
    axis.ticks.x           = element_line(color = "grey60"),
    plot.margin            = margin(12, 40, 10, 12)
  )

g_ind_pc_esp


ggsave(file.path("output/graficos", "grafico_comunicativo.png"), 
       plot = g_ind_pc_esp, width = 12, height = 7, units = "in", dpi = 300)















#Grafico Exploratorio.
#Scaterplot.


library(tidyverse)
library(scales)
library(ggtext)
library(ggrepel)
library(showtext)
library(patchwork) # Indispensable para wrap_plots y plot_annotation
#install.packages("patchwork")
rm(list = ls())



wdi_final <- read.csv("output/data/wdi_final.csv")

wdi_final_2 <- wdi_final |> 
  mutate(industria_pc = (industria_pbi*pbi_per_capita)/100)


fuente <-  "Fuente: Elaboración propia en base a WDI (Banco Mundial)."



fuentes_ok <- tryCatch({
  font_add_google("Playfair Display", "playfair", regular.wt = 600, bold.wt = 700)
  font_add_google("Lato", "lato", regular.wt = 400, bold.wt = 700)
  TRUE
}, error = function(e) FALSE)

fuente_titulo <- if (fuentes_ok) "playfair" else "serif"
fuente_texto  <- if (fuentes_ok) "lato" else "sans"
showtext_auto()
showtext_opts(dpi = 96)

col <- c(rojo = "#B13507", azul = "#286BBB", gris = "#BDBDBD")
gris_titulo <- "#363636"
gris_texto  <- "#5B5B5B"
gris_suave  <- "#858585"

theme_owid <- function(base_size = 13) {
  theme_minimal(base_size = base_size, base_family = fuente_texto) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_markdown(family = fuente_titulo, size = rel(2.9), color = gris_titulo,
                                       lineheight = 1.05, margin = margin(b = 10)),
      plot.subtitle = element_text(size = rel(1.75), color = gris_texto,
                                   lineheight = 1.15, margin = margin(b = 20)),
      plot.caption  = element_text(size = rel(0.95), color = gris_suave, hjust = 0,
                                   margin = margin(t = 18)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(color = "#DDDDDD", linetype = "dashed", linewidth = 0.4),
      axis.line.x  = element_line(color = "#999999", linewidth = 0.4),
      axis.ticks.x = element_line(color = "#999999", linewidth = 0.4),
      axis.text    = element_text(color = "#666666", size = rel(1.05)),
      axis.title   = element_blank(),
      legend.position = "none",
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 22, 15, 20)
    )
}

pintar <- function(texto, color) sprintf("<span style='color:%s'><b>%s</b></span>", color, texto)

carpeta_graficos <- "graficos_clase21"
dir.create(carpeta_graficos, showWarnings = FALSE)





#Grafico final fidedigno definitivo





col_asia  <- "#E0195B"
col_latam <- "#3919E0"

# 1) Función de panel con margen ajustado para el nuevo subtítulo triple
panel_latam <- function(datos, subtitulo = datos$sub_final[1]) {
  
  escala_y <- if (datos$log_y[1]) {
    scale_y_log10(labels = label_number(big.mark = ".", decimal.mark = ","),
                  breaks = breaks_log(n = 5),
                  expand = expansion(mult = c(0.05, 0.35))) 
  } else {
    scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","),
                       breaks = breaks_extended(n = 4),
                       expand = expansion(mult = c(0.05, 0.35)))
  }
  
  resto <- filter(datos, grupo_region == "Resto del Mundo")
  foco  <- filter(datos, grupo_region != "Resto del Mundo")
  
  destacar <- c("Argentina", "China")
  
  ggplot(datos, aes(pib, valor)) +
    geom_point(data = resto, shape = 21, size = 1.6, stroke = 0.3,
               color = "#9E9E9E", fill = alpha(col[["gris"]], 0.55)) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                color = gris_titulo, linewidth = 0.6) +
    
    geom_point(data = foco, aes(fill = grupo_region), shape = 21, size = 2.3,
               stroke = 0.5, color = "white") +
    
    geom_label_repel(data = filter(datos, codigo %in% destacar), aes(label = codigo),
                     family = fuente_texto, fontface = "bold", size = 3.4,
                     fill = alpha("white", 0.85), label.size = 0, label.padding = unit(1.5, "pt"),
                     color = gris_titulo, min.segment.length = 0, segment.color = "#777777",
                     nudge_y = if (datos$log_y[1]) 0.3 else 0.08 * diff(range(datos$valor, na.rm = TRUE)),
                     seed = 1) +
    
    scale_fill_manual(values = c("América Latina" = col_latam, 
                                 "Asia Oriental" = col_asia)) +
    escala_y +
    scale_x_log10(labels = label_number(big.mark = ".", decimal.mark = ",")) +
    labs(title = datos$titulo[1], subtitle = subtitulo) +
    theme_owid(base_size = 9.5) %+replace%
    theme(
      plot.title    = element_text(family = fuente_texto, face = "bold", size = rel(2.9),
                                   color = gris_titulo, hjust = 0, margin = margin(b = 3)),
      # Aumentamos el lineheight y el margen inferior para acomodar 3 renglones
      plot.subtitle = element_text(size = rel(1.5), color = gris_texto, hjust = 0,
                                   lineheight = 0.6, margin = margin(b = 14)),
      panel.grid.major.x = element_line(color = "#E5E5E5", linetype = "dashed", linewidth = 0.3),
      axis.line.x = element_blank(), axis.ticks.x = element_blank(),
      plot.background = element_rect(fill = "#FAFAFA", color = "#E3E3E3"),
      plot.margin = margin(8, 10, 8, 8)
    )
}

# 2) Función juntar forzando interlineado
juntar <- function(paneles, titulo, subtitulo) {
  wrap_plots(paneles, ncol = 3) +
    plot_annotation(
      title = titulo, subtitle = subtitulo, caption = fuente,
      theme = theme_owid() +
        theme(
          plot.title    = ggtext::element_markdown(family = fuente_texto, face = "bold",
                                                   size = 40, hjust = 0,
                                                   lineheight = 0.4,
                                                   margin = margin(b = 6)),
          plot.subtitle = ggtext::element_markdown(size = 30, hjust = 0,
                                                   lineheight = 0.1,
                                                   margin = margin(b = 12)),
          plot.margin   = margin(15, 15, 10, 15)))}

# 3) Armado de los datos base (con corrección de etiqueta)
dispersion <- wdi_final_2 |>
  filter(anio %in% c(2001, 2008, 2022)) |>
  filter(!is.na(indice_nivel_precios), region != "Aggregates") |>
  mutate(
    grupo_region = case_when(
      region == "Latin America & Caribbean" ~ "América Latina",
      region == "East Asia & Pacific" ~ "Asia Oriental",
      TRUE ~ "Resto del Mundo"
    )
  ) |>
  select(pais, grupo_region, anio, pib = indice_nivel_precios, 
         Industria = industria_pc, 
         Exportaciones = exportacion_manufactura) |>
  pivot_longer(c(Industria, Exportaciones), names_to = "indicador", values_to = "valor") |>
  filter(!is.na(valor), valor > 0, !is.na(pib)) |>
  mutate(
    titulo = paste0(indicador, " (", anio, ")"),
    # Corrección de la etiqueta de exportaciones
    subtitulo = if_else(indicador == "Industria", 
                        "Valor agregado per cápita", 
                        "Exportaciones (% PBI en logaritmos)"),
    log_y = TRUE,
    codigo = pais
  )

# 4) Cálculo de Residuos
dispersion <- dispersion |>
  mutate(y = log10(valor), x = log10(pib)) |>
  group_by(titulo) |>
  mutate(residuo = residuals(loess(y ~ x))) |>
  ungroup()

# 5) Resumen agrupado por Asia y Latam
resumen <- dispersion |>
  filter(grupo_region %in% c("América Latina", "Asia Oriental")) |>
  group_by(titulo, grupo_region) |>
  summarise(paises = n(),
            encima = sum(residuo > 0, na.rm = TRUE),
            .groups = "drop") |>
  # Pivoteamos para tener las columnas planas por región
  pivot_wider(names_from = grupo_region, values_from = c(paises, encima)) |>
  rename(
    paises_asia = `paises_Asia Oriental`,
    paises_latam = `paises_América Latina`,
    encima_asia = `encima_Asia Oriental`,
    encima_latam = `encima_América Latina`
  )

orden_paneles <- c("Industria (2001)", "Industria (2008)", "Industria (2022)",
                   "Exportaciones (2001)", "Exportaciones (2008)", "Exportaciones (2022)")

# 6) Paneles
paneles_finales <- dispersion |>
  left_join(resumen, by = "titulo") |>
  # Armamos el texto en 3 renglones
  mutate(sub_final = paste0(subtitulo, 
                            "\nEncima del promedio en Asia: ", encima_asia, " de ", paises_asia,
                            "\nEncima del promedio en Latam: ", encima_latam, " de ", paises_latam)) |>
  split(~ factor(titulo, levels = orden_paneles)) |>
  map(\(d) panel_latam(d))

# 7) Título y renderizado
titulo_final <- paste0(
  "Mientras ", pintar("América Latina", col_latam),
  " se concentra en niveles intermedios,<br>",
  pintar("Asia Oriental", col_asia),
  " combina extremos y una mayor fortaleza exportadora"
)

grafico_final <- juntar(
  paneles_finales,
  titulo = titulo_final,
  subtitulo = paste0("Evolución frente al Índice de Nivel de Precios (eje X logarítmico).\n",
                     "América Latina en azul, Asia Oriental en rojo. Resto del mundo en gris."))

grafico_final


ggsave(file.path("output/graficos", "scater.png"), 
       plot = grafico_final, width = 13, height = 7, units = "in", dpi = 190)




















#Graficos con correcciones




