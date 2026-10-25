#Script Graficos

# install.packages("scales")

library(readxl)
library(ggplot2)
library(tidyverse)
library(scales)

rm(list = ls())

wdi_limpio <- read.csv("output/data/wdi_limpio.csv")

wdi2 <- read.csv("output/data/wdi2.csv")

wdi_final <- read.csv("output/data/wdi_final.csv")

wdi_analisis <- wdi2 |> 
  filter(!is.na(industria_pbi))





wdi_2001 <- wdi2 |>
  filter(anio == 2001)
wdi_2008 <- wdi2 |>
  filter(anio == 2008)
wdi_2022<- wdi2 |>
  filter(anio == 2022)




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

variables <- intersect(variables, names(wdi2))

if (nrow(wdi2) == 0 || length(variables) == 0) {
  stop("No hay observaciones o variables disponibles.")
}

faltantes <- wdi2 %>%
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
    porcentaje = 100 * n_faltantes / nrow(wdi2))

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

#Ver que es distinto a "faltantes"




















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

grafico9 <- ggplot(dist_regiones, aes(x = reorder(region, pct), y = pct)) +
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
