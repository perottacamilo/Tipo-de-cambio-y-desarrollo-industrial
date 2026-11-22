#Script Graficos

#install.packages(c("ggplot2", "modelsummary", "ggrepel", "gt", "webshot2", "patchwork", "ggtext", "scales", "showtext"))

library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(modelsummary)
library(scales)
library(ggtext)
library(ggrepel)
library(showtext)
library(patchwork)
library(webshot2)

rm(list = ls())

wdi_limpio <- read.csv("input/wdi_limpio.csv")

wdi2 <- read.csv("input/wdi2.csv")

wdi_filtrado <- read.csv("input/wdi_filtrado.csv")

wdi_analisis <- wdi2 |> 
  filter(!is.na(industria_per_capita))

wdi_2001 <- wdi2 |> 
  filter(anio == 2001)
wdi_2008 <- wdi2 |> 
  filter(anio == 2008)
wdi_2022 <- wdi2 |> 
  filter(anio == 2022)

#Tabla de variables faltantes por ingreso 
na_por_ingreso <- wdi2 |>
  group_by(ingreso2, anio) |>
  summarise(
    n = n(),
    na_industria_per_capita = sum(is.na(industria_per_capita)),
    pct_na_ind = na_industria_per_capita / n,
    na_price_level = sum(is.na(indice_nivel_precios)),
    pct_na_price_level = na_price_level / n,
    .groups = "drop")

# Pasar a formato largo: una fila por ingreso-año-variable
#Creo la variable ingreso2, que contiene menos palabras para que el grafico quede bien
#La forma anterior esta al filtrado del script pero creo que queda muy cargado de "ingreso" 
na_largo <- na_por_ingreso |>
  select(ingreso2, anio, pct_na_ind, pct_na_price_level) |>
  pivot_longer(
    cols = c(pct_na_ind, pct_na_price_level),
    names_to = "variable",
    values_to = "pct_na") |>
  mutate(variable = recode(variable,
                           pct_na_ind = "industria_per_capita",
                           pct_na_price_level = "indice_nivel_precios"), 
         anio = factor(anio))




#Grafico distribucion industria (log)
grafico2 <- ggplot(wdi_analisis, aes(x = industria_per_capita)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.8) +
  facet_wrap(~ anio, ncol = 1) + # Separa el gráfico por año
  labs(title = "Distribución global de la participación industrial",
       subtitle = "Frecuencia de países según su PBI Industrial",
       x = "PBI Industrial", 
       y = "Cantidad de países") +
  scale_x_log10(labels = scales::comma) +
  theme_minimal()
print(grafico2)

ggsave(file.path("output/graficos", "histograma_industria.png"), plot = grafico2)


summary(lm(industria_per_capita ~ indice_nivel_precios, data = wdi_2022))


#Grafico medianas (1990-2023)
wdi_mediana <- wdi_limpio |> 
  filter(anio %in% 1990:2023)

wdi_latam <- wdi_mediana |> 
  filter(region == "América Latina y el Caribe") |> 
  select(anio, region, indice_nivel_precios)


wdi_asia <- wdi_mediana |> 
  filter(region == "Este de Asia y Pacífico") |> 
  select(anio, region, indice_nivel_precios)

wdi_china <- wdi_mediana |>
  filter(pais == "China",
         anio %in% 1990:2023) |>
  mutate(region = "China") 

wdi_comparativo <- bind_rows(wdi_latam, wdi_asia, wdi_china)

medianas_evolucion <- wdi_comparativo |>
  group_by(anio, region) |>
  summarize(mediana_precios = median(indice_nivel_precios, na.rm = TRUE), 
            .groups = "drop")

grafico4 <- ggplot(medianas_evolucion, aes(x = anio, y = mediana_precios, color = region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("América Latina y el Caribe" = "#D9534F", 
                                "Este de Asia y Pacífico" = "#5BC0DE",
                                "China" = "#27F52A")) +
  scale_x_continuous(limits = c(1990, 2023), breaks = seq(1990, 2023, by = 5)) +
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













library(tidyverse)
library(scales)
library(patchwork)
library(here)

df_2022 <- wdi2 |> 
  filter(anio == 2022) 

# Función auxiliar para no repetir código
hacer_hist <- function(data, col, titulo, usar_log = FALSE, mostrar_y = FALSE) {
  d <- data |>
    select(valor = {{ col }}) |>
    drop_na(valor)
  
  if (usar_log) d <- d |> filter(valor > 0)
  
  stats <- tibble(
    estadistico = c("Media", "Mediana"),
    valor = c(mean(d$valor), median(d$valor))
  )
  
  p <- ggplot(d, aes(x = valor)) +
    geom_histogram(bins = 22, fill = "#2C5F8A", color = "white", alpha = 0.85) +
    geom_vline(
      data = stats,
      aes(xintercept = valor, color = estadistico, linetype = estadistico),
      linewidth = 0.8
    ) +
    scale_color_manual(values = c("Media" = "#C0392B", "Mediana" = "#1E8449"), name = NULL) +
    scale_linetype_manual(values = c("Media" = "solid", "Mediana" = "dashed"), name = NULL) +
    labs(
      title = titulo,
      x = NULL,
      y = if (mostrar_y) "Cantidad de países" else NULL
    ) +
    theme_minimal(base_size = 11.5) +
    theme(
      plot.title = element_text(face = "bold", size = 10.5, hjust = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  if (usar_log) {
    p <- p + scale_x_log10(labels = label_number(big.mark = ".", decimal.mark = ","))
  } else {
    p <- p + scale_x_continuous(labels = label_number(big.mark = ".", decimal.mark = ","))
  }
  
  return(p)
}

# Armar los 3 paneles
p1 <- hacer_hist(
  df_2022, industria_per_capita,
  "PBI industrial per cápita\n(USD const. 2015, escala log.)",
  usar_log = TRUE, mostrar_y = TRUE) + 
    theme(
      plot.title = element_text(face = "bold", size = 22, hjust = 0.5), # Título más grande
      axis.text = element_text(size = 18),                              # Números de los ejes más grandes
      axis.title = element_text(size = 19),                             # Nombre del eje Y más grande
      legend.position = "none",                                         # Saca la leyenda de Media/Mediana
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
print(p1)
p2 <- hacer_hist(
  df_2022, exportacion_manufactura_pc,
  "Exportaciones manuf. per cápita\n(USD corr., escala log.)",
  usar_log = TRUE
) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5), # Título más grande
    axis.text = element_text(size = 18),                              # Números de los ejes más grandes
    axis.title = element_text(size = 19),                             # Nombre del eje Y más grande
    legend.position = "none",                                         # Saca la leyenda de Media/Mediana
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

p3 <- hacer_hist(
  df_2022, indice_nivel_precios,
  "Índice de nivel de precios\n(EE. UU. = 100)",
  usar_log = FALSE
) + 
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5), # Título más grande
    axis.text = element_text(size = 18),                              # Números de los ejes más grandes
    axis.title = element_text(size = 19),                             # Nombre del eje Y más grande
    legend.position = "none",                                         # Saca la leyenda de Media/Mediana
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Unir todo en una sola figura con leyenda compartida
g_hist_2022 <- (p1 + p2 + p3) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Distribución de las variables principales en 2022",
    subtitle = "Países con más de 1 millón de habitantes. Líneas verticales indican media y mediana.",
    caption = "Fuente: elaboración propia en base a Banco Mundial (WDI) y Argendata (Fundar).",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(color = "grey30", size = 11),
      plot.caption = element_text(color = "grey45", hjust = 0, size = 9)
    )
  ) &
  theme(legend.position = "top", legend.justification = "left")



# 4. Guardar con ruta relativa y tamaño apto para diapositiva
ggsave(file.path("output/graficos/eda", "eda.png"), 
       plot = g_hist_2022, width = 12, height = 7, units = "in", dpi = 300)





ggsave(file.path("output/graficos/eda", "eda1.png"), 
       plot = p1, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos/eda", "eda2.png"), 
       plot = p2, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos/eda", "eda3.png"), 
       plot = p3, width = 12, height = 7, units = "in", dpi = 300)
























# cor.test() agrega el test de significatividad y el intervalo de confianza
cor.test(wdi_2001$industria_per_capita, wdi_2001$indice_nivel_precios)
#La correlacion es significativa al 1%









alta  <- wdi_2001$industria_per_capita[wdi_2001$region == "Este de Asia y Pacífico"]
baja  <- wdi_2001$industria_per_capita[wdi_2001$region %in% c("América Latina y el Caribe", "")]

png("output/graficos/histograma_industria_regiones_2001.png", width = 8, height = 6, units = "in", res = 300)

hist(baja, breaks = 15, col = rgb(1, 0, 0, 0.5), border = "white",
     main = "2001",
     xlab = "PBI Industrial", ylab = "Cantidad de países")
hist(alta, breaks = 15, col = rgb(0, 0, 1, 0.5), border = "white", add = TRUE)

dev.off()





alta1  <- wdi_2008$industria_per_capita[wdi_2008$region == "Este de Asia y Pacífico"]
baja1  <- wdi_2008$industria_per_capita[wdi_2008$region %in% c("América Latina y el Caribe", "")]

png("output/graficos/histograma_industria_regiones_2008.png", width = 8, height = 6, units = "in", res = 300)

hist(baja1, breaks = 15, col = rgb(1, 0, 0, 0.5), border = "white",
     main = "2008",
     xlab = "PBI Industrial", ylab = "Cantidad de países")
hist(alta1, breaks = 15, col = rgb(0, 0, 1, 0.5), border = "white", add = TRUE)

dev.off()









alta2  <- wdi_2022$industria_per_capita[wdi_2022$region == "Este de Asia y Pacífico"]
baja2  <- wdi_2022$industria_per_capita[wdi_2022$region %in% c("América Latina y el Caribe", "")]

png("output/graficos/histograma_industria_regiones_2022.png", width = 8, height = 6, units = "in", res = 300)

hist(baja2, breaks = 15, col = rgb(1, 0, 0, 0.5), border = "white",
     main = "2022",
     xlab = "PBI Industrial", ylab = "Cantidad de países")
hist(alta2, breaks = 15, col = rgb(0, 0, 1, 0.5), border = "white", add = TRUE)

dev.off()



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
  "pbi_millones")




wdi_2 <- wdi2 |> 
  filter(poblacion_mill > 1)

variables <- intersect(variables, names(wdi_2))

if (nrow(wdi_2) == 0 || length(variables) == 0) {
  stop("No hay observaciones o variables disponibles.")}

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

ggsave(file.path("output/graficos/eda", "datos_faltanes_vars.png"), 
       plot = grafico5, width = 12, height = 7, units = "in", dpi = 300)













filtrado1 <- wdi2 %>%
  count(region) %>%
  mutate(porcentaje = (n / sum(n)) * 100,
         base = "Antes de filtrar") 

filtrado2 <- wdi_filtrado %>%
  count(region) %>%
  mutate(porcentaje = (n / sum(n)) * 100,
         base = "Base filtrada") 

# 3. Unir ambos dataframes verticalmente
datos_combinados <- bind_rows(filtrado1, filtrado2)

# 4. Graficar comparando ambas bases
grafico10 <- ggplot(datos_combinados, aes(x = reorder(region, -porcentaje), y = porcentaje, fill = base)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
    labs(title = "Distribución global por región",
       subtitle = "Porcentaje de países en cada categoría sobre el total",
       x = "Región",
       y = "Porcentaje del total de países",
       fill = "Muestra") + 
  theme_minimal(base_size = 14) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13), # Nombres de las regiones (eje X)
    axis.text.y = element_text(size = 13),                        # Números del porcentaje (eje Y)
    axis.title = element_text(size = 15, face = "bold"),          # Títulos "Región" y "Porcentaje..."
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  )

print(grafico10)





ingreso1 <- wdi2 %>%
  count(ingreso2) %>%
  mutate(porcentaje = (n / sum(n)) * 100,
         base = "Antes de filtrar") 

ingreso_2 <- wdi_filtrado %>%
  count(ingreso2) %>%
  mutate(porcentaje = (n / sum(n)) * 100,
         base = "Base filtrada") 

datos_combinados <- bind_rows(ingreso1, ingreso_2)

grafico11 <- ggplot(datos_combinados, aes(x = reorder(ingreso2, -porcentaje), y = porcentaje, fill = base)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") + 
  labs(title = "Distribución de los ingresos",
       subtitle = "Porcentaje de países en cada categoría sobre el total",
       x = "Ingreso",
       y = "Porcentaje del total de países",
       fill = "Muestra") + 
  theme_minimal(base_size = 14) + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y = element_text(size = 13),
    axis.title = element_text(size = 15, face = "bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)
  )

print(grafico11)

ggsave(file.path("output/graficos/eda", "variacion_region.png"), 
       plot = grafico10, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos/eda", "variacion_ingreso.png"), 
       plot = grafico11, width = 12, height = 7, units = "in", dpi = 300)





















# ==============================================================================
# SCRIPT MAESTRO: GRÁFICO EXPLORATORIO Y COMUNICATIVO
# ==============================================================================

library(tidyverse)
library(scales)
library(ggtext)
library(ggrepel)
library(showtext)
library(patchwork)
rm(list = ls())
# ------------------------------------------------------------------------------
# 1. CONFIGURACIÓN DEL ENTORNO Y TIPOGRAFÍAS (Unificado para todo el script)
# ------------------------------------------------------------------------------
fuentes_ok <- tryCatch({
  font_add_google("Playfair Display", "playfair", regular.wt = 600, bold.wt = 700)
  font_add_google("Lato", "lato", regular.wt = 400, bold.wt = 700)
  TRUE
}, error = function(e) FALSE)

fuente_titulo <- if (fuentes_ok) "playfair" else "serif"
fuente_texto  <- if (fuentes_ok) "lato" else "sans"

# Activamos un único motor tipográfico para evitar conflictos de resolución
showtext_auto()
showtext_opts(dpi = 96)

# Paletas globales
col <- c(rojo = "#B13507", azul = "#286BBB", gris = "#BDBDBD")
col_asia  <- "#E0195B"
col_latam <- "#3919E0"
gris_titulo <- "#363636"
gris_texto  <- "#5B5B5B"
gris_suave  <- "#858585"

fmt_num <- label_number(big.mark = ".", decimal.mark = ",", drop0trailing = TRUE)
base_indice <- "EE. UU. = 100"

# Tema base OWID
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

# Funciones de ploteo para el exploratorio
panel_latam <- function(datos, subtitulo = datos$sub_filtrado[1]) {
  ind <- datos$indicador[1]
  resto <- filter(datos, grupo_region == "Resto del Mundo")
  foco  <- filter(datos, grupo_region != "Resto del Mundo")
  destacar <- c("Argentina", "China")
  
  tit_y <- if (ind == "Industria") "USD (escala log)" else "% del PBI (escala log)"
  tit_x <- if (ind == "Exportaciones") "Índice de nivel de precios (escala log)" else NULL
  
  ggplot(datos, aes(pib, valor)) +
    geom_point(data = resto, shape = 21, size = 1.1, stroke = 0.25,
               color = "#9E9E9E", fill = alpha(col[["gris"]], 0.55)) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                color = gris_titulo, linewidth = 0.5) +
    geom_point(data = foco, aes(fill = grupo_region), shape = 21, size = 1.7,
               stroke = 0.4, color = "white") +
    geom_label_repel(data = filter(datos, codigo %in% destacar), aes(label = codigo),
                     family = fuente_texto, fontface = "bold", size = 5,
                     fill = alpha("white", 0.85), label.size = 0, label.padding = unit(1.5, "pt"),
                     color = gris_titulo, min.segment.length = 0, segment.color = "#777777",
                     nudge_y = 0.3, seed = 1) +
    scale_fill_manual(values = c("América Latina" = col_latam, "Asia Oriental" = col_asia)) +
    scale_y_log10(labels = fmt_num, expand = expansion(mult = c(0.1, 0.35))) +
    scale_x_log10(breaks = c(30, 50, 100), labels = fmt_num) +
    coord_cartesian(ylim = if (ind == "Exportaciones") c(1, NA) else NULL) +
    labs(title = datos$titulo[1], subtitle = subtitulo, x = tit_x, y = tit_y) +
    theme_owid(base_size = 9.5) %+replace%
    theme(
      plot.title    = element_text(family = fuente_texto, face = "bold", size = rel(3.5),
                                   color = gris_titulo, hjust = 0, margin = margin(b = 3)),
      plot.subtitle = element_text(size = rel(2), color = gris_texto, hjust = 0,
                                   lineheight = 0.6, margin = margin(b = 14)),
      axis.title.x  = element_text(size = rel(1.75), color = gris_texto, margin = margin(t = 6)),
      axis.title.y  = element_text(size = rel(1.75), color = gris_texto, angle = 90, margin = margin(r = 6)),
      panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      axis.line.x = element_blank(), axis.ticks.x = element_blank(),
      plot.background = element_rect(fill = "#FAFAFA", color = "#E3E3E3"),
      plot.margin = margin(8, 10, 8, 8)
    )
}

juntar <- function(paneles, titulo, subtitulo, caption) {
  wrap_plots(paneles, ncol = 3) +
    plot_annotation(
      title = titulo, 
      subtitle = subtitulo, 
      caption = caption, 
      theme = theme_owid() +
        theme(
          plot.title    = ggtext::element_markdown(family = fuente_texto, face = "bold",
                                                   size = 45, hjust = 0, halign = 0,
                                                   lineheight = 0.4, margin = margin(b = 6)),
          plot.subtitle = ggtext::element_markdown(size = 30, hjust = 0, halign = 0,
                                                   lineheight = 0.35, margin = margin(b = 12)),
          plot.caption  = element_text(size = 18, hjust = 0, lineheight = 0.4, 
                                       color = gris_texto, margin = margin(t = 10)),
          plot.margin   = margin(15, 15, 10, 15)
        )
    )
}

# ------------------------------------------------------------------------------
# 2. PROCESAMIENTO Y GRÁFICO EXPLORATORIO
# ------------------------------------------------------------------------------
wdi_filtrado <- read.csv("input/wdi_filtrado.csv")

# CORRECCIÓN METODOLÓGICA: if_else para aislar China sin saltos espurios
wdi_filtrado_2 <- wdi_filtrado |> 
  mutate(industria_pc_wdi = (industria_per_capita * pbi_per_capita) / 100,
         industria_per_capita = if_else(pais == "China", industria_pc, industria_pc_wdi))

dispersion <- wdi_filtrado_2 |>
  filter(anio %in% c(2001, 2008, 2022)) |>
  filter(!is.na(indice_nivel_precios), region != "Aggregates") |>
  mutate(
    grupo_region = case_when(
      region == "América Latina y el Caribe" ~ "América Latina",
      region == "Este de Asia y Pacífico" ~ "Asia Oriental",
      TRUE ~ "Resto del Mundo"
    )
  ) |>
  select(pais, grupo_region, anio, pib = indice_nivel_precios,
         Industria = industria_per_capita,
         Exportaciones = exportacion_manufactura_pc) |>
  pivot_longer(c(Industria, Exportaciones), names_to = "indicador", values_to = "valor") |>
  filter(!is.na(valor), valor > 0, !is.na(pib)) |>
  mutate(
    titulo = paste0(indicador, " (", anio, ")"),
    subtitulo = if_else(indicador == "Industria", "Valor agregado per cápita", "Exportaciones per cápita (USD)"),
    log_y = TRUE, codigo = pais
  )

dispersion <- dispersion |>
  mutate(y = log10(valor), x = log10(pib)) |>
  group_by(titulo) |>
  mutate(residuo = residuals(loess(y ~ x))) |>
  ungroup()

resumen <- dispersion |>
  filter(grupo_region %in% c("América Latina", "Asia Oriental")) |>
  group_by(titulo, grupo_region) |>
  summarise(paises = n(), encima = sum(residuo > 0, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = grupo_region, values_from = c(paises, encima)) |>
  rename(paises_asia = `paises_Asia Oriental`, paises_latam = `paises_América Latina`,
         encima_asia = `encima_Asia Oriental`, encima_latam = `encima_América Latina`)

orden_paneles <- c("Industria (2001)", "Industria (2008)", "Industria (2022)",
                   "Exportaciones (2001)", "Exportaciones (2008)", "Exportaciones (2022)")

paneles_filtradoes <- dispersion |>
  left_join(resumen, by = "titulo") |>
  mutate(sub_filtrado = paste0(subtitulo,
                               "\nPor encima de la curva mundial. Asia: ", encima_asia, " de ", paises_asia,
                               " · Latam: ", encima_latam, " de ", paises_latam)) |>
  split(~ factor(titulo, levels = orden_paneles)) |>
  purrr::map(\(d) panel_latam(d))

titulo_filtrado <- paste0("Industria y Exportaciones según el Nivel de Precios:<br>",
                          pintar("Asia Oriental", col_asia), " y ", pintar("América Latina", col_latam),
                          " frente al resto del mundo, 2001, 2008 y 2022")

subtitulo_filtrado <- "Los paises de Asia presentan una gran dispersion y una fuerte capacidad exportadora, <br>mientras que los latinoamericanos se concentran en torno a los promedios. <br>Evolución frente al Índice de Nivel de Precios (ambos ejes en escala logarítmica)."

caption_filtrado <- paste0("Fuente: elaboración propia en base a Banco Mundial (WDI) y Schteingart, D. y Sidicaro, N. (2025). Industria. Argendata. Fundar.\n",
                           "Notas: cada punto es un país; resto del mundo en gris. La curva es la tendencia estimada con todos los países. Industria: valor agregado industrial per cápita en USD constantes 2015. Índice de nivel de precios: ", base_indice, ".\n",
                           "Algunos países de Asia carecen de datos en ciertos años y no aparecen en los tres gráficos. Otros paises como Venezuela o Corea del Norte no aparecen por falta de datos en los tres años.")

grafico_exploratorio_2 <- juntar(paneles_filtradoes, titulo = titulo_filtrado, subtitulo = subtitulo_filtrado, caption = caption_filtrado)

ggsave("grupo_01_exploratorio.jpg", plot = grafico_exploratorio_2,
       width = 22, height = 14, units = "cm", dpi = 300)

# ------------------------------------------------------------------------------
# 3. PROCESAMIENTO Y GRÁFICO COMUNICATIVO
# ------------------------------------------------------------------------------
wdi_limpio <- read.csv("input/wdi_limpio.csv")

# CORRECCIÓN METODOLÓGICA: if_else para evitar saltos en la serie de tiempo
wdi_limpio_2 <- wdi_limpio |> 
  filter(region %in% c("Este de Asia y Pacífico", "América Latina y el Caribe"), poblacion_mill > 1, anio >= 1990, 
         !pais %in% c("Cuba", "Korea, Dem. People's Rep.", "Venezuela, RB")) |> 
  select(pais, anio, industria_per_capita, exportacion_manufactura_pc, region, pbi_per_capita, indice_nivel_precios, industria_pc) |>
  mutate(industria_pc_wdi = (industria_per_capita * pbi_per_capita) / 100,
         industria_per_capita = if_else(pais == "China", industria_pc, industria_pc_wdi)) |> 
  arrange(pais, anio) 

k <- 1.25
col_personalizados <- c("Asia Oriental y Pacífico" = "#E0195B", "América Latina y el Caribe" = "#3919E0", "Argentina" = "#19E09E", "China" = "#FFAC47")

df_eap <- wdi_limpio_2 %>% filter(region == "Este de Asia y Pacífico") %>% group_by(anio) %>% summarise(industria_per_capita = mean(industria_per_capita, na.rm = TRUE), .groups = "drop") %>% mutate(pais = "Asia Oriental y Pacífico") %>% select(year = anio, pais, industria_per_capita)
df_lac <- wdi_limpio_2 %>% filter(region == "América Latina y el Caribe") %>% group_by(anio) %>% summarise(industria_per_capita = mean(industria_per_capita, na.rm = TRUE), .groups = "drop") %>% mutate(pais = "América Latina y el Caribe") %>% select(year = anio, pais, industria_per_capita)
df_paises <- wdi_limpio_2 %>% filter(pais %in% c("Argentina", "China")) %>% mutate(pais = if_else(pais == "China", "China", "Argentina")) %>% select(year = anio, pais, industria_per_capita)

df_industrializacion <- bind_rows(df_eap, df_lac, df_paises) %>% filter(year >= 1990, year <= 2024)
ult_ind_pc <- df_industrializacion %>% group_by(pais) %>% filter(year == max(year)) %>% ungroup() %>% mutate(etiqueta = str_wrap(pais, 14))

g_ind_pc_esp <- ggplot(df_industrializacion, aes(x = year, y = industria_per_capita, color = pais, group = pais)) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(data = ult_ind_pc, size = 3) +
  geom_text(data = ult_ind_pc, aes(x = 2025, label = etiqueta), hjust = 0, lineheight = 0.3, fontface = "bold", size = 7 * k, show.legend = FALSE) +
  geom_vline(xintercept = 2001, linetype = "dotted", color = "grey60") +
  geom_richtext(data = data.frame(x = 2001.25, y = max(df_industrializacion$industria_per_capita, na.rm = TRUE) * 0.08),
                aes(x = x, y = y, label = "<b>2001: Ingreso de <span style='color:#ffac47'>China</span><br>a la OMC</b>"),
                hjust = 0, size = 6 * k, color = "grey35", lineheight = 0.3, fill = NA, label.color = NA, inherit.aes = FALSE) +
  annotate(geom = "curve", x = 2002.5, y = 350, xend = 2001, yend = 548.45, curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "grey60") +
  annotate(geom = "curve", x = 2009.5, y = 3100, xend = 2008, yend = 3100, curvature = 0.1, arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +
  annotate(geom = "text", x = 2010, y = 3100, label = "Crisis del 2008", hjust = 0, size = 6 * k, color = "black", lineheight = 0.3) +
  scale_color_manual(values = col_personalizados) +
  scale_x_continuous(breaks = c(seq(1990, 2020, by = 10), 2024), limits = c(1990, 2032), expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(labels = label_dollar(prefix = "$", big.mark = ".", decimal.mark = ","), limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Desde 1990 <span style='color:#ffac47'>China</span> aumentó 21 veces su producción industrial por habitante,<br>superando al promedio de <span style='color:#E0195B'>Asia</span> y <span style='color:#3919E0'>América Latina</span>.<br><span style='color:#19E09E'>Argentina</span> perdió la ventaja que tenía sobre <span style='color:#E0195B'>Asia</span>.",
    subtitle = "Evolución de la Producción Industrial por Habitante (USD constantes de 2015): 1990-2024",
    caption  = "Fuente: Elaboración propia en base a WDI (Banco Mundial) y Schteingart, D. y Sidicaro, N. (2025). Industria. Argendata. Fundar.",
    x = "Año", y = "Producción Industrial por Habitante"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position        = "none",
    plot.title.position    = "plot",
    plot.title             = element_textbox_simple(family = "serif", face = "bold", size = 40 * k, lineheight = 0.33, margin = margin(b = 6)),
    plot.subtitle          = element_text(color = "grey35", margin = margin(b = 10), size = 26),
    plot.caption           = element_text(color = "grey45", hjust = 0, size = 15 * k),
    panel.grid.minor       = element_blank(),
    panel.grid.major.x     = element_blank(),
    panel.grid.major.y     = element_line(color = "grey92", linetype = "dashed", linewidth = 0.3),
    axis.text              = element_text(color = "grey40", size = 25),
    axis.ticks.x           = element_line(color = "grey60"),
    plot.margin            = margin(12, 50, 10, 12),
    axis.title             = element_text(color = "grey35", size = 20),
  )

# CORRECCIÓN DE DISPOSITIVO: Eliminado device = ragg::agg_jpeg para evitar conflicto con showtext
ggsave("grupo_01_comunicativolalalal.jpg", plot = g_ind_pc_esp,
       width = 22, height = 14, units = "cm", dpi = 300)















# ==============================================================================
# SCRIPT MAESTRO: GRÁFICO EXPLORATORIO Y COMUNICATIVO
# ==============================================================================
library(tidyverse)
library(scales)
library(ggtext)
library(ggrepel)
library(showtext)
library(patchwork)
rm(list = ls())
# ------------------------------------------------------------------------------
# 1. CONFIGURACIÓN DEL ENTORNO Y TIPOGRAFÍAS (Unificado para todo el script)
# ------------------------------------------------------------------------------
fuentes_ok <- tryCatch({
  font_add_google("Playfair Display", "playfair", regular.wt = 600, bold.wt = 700)
  font_add_google("Lato", "lato", regular.wt = 400, bold.wt = 700)
  TRUE
}, error = function(e) FALSE)

fuente_titulo <- if (fuentes_ok) "playfair" else "serif"
fuente_texto  <- if (fuentes_ok) "lato" else "sans"

# Activamos un único motor tipográfico para evitar conflictos de resolución
showtext_auto()
showtext_opts(dpi = 96)

# Paletas globales
col <- c(rojo = "#B13507", azul = "#286BBB", gris = "#BDBDBD")
col_asia  <- "#E0195B"
col_latam <- "#3919E0"
gris_titulo <- "#363636"
gris_texto  <- "#5B5B5B"
gris_suave  <- "#858585"

fmt_num <- label_number(big.mark = ".", decimal.mark = ",", drop0trailing = TRUE)
fmt_pct <- label_number(suffix = "%", big.mark = ".", decimal.mark = ",", drop0trailing = TRUE)
base_indice <- "EE. UU. = 100"

# Tema base OWID
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

# Funciones de ploteo para el exploratorio
panel_latam <- function(datos, subtitulo = datos$sub_filtrado[1]) {
  ind <- datos$indicador[1]
  resto <- filter(datos, grupo_region == "Resto del Mundo")
  foco  <- filter(datos, grupo_region != "Resto del Mundo")
  destacar <- c("Argentina", "China")
  
  tit_y <- if (ind == "Industria") "USD por habitante (escala log)" else "% de las exportaciones"
  tit_x <- if (ind == "Exportaciones") " Índice de nivel de precios (EE. UU. = 100, escala log)" else NULL
  
  ggplot(datos, aes(pib, valor)) +
    geom_point(data = resto, shape = 21, size = 1.1, stroke = 0.25,
               color = "#9E9E9E", fill = alpha(col[["gris"]], 0.55)) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                color = gris_titulo, linewidth = 0.5) +
    geom_point(data = foco, aes(fill = grupo_region), shape = 21, size = 1.7,
               stroke = 0.4, color = "white") +
    geom_label_repel(data = filter(datos, codigo %in% destacar), aes(label = codigo),
                     family = fuente_texto, fontface = "bold", size = 5,
                     fill = alpha("white", 0.85), label.size = 0, label.padding = unit(1.5, "pt"),
                     color = gris_titulo, min.segment.length = 0, segment.color = "#777777",
                     nudge_y = 0.3, seed = 1) +
    scale_fill_manual(values = c("América Latina" = col_latam, "Asia Oriental" = col_asia)) +
    (if (ind == "Industria") {
      scale_y_log10(labels = fmt_num, expand = expansion(mult = c(0.1, 0.15)))
    } else {
      scale_y_continuous(labels = fmt_pct, breaks = c(0, 25, 50, 75, 100), limits = c(0, 100))
    }) +
    scale_x_log10(breaks = c(30, 50, 100), labels = fmt_num) +
    (if (ind == "Exportaciones") coord_cartesian(ylim = c(0, 100)) else NULL) +
    scale_x_log10(breaks = c(30, 50, 100), labels = fmt_num) +    
    scale_x_log10(breaks = c(30, 50, 100), labels = fmt_num) +    scale_x_log10(breaks = c(30, 50, 100), labels = fmt_num) +
    coord_cartesian(ylim = if (ind == "Exportaciones") c(1, NA) else NULL) +
    labs(title = datos$titulo[1], subtitle = subtitulo, x = tit_x, y = tit_y) +
    theme_owid(base_size = 9.5) %+replace%
    theme(
      plot.title    = element_text(family = fuente_texto, face = "bold", size = rel(3.5),
                                   color = gris_titulo, hjust = 0, margin = margin(b = 3)),
      plot.subtitle = element_text(size = rel(2), color = gris_texto, hjust = 0,
                                   lineheight = 0.6, margin = margin(b = 14)),
      axis.title.x  = element_text(size = rel(1.75), color = gris_texto, margin = margin(t = 6)),
      axis.title.y  = element_text(size = rel(1.75), color = gris_texto, angle = 90, margin = margin(r = 6)),
      panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
      axis.line.x = element_blank(), axis.ticks.x = element_blank(),
      plot.background = element_rect(fill = "#FAFAFA", color = "#E3E3E3"),
      plot.margin = margin(8, 10, 8, 8)
    )
}

juntar <- function(paneles, titulo, subtitulo, caption) {
  wrap_plots(paneles, ncol = 3) +
    plot_annotation(
      title = titulo, 
      subtitle = subtitulo, 
      caption = caption, 
      theme = theme_owid() +
        theme(
          plot.title    = ggtext::element_markdown(family = fuente_texto, face = "bold",
                                                   size = 45, hjust = 0, halign = 0,
                                                   lineheight = 0.4, margin = margin(b = 6)),
          plot.subtitle = ggtext::element_markdown(size = 30, hjust = 0, halign = 0,
                                                   lineheight = 0.35, margin = margin(b = 12)),
          plot.caption  = element_text(size = 18, hjust = 0, lineheight = 0.4, 
                                       color = gris_texto, margin = margin(t = 10)),
          plot.margin   = margin(5, 5, 5, 5)
        )
    )
}

# ------------------------------------------------------------------------------
# 2. PROCESAMIENTO Y GRÁFICO EXPLORATORIO
# ------------------------------------------------------------------------------
wdi_filtrado <- read.csv("input/wdi_filtrado.csv")

# CORRECCIÓN METODOLÓGICA: if_else para aislar China sin saltos espurios
wdi_filtrado_2 <- wdi_filtrado |> 
  mutate(industria_pc_wdi = (industria_pbi * pbi_per_capita) / 100,
         industria_per_capita = coalesce(industria_pc, industria_pc_wdi))

dispersion <- wdi_filtrado_2 |>
  filter(anio %in% c(2001, 2008, 2022)) |>
  filter(!is.na(indice_nivel_precios), region != "Aggregates") |>
  mutate(
    grupo_region = case_when(
      region == "América Latina y el Caribe" ~ "América Latina",
      region == "Este de Asia y Pacífico" ~ "Asia Oriental",
      TRUE ~ "Resto del Mundo"
    )
  ) |>
  select(pais, grupo_region, anio, pib = indice_nivel_precios,
         Industria = industria_per_capita,
         Exportaciones = exportacion_manufactura_pc) |>
  pivot_longer(c(Industria, Exportaciones), names_to = "indicador", values_to = "valor") |>
  filter(!is.na(valor), valor > 0, !is.na(pib)) |>
  mutate(
    titulo = paste0(indicador, " (", anio, ")"),
    subtitulo = if_else(indicador == "Industria", "Industria per cápita", "Exportaciones per cápita (USD)"),
    log_y = TRUE, codigo = pais
  )

dispersion <- dispersion |>
  mutate(
    # Industria sigue en logaritmo, Exportaciones pasa a lineal
    y = if_else(indicador == "Industria", log10(valor), valor), 
    x = log10(pib)
  ) |>
  group_by(titulo) |>
  mutate(residuo = residuals(loess(y ~ x))) |>
  ungroup()

resumen <- dispersion |>
  filter(grupo_region %in% c("América Latina", "Asia Oriental")) |>
  group_by(titulo, grupo_region) |>
  summarise(paises = n(), encima = sum(residuo > 0, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = grupo_region, values_from = c(paises, encima)) |>
  rename(paises_asia = `paises_Asia Oriental`, paises_latam = `paises_América Latina`,
         encima_asia = `encima_Asia Oriental`, encima_latam = `encima_América Latina`)

orden_paneles <- c("Industria (2001)", "Industria (2008)", "Industria (2022)",
                   "Exportaciones (2001)", "Exportaciones (2008)", "Exportaciones (2022)")

# 6) Paneles: Dejamos únicamente el conteo de países por encima de la curva
paneles_filtradoes <- dispersion |>
  left_join(resumen, by = "titulo") |>
  mutate(
    # Eliminamos 'subtitulo' y el '\n' inicial para que arranque directo
    sub_filtrado = paste0("Por encima de la curva mundial. Asia: ", encima_asia, " de ", paises_asia,
                          " · Latam: ", encima_latam, " de ", paises_latam)
  ) |>
  split(~ factor(titulo, levels = orden_paneles)) |>
  purrr::map(\(d) panel_latam(d))

titulo_filtrado <- paste0("PBI Industrial y Exportaciones según el Nivel de Precios:<br>",
                          pintar("Asia Oriental", col_asia), " y ", pintar("América Latina", col_latam),
                          " frente al resto del mundo, 2001, 2008 y 2022.")

subtitulo_filtrado <- "Cada punto es un país; América Latina en azul y Asia en rojo, resto del mundo en gris. La curva es la tendencia estimada con todos los países.  
Los países por encima de la curva tienen un valor más alto que el esperado dado su nivel de precios.
<br>PBI Industrial per cápita, en USD constantes de 2015. Exportaciones: manufacturas como % de las exportaciones de mercancías."

caption_filtrado <- paste0(
  "Fuente: elaboración propia en base a Banco Mundial (WDI) y Schteingart, D. y Sidicaro, N. (2025). Industria. Argendata. Fundar.\n",
  "Notas: Industria: datos de Argendata, completados con WDI. ",
  "Se excluyen países con menos de 1 millón de habitantes, y no figuran países sin datos en el año."
)  
grafico_exploratorio <- juntar(paneles_filtradoes, titulo = titulo_filtrado, subtitulo = subtitulo_filtrado, caption = caption_filtrado)

# CORRECCIÓN DE DISPOSITIVO: Eliminado device = ragg::agg_jpeg para evitar conflicto con showtext
ggsave("output/graficos/grupo_01_exploratorio.jpg", plot = grafico_exploratorio,
       width = 22, height = 14, units = "cm", dpi = 300)
# ------------------------------------------------------------------------------
# 3. PROCESAMIENTO Y GRÁFICO COMUNICATIVO
# ------------------------------------------------------------------------------
rm(list = ls())
wdi_limpio <- read.csv("input/wdi_limpio.csv")

# CORRECCIÓN METODOLÓGICA: if_else para evitar saltos en la serie de tiempo
wdi_limpio_2 <- wdi_limpio |> 
  filter(region %in% c("Este de Asia y Pacífico", "América Latina y el Caribe"), poblacion_mill > 1, anio >= 1990 & anio <= 2023, 
         !pais %in% c("Cuba", "Korea, Dem. People's Rep.", "Venezuela, RB")) |> 
  select(pais, anio, industria_pbi, exportacion_manufactura_pc, region, pbi_per_capita, indice_nivel_precios, industria_pc) |>
  mutate(industria_pc_wdi = (industria_pbi * pbi_per_capita) / 100,
         industria_per_capita = coalesce(industria_pc, industria_pc_wdi)) |>   arrange(pais, anio) 

fuentes_ok <- tryCatch({
  font_add_google("Playfair Display", "playfair", regular.wt = 600, bold.wt = 700)
  font_add_google("Lato", "lato", regular.wt = 400, bold.wt = 700)
  TRUE
}, error = function(e) FALSE)

fuente_titulo <- if (fuentes_ok) "playfair" else "serif"
fuente_texto  <- if (fuentes_ok) "lato" else "sans"


k <- 1.25
col_personalizados <- c("Asia Oriental y Pacífico" = "#E0195B", "América Latina y el Caribe" = "#3919E0", "Argentina" = "#19E09E", "China" = "#FFAC47")

df_eap <- wdi_limpio_2 %>% filter(region == "Este de Asia y Pacífico") %>% group_by(anio) %>% summarise(industria_per_capita = mean(industria_per_capita, na.rm = TRUE), .groups = "drop") %>% mutate(pais = "Asia Oriental y Pacífico") %>% select(year = anio, pais, industria_per_capita)
df_lac <- wdi_limpio_2 %>% filter(region == "América Latina y el Caribe") %>% group_by(anio) %>% summarise(industria_per_capita = mean(industria_per_capita, na.rm = TRUE), .groups = "drop") %>% mutate(pais = "América Latina y el Caribe") %>% select(year = anio, pais, industria_per_capita)
df_paises <- wdi_limpio_2 %>% filter(pais %in% c("Argentina", "China")) %>% mutate(pais = if_else(pais == "China", "China", "Argentina")) %>% select(year = anio, pais, industria_per_capita)

df_industrializacion <- bind_rows(df_eap, df_lac, df_paises) %>% filter(year >= 1990, year <= 2023)
ult_ind_pc <- df_industrializacion %>% group_by(pais) %>% filter(year == max(year)) %>% ungroup() %>% mutate(etiqueta = str_wrap(pais, 14))

g_ind_pc_esp <- ggplot(df_industrializacion, aes(x = year, y = industria_per_capita, color = pais, group = pais)) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(data = ult_ind_pc, size = 3) +
  geom_text(data = ult_ind_pc, aes(x = 2023.7, label = etiqueta), hjust = 0, lineheight = 0.3, fontface = "bold", size = 8 * k, show.legend = FALSE) +
  geom_vline(xintercept = 2001, linetype = "dotted", color = "grey60") +
  geom_richtext(data = data.frame(x = 2001.25, y = max(df_industrializacion$industria_per_capita, na.rm = TRUE) * 0.06),
                aes(x = x, y = y, label = "<b>2001: Ingreso de <span style='color:#ffac47'>China</span><br>a la OMC</b>", family = fuente_texto),
                hjust = 0, size = 6 * k, color = "grey35", lineheight = 0.3, fill = NA, label.color = NA, inherit.aes = FALSE) +
  annotate(geom = "curve", x = 2002.5, y = 350, xend = 2001, yend = 548.45, curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "grey60") +
  annotate(geom = "curve", x = 2009.5, y = 3400, xend = 2008, yend = 3400, curvature = 0.1, arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +
  annotate(geom = "text", x = 2010, y = 3400, label = "Crisis del 2008", hjust = 0, size = 6 * k, lineheight = 0.3, color = "grey35",       # <-- IGUALAMOS EL COLOR GRIS
           family = fuente_texto,  # <-- FORZAMOS LA MISMA FUENTE
           fontface = "bold"       # <-- IGUALAMOS LA NEGRITA DEL HTML
  ) +
  scale_color_manual(values = col_personalizados) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2023), limits = c(1990, 2028.5), expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(labels = label_dollar(prefix = "$", big.mark = ".", decimal.mark = ","), limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 15) +
  labs(
    title = "Desde 1990 <span style='color:#ffac47'>China</span> aumentó 22 veces su PBI Industrial por habitante,<br>superando al promedio de <span style='color:#E0195B'>Asia Oriental</span> y <span style='color:#3919E0'>América Latina</span>.",
    
    # Agrupamos la frase de Argentina y la técnica en el subtítulo separadas por un <br>
    # Ajustá los valores de font-size (ej. 24pt y 18pt) hasta que quede a tu gusto
    subtitle = "<span style='font-size: 32pt; color: #5B5B5B;'><b>Argentina, que superaba a Asia en 1990, terminó por debajo.
    </b></span><br><span style='font-size: 25pt; color: #858585;'> Evolución del PBI Industrial per cápita (USD constantes de 2015): 1990-2023</span>",
    
    caption = paste0(
      "Fuente: Schteingart, D. y Sidicaro, N. (2025). Industria. Argendata. Fundar, en base a UNSTATS, INDEC y Fundación Norte y Sur.\n",
      "Nota: PBI industrial per cápita en USD constantes de 2015. Argentina está calculada a precios constantes de 2004.\n",
      "Asia Oriental y América Latina: promedio simple de los países de cada región."
    ),
    x = NULL, y = "PBI Industrial per cápita"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position        = "none",
    plot.title.position    = "plot",
    plot.title             = element_textbox_simple(family = "serif", face = "bold", 
                                                    size = 40 * k, lineheight = 0.33, 
                                                    margin = margin(b = 5)),
    plot.subtitle          = element_markdown(lineheight = 0.7, margin = margin(b = 15)),
    plot.caption           = element_text(color = "grey45", hjust = 0, size = 17 * k, lineheight = 0.33),
    # ... (el resto de tu theme queda igual)
    panel.grid.minor       = element_blank(),
    panel.grid.major.x     = element_blank(),
    panel.grid.major.y     = element_line(color = "grey92", linetype = "dashed", linewidth = 0.3),
    axis.text              = element_text(color = "grey40", size = 25),
    axis.ticks.x           = element_line(color = "grey60"),
    plot.margin            = margin(12, 50, 10, 12),
    axis.title             = element_text(color = "grey35", size = 27),
  )

# CORRECCIÓN DE DISPOSITIVO: Eliminado device = ragg::agg_jpeg para evitar conflicto con showtext
ggsave("output/graficos/grupo_01_comunicativo.jpg", plot = g_ind_pc_esp,
       width = 22, height = 14, units = "cm", dpi = 300)






























wdi_limpio <- read.csv("input/wdi_limpio.csv")

# CORRECCIÓN METODOLÓGICA: if_else para evitar saltos en la serie de tiempo
wdi_limpio_2 <- wdi_limpio |> 
  filter(region %in% c("Este de Asia y Pacífico", "América Latina y el Caribe"), poblacion_mill > 1, anio >= 1990 & anio <= 2023, 
         !pais %in% c("Cuba", "Korea, Dem. People's Rep.", "Venezuela, RB")) |> 
  select(pais, anio, indice_nivel_precios, region, pbi_per_capita) 
 
fuentes_ok <- tryCatch({
  font_add_google("Playfair Display", "playfair", regular.wt = 600, bold.wt = 700)
  font_add_google("Lato", "lato", regular.wt = 400, bold.wt = 700)
  TRUE
}, error = function(e) FALSE)

fuente_titulo <- if (fuentes_ok) "playfair" else "serif"
fuente_texto  <- if (fuentes_ok) "lato" else "sans"


k <- 0.25
col_personalizados <- c("Asia Oriental y Pacífico" = "#E0195B", "América Latina y el Caribe" = "#3919E0", "Argentina" = "#19E09E", "China" = "#FFAC47")

df_eap <- wdi_limpio_2 %>% filter(region == "Este de Asia y Pacífico") %>% group_by(anio) %>% summarise(indice_nivel_precios = mean(indice_nivel_precios, na.rm = TRUE), .groups = "drop") %>% mutate(pais = "Asia Oriental y Pacífico") %>% select(year = anio, pais, indice_nivel_precios)
df_lac <- wdi_limpio_2 %>% filter(region == "América Latina y el Caribe") %>% group_by(anio) %>% summarise(indice_nivel_precios = mean(indice_nivel_precios, na.rm = TRUE), .groups = "drop") %>% mutate(pais = "América Latina y el Caribe") %>% select(year = anio, pais, indice_nivel_precios)
df_paises <- wdi_limpio_2 %>% filter(pais %in% c("Argentina", "China")) %>% mutate(pais = if_else(pais == "China", "China", "Argentina")) %>% select(year = anio, pais, indice_nivel_precios)

df_industrializacion <- bind_rows(df_eap, df_lac, df_paises) %>% filter(year >= 1990, year <= 2023)
ult_ind_pc <- df_industrializacion %>% group_by(pais) %>% filter(year == max(year)) %>% ungroup() %>% mutate(etiqueta = str_wrap(pais, 14))

g_ind_niv <- ggplot(df_industrializacion, aes(x = year, y = indice_nivel_precios, color = pais, group = pais)) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(data = ult_ind_pc, size = 3) +
  geom_text(data = ult_ind_pc, aes(x = 2023.7, label = etiqueta), hjust = 0, lineheight = 0.3, fontface = "bold", size = 8 * k, show.legend = FALSE) +
  geom_vline(xintercept = 2001, linetype = "dotted", color = "grey60") +
  geom_richtext(data = data.frame(x = 2001.25, y = max(df_industrializacion$indice_nivel_precios, na.rm = TRUE) * 0.06),
                aes(x = x, y = y, label = "<b>2001: Ingreso de <span style='color:#ffac47'>China</span><br>a la OMC</b>", family = fuente_texto),
                hjust = 0, size = 6 * k, color = "grey35", lineheight = 0.3, fill = NA, label.color = NA, inherit.aes = FALSE) +
  annotate(geom = "curve", x = 2002.5, y = 350, xend = 2001, yend = 548.45, curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "grey60") +
  annotate(geom = "curve", x = 2009.5, y = 3400, xend = 2008, yend = 3400, curvature = 0.1, arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +
  annotate(geom = "text", x = 2010, y = 3400, label = "Crisis del 2008", hjust = 0, size = 6 * k, lineheight = 0.3, color = "grey35",       # <-- IGUALAMOS EL COLOR GRIS
           family = fuente_texto,  # <-- FORZAMOS LA MISMA FUENTE
           fontface = "bold"       # <-- IGUALAMOS LA NEGRITA DEL HTML
  ) +
  scale_color_manual(values = col_personalizados) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2023), limits = c(1990, 2028.5), expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(labels = label_dollar(prefix = "$", big.mark = ".", decimal.mark = ","), limits = c(0, 120), expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 15) +
  labs(
    title = "Desde 1990 <span style='color:#ffac47'>China</span> aumentó 22 veces su PBI Industrial por habitante,<br>superando al promedio de <span style='color:#E0195B'>Asia Oriental</span> y <span style='color:#3919E0'>América Latina</span>.",
    
    # Agrupamos la frase de Argentina y la técnica en el subtítulo separadas por un <br>
    # Ajustá los valores de font-size (ej. 24pt y 18pt) hasta que quede a tu gusto
    subtitle = "<span style='font-size: 32pt; color: #5B5B5B;'><b>Argentina, que superaba a Asia en 1990, terminó por debajo.
    </b></span><br><span style='font-size: 25pt; color: #858585;'> Evolución del PBI Industrial per cápita (USD constantes de 2015): 1990-2023</span>",
    
    caption = paste0(
      "Fuente: Schteingart, D. y Sidicaro, N. (2025). Industria. Argendata. Fundar, en base a UNSTATS, INDEC y Fundación Norte y Sur.\n",
      "Nota: PBI industrial per cápita en USD constantes de 2015. Argentina está calculada a precios constantes de 2004.\n",
      "Asia Oriental y América Latina: promedio simple de los países de cada región."
    ),
    x = NULL, y = "PBI Industrial per cápita"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position        = "none",
    plot.title.position    = "plot",
    plot.title             = element_textbox_simple(family = "serif", face = "bold", 
                                                    size = 40 * k, lineheight = 0.33, 
                                                    margin = margin(b = 5)),
    plot.subtitle          = element_markdown(lineheight = 0.7, margin = margin(b = 15)),
    plot.caption           = element_text(color = "grey45", hjust = 0, size = 17 * k, lineheight = 0.33),
    # ... (el resto de tu theme queda igual)
    panel.grid.minor       = element_blank(),
    panel.grid.major.x     = element_blank(),
    panel.grid.major.y     = element_line(color = "grey92", linetype = "dashed", linewidth = 0.3),
    axis.text              = element_text(color = "grey40", size = 25),
    axis.ticks.x           = element_line(color = "grey60"),
    plot.margin            = margin(12, 50, 10, 12),
    axis.title             = element_text(color = "grey35", size = 27),
  )

print(g_ind_niv)
# CORRECCIÓN DE DISPOSITIVO: Eliminado device = ragg::agg_jpeg para evitar conflicto con showtext
ggsave("output/graficos/grupo_01_comunicativo.jpg", plot = g_ind_pc_esp,
       width = 22, height = 14, units = "cm", dpi = 300)

