
# ==============================================================================
# SCRIPT MAESTRO: GRÁFICO EXPLORATORIO Y COMUNICATIVO
# ==============================================================================
rm(list = ls())
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
dir.create("graficos_clase21", showWarnings = FALSE)

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
      region %in% c("América Latina y el Caribe", "Latin America & Caribbean") ~ "América Latina",
      region %in% c("Este de Asia y Pacífico", "East Asia & Pacific") ~ "Asia Oriental",
      TRUE ~ "Resto del Mundo"
    )
  ) |>
  select(pais, grupo_region, anio, pib = indice_nivel_precios,
         Industria = industria_per_capita,
         Exportaciones = exportacion_manufactura_brutas) |>
  pivot_longer(c(Industria, Exportaciones), names_to = "indicador", values_to = "valor") |>
  filter(!is.na(valor), valor > 0, !is.na(pib)) |>
  mutate(
    titulo = paste0(indicador, " (", anio, ")"),
    subtitulo = if_else(indicador == "Industria", "Industria per cápita", "Exportaciones brutas (millones USD)"),
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
  map(\(d) panel_latam(d))

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
grafico_exploratorio_2 <- juntar(paneles_filtradoes, titulo = titulo_filtrado, subtitulo = subtitulo_filtrado, caption = caption_filtrado)

# CORRECCIÓN DE DISPOSITIVO: Eliminado device = ragg::agg_jpeg para evitar conflicto con showtext
ggsave("output/graficos/grupo_01_exploratorio.jpg", plot = grafico_exploratorio_2,
       width = 22, height = 14, units = "cm", dpi = 300)
# ------------------------------------------------------------------------------
# 3. PROCESAMIENTO Y GRÁFICO COMUNICATIVO
# ------------------------------------------------------------------------------
rm(list = ls())
wdi_limpio <- read.csv("output/data/wdi_limpio.csv")

# CORRECCIÓN METODOLÓGICA: if_else para evitar saltos en la serie de tiempo
wdi_limpio_2 <- wdi_limpio |> 
  filter(region %in% c("East Asia & Pacific", "Latin America & Caribbean"), poblacion_mill > 1, anio >= 1990 & anio <= 2023, 
         !pais %in% c("Cuba", "Korea, Dem. People's Rep.", "Venezuela, RB")) |> 
  select(pais, anio, industria_pbi, exportacion_manufactura_brutas, region, pbi_per_capita, indice_nivel_precios, industria_pc) |>
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

df_eap <- wdi_limpio_2 %>% filter(region == "East Asia & Pacific") %>% group_by(anio) %>% summarise(industria_per_capita = mean(industria_per_capita, na.rm = TRUE), .groups = "drop") %>% mutate(pais = "Asia Oriental y Pacífico") %>% select(year = anio, pais, industria_per_capita)
df_lac <- wdi_limpio_2 %>% filter(region == "Latin America & Caribbean") %>% group_by(anio) %>% summarise(industria_per_capita = mean(industria_per_capita, na.rm = TRUE), .groups = "drop") %>% mutate(pais = "América Latina y el Caribe") %>% select(year = anio, pais, industria_per_capita)
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

