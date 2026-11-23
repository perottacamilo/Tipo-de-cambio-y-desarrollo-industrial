# ==============================================================================
# 0. CONFIGURACIÓN, LIBRERÍAS, TIPOGRAFÍAS Y BASES
# ==============================================================================
rm(list = ls())
options(scipen = 999)

library(tidyverse)
library(scales)
library(ggtext)
library(ggrepel)
library(showtext)
library(patchwork)

# Carga de funciones auxiliares propias (hacer_hist, theme_owid, pintar, panel_latam, juntar)
invisible(lapply(list.files("utils", full.names = TRUE), source))

# Tipografías globales (usadas en el gráfico comunicativo y el exploratorio)
fuentes_ok <- tryCatch({
  font_add_google("Playfair Display", "playfair", regular.wt = 600, bold.wt = 700)
  font_add_google("Lato", "lato", regular.wt = 400, bold.wt = 700)
  TRUE
}, error = function(e) FALSE)

fuente_titulo <- if (fuentes_ok) "playfair" else "serif"
fuente_texto  <- if (fuentes_ok) "lato" else "sans"

showtext_auto()
showtext_opts(dpi = 96)

# Paletas, formatos y rutas comunes
col         <- c(rojo = "#B13507", azul = "#286BBB", gris = "#BDBDBD")
col_asia    <- "#E0195B"
col_latam   <- "#3919E0"
gris_titulo <- "#363636"
gris_texto  <- "#5B5B5B"
gris_suave  <- "#858585"
base_indice <- "EE. UU. = 100"

fmt_num    <- label_number(big.mark = ".", decimal.mark = ",", drop0trailing = TRUE)
fmt_pct    <- label_number(suffix = "%", big.mark = ".", decimal.mark = ",", drop0trailing = TRUE)
fuente_txt <- "Fuente: Elaboración propia en base al Banco Mundial, WDI; y ArgenData."

dir.create("output/graficos/eda", recursive = TRUE, showWarnings = FALSE)

# Carga de bases
wdi_limpio   <- read.csv("input/wdi_limpio.csv")
wdi2         <- read.csv("input/wdi2.csv")
wdi_filtrado <- read.csv("input/wdi_filtrado.csv")


# ==============================================================================
# 1. GRÁFICO COMUNICATIVO: EVOLUCIÓN DEL PBI INDUSTRIAL (1990-2023)
# ==============================================================================
showtext_auto(TRUE)

wdi_limpio_2 <- wdi_limpio |>
  filter(
    region %in% c("East Asia & Pacific", "Este de Asia y Pacífico",
                  "Latin America & Caribbean", "América Latina y el Caribe"),
    poblacion_mill > 1,
    anio >= 1990 & anio <= 2023,
    !pais %in% c("Cuba", "Korea, Dem. People's Rep.", "Venezuela, RB")
  ) |>
  select(pais, anio, industria_pbi, exportacion_manufactura_pc, region,
         pbi_per_capita, indice_nivel_precios, industria_pc) |>
  mutate(
    industria_pc_wdi     = (industria_pbi * pbi_per_capita) / 100,
    industria_per_capita = coalesce(industria_pc, industria_pc_wdi)
  ) |>
  arrange(pais, anio)

k <- 1.25
col_personalizados <- c(
  "Asia Oriental y Pacífico"   = "#E0195B",
  "América Latina y el Caribe" = "#3919E0",
  "Argentina"                  = "#19E09E",
  "China"                      = "#FFAC47"
)

df_eap <- wdi_limpio_2 |>
  filter(region %in% c("East Asia & Pacific", "Este de Asia y Pacífico")) |>
  group_by(anio) |>
  summarise(industria_per_capita = mean(industria_per_capita, na.rm = TRUE), .groups = "drop") |>
  mutate(pais = "Asia Oriental y Pacífico") |>
  select(year = anio, pais, industria_per_capita)

df_lac <- wdi_limpio_2 |>
  filter(region %in% c("Latin America & Caribbean", "América Latina y el Caribe")) |>
  group_by(anio) |>
  summarise(industria_per_capita = mean(industria_per_capita, na.rm = TRUE), .groups = "drop") |>
  mutate(pais = "América Latina y el Caribe") |>
  select(year = anio, pais, industria_per_capita)

df_paises <- wdi_limpio_2 |>
  filter(pais %in% c("Argentina", "China")) |>
  mutate(pais = if_else(pais == "China", "China", "Argentina")) |>
  select(year = anio, pais, industria_per_capita)

df_industrializacion <- bind_rows(df_eap, df_lac, df_paises) |>
  filter(year >= 1990, year <= 2023)

ult_ind_pc <- df_industrializacion |>
  group_by(pais) |>
  filter(year == max(year)) |>
  ungroup() |>
  mutate(etiqueta = str_wrap(pais, 14))

g_ind_pc_esp <- ggplot(df_industrializacion, aes(x = year, y = industria_per_capita, color = pais, group = pais)) +
  geom_line(linewidth = 1.1, alpha = 0.85) +
  geom_point(data = ult_ind_pc, size = 3) +
  geom_text(data = ult_ind_pc, aes(x = 2023.7, label = etiqueta),
            hjust = 0, lineheight = 0.3, fontface = "bold", size = 8 * k, show.legend = FALSE) +
  geom_vline(xintercept = 2001, linetype = "dotted", color = "grey60") +
  geom_richtext(
    data = data.frame(x = 2001.25, y = max(df_industrializacion$industria_per_capita, na.rm = TRUE) * 0.06),
    aes(x = x, y = y, label = "<b>2001: Ingreso de <span style='color:#ffac47'>China</span><br>a la OMC</b>", family = fuente_texto),
    hjust = 0, size = 6 * k, color = "grey35", lineheight = 0.3, fill = NA, label.color = NA, inherit.aes = FALSE
  ) +
  annotate(geom = "curve", x = 2002.5, y = 350, xend = 2001, yend = 548.45,
           curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +
  geom_vline(xintercept = 2008, linetype = "dotted", color = "grey60") +
  annotate(geom = "curve", x = 2009.5, y = 3400, xend = 2008, yend = 3400,
           curvature = 0.1, arrow = arrow(length = unit(0.2, "cm")), color = "black", size = 0.5) +
  annotate(geom = "text", x = 2010, y = 3400, label = "Crisis del 2008",
           hjust = 0, size = 6 * k, lineheight = 0.3, color = "grey35",
           family = fuente_texto, fontface = "bold") +
  scale_color_manual(values = col_personalizados) +
  scale_x_continuous(breaks = c(1990, 2000, 2010, 2023), limits = c(1990, 2028.5),
                     expand = expansion(mult = c(0.01, 0.05))) +
  scale_y_continuous(labels = label_dollar(prefix = "$", big.mark = ".", decimal.mark = ","),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(clip = "off") +
  labs(
    title    = "Desde 1990 <span style='color:#ffac47'>China</span> aumentó 22 veces su PBI Industrial por habitante,<br>superando al promedio de <span style='color:#E0195B'>Asia Oriental</span> y <span style='color:#3919E0'>América Latina</span>.",
    subtitle = "<span style='font-size: 32pt; color: #5B5B5B;'><b>Argentina, que superaba a Asia en 1990, terminó por debajo.</b></span><br><span style='font-size: 25pt; color: #858585;'>Evolución del PBI Industrial per cápita (USD constantes de 2015): 1990-2023</span>",
    caption  = paste0(
      "Fuente: Schteingart, D. y Sidicaro, N. (2025). Industria. Argendata. Fundar, en base a UNSTATS, INDEC y Fundación Norte y Sur.\n",
      "Nota: PBI industrial per cápita en USD constantes de 2015. Argentina está calculada a precios constantes de 2004.\n",
      "Asia Oriental y América Latina: promedio simple de los países de cada región."
    ),
    x = NULL,
    y = NULL) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position     = "none",
    plot.title.position = "plot",
    plot.title          = element_textbox_simple(family = "serif", face = "bold", size = 40 * k, lineheight = 0.33, margin = margin(b = 5)),
    plot.subtitle       = element_markdown(lineheight = 0.7, margin = margin(b = 15)),
    plot.caption        = element_text(color = "grey45", hjust = 0, size = 17 * k, lineheight = 0.33),
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y  = element_line(color = "grey92", linetype = "dashed", linewidth = 0.3),
    axis.text           = element_text(color = "grey40", size = 25),
    axis.ticks.x        = element_line(color = "grey60"),
    plot.margin         = margin(12, 50, 10, 12),
    axis.title          = element_text(color = "grey35", size = 27)
  )

ggsave("output/graficos/grafico_comunicativo.png", plot = g_ind_pc_esp,
       width = 22, height = 14, units = "cm", dpi = 300)

showtext_auto(FALSE)
# ==============================================================================
# 2. HISTOGRAMAS DE DISTRIBUCIÓN DE VARIABLES PRINCIPALES (2022)
# ==============================================================================
# Usa la función hacer_hist() cargada desde utils/
datos_2022 <- wdi2 |> filter(anio == 2022)

g_exp <- hacer_hist(
  datos_2022, "exportacion_manufactura_pc",
  "Exportaciones manuf. per cápita\n(USD corr., escala log.)",
  escala_log = TRUE
)

g_ind <- hacer_hist(
  datos_2022, "industria_per_capita",
  "PBI industrial per cápita\n(USD const. 2015, escala log.)",
  escala_log = TRUE
)

g_precios <- hacer_hist(
  datos_2022, "indice_nivel_precios",
  "Índice de nivel de precios\n(EE. UU. = 100)",
  escala_log = FALSE
)

print(g_exp)
print(g_ind)
print(g_precios)

ggsave("output/graficos/eda/hist_exportaciones.png", g_exp,     width = 6, height = 4.2, dpi = 300, bg = "white")
ggsave("output/graficos/eda/hist_industria.png",     g_ind,     width = 6, height = 4.2, dpi = 300, bg = "white")
ggsave("output/graficos/eda/hist_precios.png",       g_precios, width = 6, height = 4.2, dpi = 300, bg = "white")


# ==============================================================================
# 3. PORCENTAJE DE DATOS FALTANTES POR VARIABLE (2001, 2008 Y 2022)
# ==============================================================================
variables <- c(
  "crecimiento_industria", "industria_per_capita", "tipo_cambio_real_efectivo",
  "indice_nivel_precios", "empleo_industrial", "exportacion_manufactura_pc",
  "gasto_gobierno_pbi", "gastos_totales_gob_pbi", "valor_agregado_trabajador",
  "ahorro_bruto_pbi", "credito_privado_pbi", "recursos_naturales",
  "pbi_per_capita", "pbi_millones")

etiquetas <- c(
  "crecimiento_industria"      = "Crecimiento industrial (%)",
  "industria_per_capita"       = "PBI industrial per cápita",
  "tipo_cambio_real_efectivo"  = "Tipo de cambio real efectivo",
  "indice_nivel_precios"       = "Índice de nivel de precios",
  "empleo_industrial"          = "Empleo industrial (%)",
  "exportacion_manufactura_pc" = "Exportaciones manuf. per cápita",
  "gasto_gobierno_pbi"         = "Gasto de gobierno (% PBI)",
  "gastos_totales_gob_pbi"     = "Gasto total del gobierno (% PBI)",
  "valor_agregado_trabajador"  = "Valor agregado por trabajador",
  "ahorro_bruto_pbi"           = "Ahorro bruto (% PBI)",
  "credito_privado_pbi"        = "Crédito privado (% PBI)",
  "recursos_naturales"         = "Rentas de recursos naturales (% PBI)",
  "pbi_per_capita"             = "PBI per cápita",
  "pbi_millones"               = "PBI total (millones USD)")

wdi_2     <- wdi2 |> filter(poblacion_mill > 1)
variables <- intersect(variables, names(wdi_2))

if (nrow(wdi_2) == 0 || length(variables) == 0) {
  stop("No hay observaciones o variables disponibles.")
}

faltantes <- wdi_2 |>
  group_by(anio) |>
  summarise(across(all_of(variables), ~ sum(is.na(.x))), .groups = "drop") |>
  pivot_longer(cols = -anio, names_to = "variable", values_to = "n_faltantes") |>
  complete(variable = variables, fill = list(n_faltantes = 0)) |>
  mutate(porcentaje = 100 * n_faltantes / nrow(wdi_2))

totales <- faltantes |>
  group_by(variable) |>
  summarise(porcentaje_total = sum(porcentaje), .groups = "drop") |>
  filter(porcentaje_total > 0) |>
  arrange(porcentaje_total)

grafico_df <- faltantes |>
  semi_join(totales, by = "variable") |>
  mutate(variable = factor(variable, levels = totales$variable))

totales <- totales |>
  mutate(variable = factor(variable, levels = variable))

grafico5 <- ggplot(grafico_df, aes(x = porcentaje, y = variable, fill = factor(anio))) +
  geom_col(position = position_stack(reverse = TRUE), width = 0.7, color = "white", linewidth = 0.4) +
  geom_text(
    aes(label = ifelse(porcentaje >= 3, paste0(round(porcentaje, 1), "%"), "")),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 3.2
  ) +
  geom_text(
    data = totales,
    aes(x = porcentaje_total, y = variable, label = paste0(round(porcentaje_total, 1), "%")),
    inherit.aes = FALSE, hjust = -0.2, size = 3.5
  ) +
  scale_fill_manual(values = c("2001" = "#FFE89C", "2008" = "#A8E7BC", "2022" = "#A8D5F7")) +
  scale_x_continuous(limits = c(0, 108), breaks = seq(0, 100, 20),
                     labels = function(x) paste0(x, "%"), expand = c(0, 0)) +
  scale_y_discrete(labels = etiquetas) +
  labs(
    title    = "Porcentaje de datos faltantes por variable",
    subtitle = "Distribución de faltantes entre 2001, 2008 y 2022",
    x        = "Porcentaje de datos faltantes",
    y        = NULL,
    fill     = "Año",
    caption  = fuente_txt
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position    = "bottom",
    plot.title         = element_text(face = "bold"),
    axis.text.y        = element_text(size = 10)
  )

print(grafico5)
ggsave("output/graficos/eda/datos_faltanes_vars.png", plot = grafico5, width = 12, height = 7, dpi = 300)


# ==============================================================================
# 4. IMPACTO DEL FILTRADO: REGIÓN, INGRESOS Y TABLA DESCRIPTIVA
# ==============================================================================

# --- 4.1 Distribución por región (Antes vs. Después) --------------------------
filtrado1 <- wdi2 |> count(region) |> mutate(porcentaje = (n / sum(n)) * 100, base = "Antes de filtrar")
filtrado2 <- wdi_filtrado |> count(region) |> mutate(porcentaje = (n / sum(n)) * 100, base = "Base filtrada")

datos_combinados <- bind_rows(filtrado1, filtrado2) |>
  mutate(region = if_else(str_detect(region, "Afganistán"), "Medio Oriente*", region))

grafico10 <- ggplot(datos_combinados, aes(x = reorder(region, -porcentaje), y = porcentaje, fill = base)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title    = "Distribución global por región",
    subtitle = "Porcentaje de países en cada categoría sobre el total",
    x        = "Región",
    y        = "Porcentaje del total de países",
    fill     = "Muestra",
    caption  = paste0("*Medio Oriente, Norte de África, Afganistán y Pakistán.\n", fuente_txt)
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x           = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y           = element_text(size = 13),
    axis.title            = element_text(size = 15, face = "bold"),
    panel.border          = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.caption          = element_text(hjust = 0, size = 10, color = "gray30", margin = margin(t = 12)),
    plot.caption.position = "plot"
  )

print(grafico10)
ggsave("output/graficos/eda/variacion_region.png", plot = grafico10, width = 12, height = 7, dpi = 300)

# --- 4.2 Distribución por nivel de ingreso (Antes vs. Después) ----------------
ingreso1  <- wdi2 |> count(ingreso2) |> mutate(porcentaje = (n / sum(n)) * 100, base = "Antes de filtrar")
ingreso_2 <- wdi_filtrado |> count(ingreso2) |> mutate(porcentaje = (n / sum(n)) * 100, base = "Base filtrada")

datos_combinados2 <- bind_rows(ingreso1, ingreso_2)

grafico11 <- ggplot(datos_combinados2, aes(x = reorder(ingreso2, -porcentaje), y = porcentaje, fill = base)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(
    title    = "Distribución de los ingresos",
    subtitle = "Porcentaje de países en cada categoría sobre el total",
    x        = "Ingreso",
    y        = "Porcentaje del total de países",
    fill     = "Muestra",
    caption  = fuente_txt
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x           = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y           = element_text(size = 13),
    axis.title            = element_text(size = 15, face = "bold"),
    panel.border          = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.caption          = element_text(hjust = 0, size = 10, color = "gray30", margin = margin(t = 12)),
    plot.caption.position = "plot"
  )

print(grafico11)
ggsave("output/graficos/eda/variacion_ingreso.png", plot = grafico11, width = 12, height = 7, dpi = 300)

# --- 4.3 Tabla descriptiva antes vs. después del filtrado (Anexo Pág. 2) ------
vars_clave <- c("industria_per_capita", "exportacion_manufactura_pc", "indice_nivel_precios")

resumen_stats <- function(df, nombre_muestra) {
  df |>
    select(all_of(vars_clave)) |>
    pivot_longer(everything(), names_to = "Variable", values_to = "valor") |>
    group_by(Variable) |>
    summarise(
      N       = sum(!is.na(valor)),
      Media   = round(mean(valor, na.rm = TRUE), 1),
      Mediana = round(median(valor, na.rm = TRUE), 1),
      Desvio  = round(sd(valor, na.rm = TRUE), 1),
      .groups = "drop"
    ) |>
    mutate(Muestra = nombre_muestra)
}

tabla_antes_despues <- bind_rows(
  resumen_stats(wdi2, "Antes de filtrar"),
  resumen_stats(wdi_filtrado, "Base filtrada")
) |>
  arrange(Variable, Muestra)

print(tabla_antes_despues)


# ==============================================================================
# 5. GRÁFICO EXPLORATORIO: INDUSTRIA Y EXPORTACIONES VS. NIVEL DE PRECIOS
# ==============================================================================
# Usa las funciones theme_owid(), pintar(), panel_latam() y juntar() de utils/
showtext_auto(TRUE)

wdi_filtrado_2 <- wdi_filtrado |>
  mutate(
    industria_pc_wdi     = (industria_pbi * pbi_per_capita) / 100,
    industria_per_capita = coalesce(industria_pc, industria_pc_wdi)
  )

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
  select(
    pais, grupo_region, anio,
    pib           = indice_nivel_precios,
    Industria     = industria_per_capita,
    Exportaciones = exportacion_manufactura_pc
  ) |>
  pivot_longer(c(Industria, Exportaciones), names_to = "indicador", values_to = "valor") |>
  filter(!is.na(valor), valor > 0, !is.na(pib)) |>
  mutate(
    titulo    = paste0(indicador, " (", anio, ")"),
    subtitulo = if_else(indicador == "Industria", "Industria per cápita", "Exportaciones per cápita (USD)"),
    log_y     = TRUE,
    codigo    = pais,
    y         = log10(valor),
    x         = log10(pib)
  ) |>
  group_by(titulo) |>
  mutate(residuo = residuals(loess(y ~ x))) |>
  ungroup()

resumen <- dispersion |>
  filter(grupo_region %in% c("América Latina", "Asia Oriental")) |>
  group_by(titulo, grupo_region) |>
  summarise(paises = n(), encima = sum(residuo > 0, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = grupo_region, values_from = c(paises, encima)) |>
  rename(
    paises_asia  = `paises_Asia Oriental`,
    paises_latam = `paises_América Latina`,
    encima_asia  = `encima_Asia Oriental`,
    encima_latam = `encima_América Latina`
  )

orden_paneles <- c(
  "Industria (2001)", "Industria (2008)", "Industria (2022)",
  "Exportaciones (2001)", "Exportaciones (2008)", "Exportaciones (2022)"
)

paneles_filtradoes <- dispersion |>
  left_join(resumen, by = "titulo") |>
  mutate(
    sub_filtrado = paste0(
      "Por encima de la curva mundial. Asia: ", encima_asia, " de ", paises_asia,
      " · Latam: ", encima_latam, " de ", paises_latam
    )
  ) |>
  split(~ factor(titulo, levels = orden_paneles)) |>
  purrr::map(\(d) panel_latam(d))

titulo_filtrado <- paste0(
  "PBI Industrial y Exportaciones según el Nivel de Precios:<br>",
  pintar("Asia Oriental", col_asia), " y ", pintar("América Latina", col_latam),
  " frente al resto del mundo, 2001, 2008 y 2022."
)

subtitulo_filtrado <- paste0(
  "Cada punto es un país; América Latina en azul y Asia en rojo, resto del mundo en gris. La curva es la tendencia estimada con todos los países.<br>",
  "Los países por encima de la curva tienen un valor más alto que el esperado dado su nivel de precios.<br>",
  "PBI Industrial per cápita, en USD constantes de 2015. Exportaciones: manufacturas per cápita en USD corrientes (escala logarítmica)."
)

caption_filtrado <- paste0(
  "Fuente: elaboración propia en base a Banco Mundial (WDI) y Schteingart, D. y Sidicaro, N. (2025). Industria. Argendata. Fundar.\n",
  "Notas: Industria: datos de Argendata, completados con WDI. ",
  "Se excluyen países con menos de 1 millón de habitantes, y no figuran países sin datos en el año."
)

grafico_exploratorio <- juntar(
  paneles_filtradoes,
  titulo    = titulo_filtrado,
  subtitulo = subtitulo_filtrado,
  caption   = caption_filtrado
)

ggsave("output/graficos/grafico_exploratorio.png", plot = grafico_exploratorio,
       width = 22, height = 14, units = "cm", dpi = 300)

showtext_auto(FALSE)

