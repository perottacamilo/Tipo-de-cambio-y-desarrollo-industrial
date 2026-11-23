# ==============================================================================
# 0. CONFIGURACIÓN, LIBRERÍAS Y PREPARACIÓN DE DATOS
# ==============================================================================

library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)

options(scipen = 999)
set.seed(2026)

theme_set(theme_minimal(base_size = 12))
if (requireNamespace("showtext", quietly = TRUE)) showtext::showtext_auto(FALSE)

# Ruta de salida y fuente común para todos los gráficos
ruta_pca   <- "output/graficos/clustering"
fuente_txt <- "Fuente: elaboración propia en base a Banco Mundial (WDI) y Schteingart, D. y Sidicaro, N. (2025). Industria. Argendata. Fundar."

# Cargar base final y preparar corte 2022 con logaritmos
wdi_final <- read.csv("input/wdi_final.csv")

wdi_pca <- wdi_final |>
  filter(anio == 2022) |>
  mutate(
    ln_pbi_pc                     = log(pbi_per_capita),
    ln_va_trabajador              = log(valor_agregado_trabajador),
    ln_industria_per_capita       = log(industria_per_capita),
    ln_exportacion_manufactura_pc = log(exportacion_manufactura_pc),
    region_resaltada = case_when(
      region %in% c("América Latina y el Caribe", "Este de Asia y Pacífico") ~ region,
      TRUE ~ "Otros"
    )
  )

# Matriz de 11 variables numéricas activas
vars <- c(
  "crecimiento_industria", "ln_industria_per_capita", "indice_nivel_precios",
  "empleo_industrial", "ln_exportacion_manufactura_pc", "gasto_gobierno_pbi",
  "ln_va_trabajador", "ahorro_bruto_pbi", "credito_privado_pbi",
  "recursos_naturales", "ln_pbi_pc"
)

X <- wdi_pca |> select(all_of(vars)) |> as.data.frame()
rownames(X) <- wdi_pca$pais


# ==============================================================================
# 1. ANÁLISIS DE CORRELACIONES (PREVIO AL PCA)
# ==============================================================================
# Verifica qué variables se mueven juntas y justifica reducir dimensionalidad

heatmap <- cor(X) |>
  as_tibble(rownames = "v1") |>
  pivot_longer(-v1, names_to = "v2", values_to = "r") |>
  ggplot(aes(v1, v2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", limits = c(-1, 1)) +
  labs(
    x = NULL, y = NULL,
    title = "Correlaciones entre las variables activas",
    caption = fuente_txt
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(heatmap)
ggsave(file.path(ruta_pca, "heatmap.png"), plot = heatmap, width = 12, height = 7, dpi = 300)


# ==============================================================================
# 2. ANÁLISIS DE COMPONENTES PRINCIPALES (PCA)
# ==============================================================================
# FactoMineR::PCA() estandariza las variables (scale.unit = TRUE) y retiene ncp = 5

res <- PCA(X, ncp = 5, scale.unit = TRUE, graph = FALSE)

# --- 2.1 Autovalores y Scree Plot ---------------------------------------------
res$eig
round(res$var$coord[, 1:4], 3)

tema_agrandado <- theme(
  axis.text  = element_text(size = 18),
  axis.title = element_text(size = 18),
  panel.grid = element_blank())

screeplot <- fviz_eig(res, addlabels = TRUE) +
  labs(
    title   = "Varianza explicada por componente principal",
    x       = "Dimensiones",
    y       = "Porcentaje de varianza explicada (%)",
    caption = fuente_txt) +
  tema_agrandado

print(screeplot)
ggsave(file.path(ruta_pca, "screeplot.png"), plot = screeplot, width = 12, height = 7, dpi = 300)

# --- 2.2 Contribución de variables a cada dimensión (Dim 1 a Dim 4 - Anexo) ---

ejes <- theme(
  axis.text = element_text(size = 18),  
  axis.title = element_text(size = 18))


pc1 <- fviz_contrib(res, choice = "var", axes = 1)  + ejes + 
  labs(
    title   = NULL,
    y       = "Contribución (%)",
    caption = fuente_txt
  ) +
  theme(
    plot.title            = element_blank(),
    axis.text.x           = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y           = element_text(size = 12),
    axis.title.y          = element_text(size = 14, face = "bold"),
    panel.border          = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.caption          = element_text(hjust = 0, size = 10, color = "gray30", margin = margin(t = 12)),
    plot.caption.position = "plot",
    panel.grid            = element_blank(),
  )

pc2 <- fviz_contrib(res, choice = "var", axes = 2) + ejes  + 
  labs(
    title   = NULL,
    y       = "Contribución (%)",
    caption = fuente_txt
  ) +
  theme(
    plot.title            = element_blank(),
    axis.text.x           = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y           = element_text(size = 12),
    axis.title.y          = element_text(size = 14, face = "bold"),
    panel.border          = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.caption          = element_text(hjust = 0, size = 10, color = "gray30", margin = margin(t = 12)),
    plot.caption.position = "plot",
    panel.grid            = element_blank(),
  )

pc3 <- fviz_contrib(res, choice = "var", axes = 3)  + ejes + 
  labs(
    title   = NULL,
    y       = "Contribución (%)",
    caption = fuente_txt
  ) +
  theme(
    plot.title            = element_blank(),
    axis.text.x           = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y           = element_text(size = 12),
    axis.title.y          = element_text(size = 14, face = "bold"),
    panel.border          = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.caption          = element_text(hjust = 0, size = 10, color = "gray30", margin = margin(t = 12)),
    plot.caption.position = "plot",
    panel.grid            = element_blank(),
  )

pc4 <- fviz_contrib(res, choice = "var", axes = 4) + ejes + 
  labs(
    title   = NULL,
    y       = "Contribución (%)",
    caption = fuente_txt
  ) +
  theme(
    plot.title            = element_blank(),
    axis.text.x           = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y           = element_text(size = 12),
    axis.title.y          = element_text(size = 14, face = "bold"),
    panel.border          = element_rect(colour = "black", fill = NA, linewidth = 1),
    plot.caption          = element_text(hjust = 0, size = 10, color = "gray30", margin = margin(t = 12)),
    plot.caption.position = "plot",
    panel.grid            = element_blank(),
  )


ggsave(file.path(ruta_pca, "pc1.png"), 
       plot = pc1, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path(ruta_pca, "pc2.png"), 
       plot = pc2, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path(ruta_pca, "pc3.png"), 
       plot = pc3, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path(ruta_pca, "pc4.png"), 
       plot = pc4, width = 12, height = 7, units = "in", dpi = 300)

print(pc1)
print(pc2)
print(pc3)
print(pc4)



# --- 2.3 Mapa completo de países en el plano PC1-PC2 (Anexo) ------------------
mapa_paises <- fviz_pca_ind(
  res,
  repel         = TRUE,
  labelsize     = 3,
  col.ind       = "cos2",
  gradient.cols = c("gray80", "steelblue", "firebrick"),
  title         = "Mapa países - PCA",
  caption       = fuente_txt
)

print(mapa_paises)
ggsave(file.path(ruta_pca, "mapa_paises.png"), plot = mapa_paises, width = 12, height = 7, dpi = 300)

# --- 2.4 Mapa resaltando Asia Oriental vs. América Latina (Presentación) ------
mapa_regiones <- fviz_pca_ind(
  res,
  label         = "none",
  habillage     = factor(wdi_pca$region_resaltada),
  addEllipses   = TRUE,
  labelsize     = 7.5,
  ellipse.level = 0.68,
  palette       = c("#3919E0", "#E0195B", "gray85"),
  title         = "PCA - Asia y Latam",
  legend.title  = "Región",
  caption       = fuente_txt
) +
  theme(
    axis.text.x           = element_text(angle = 45, hjust = 1, size = 13),
    axis.text.y           = element_text(size = 13),
    axis.title            = element_text(size = 15, face = "bold"),
    panel.border          = element_rect(colour = "black", fill = NA, linewidth = 1),
    panel.grid            = element_blank(),
    plot.caption          = element_text(hjust = 0, size = 10, color = "gray30", margin = margin(t = 12)),
    plot.caption.position = "plot"
  )

print(mapa_regiones)
ggsave(file.path(ruta_pca, "mapa_regiones.png"), plot = mapa_regiones, width = 12, height = 7, dpi = 300)

# --- 2.5 Análisis de sensibilidad del PC1 (Anexo) -----------------------------
# Evalúa si la varianza y el orden de PC1 cambian al excluir una variable por vez
coord_pc1 <- res$ind$coord[, 1]

sensibilidad <- map_dfr(vars, function(v) {
  r <- PCA(X[, setdiff(vars, v)], graph = FALSE)
  tibble(
    sin     = v,
    var_pc1 = r$eig[1, 2],
    cor_pc1 = abs(cor(r$ind$coord[, 1], coord_pc1, method = "spearman"))
  )
})
print(sensibilidad)


# ==============================================================================
# 3. CLUSTERING JERÁRQUICO
# ==============================================================================

# --- 3.1 Distancias euclidianas y validación (Método de Ward sobre Z) ---------
# Sirve para: (a) ver países más parecidos, (b) elegir k=5 por altura, (c) validar silueta
Z <- scale(X)
d <- dist(Z, method = "euclidean")

# Países más parecidos a Argentina y China (Anexo)
as.matrix(d)["Argentina", ] |> sort() |> head(6)
as.matrix(d)["China", ]     |> sort() |> head(6)

hc_ward <- hclust(d, method = "ward.D2")

# Gráfico de altura de fusiones (Anexo)
alturas_plot <- tibble(k = 1:10, altura = rev(hc_ward$height)[1:10]) |>
  ggplot(aes(k, altura)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title   = "Altura de las últimas fusiones",
    x       = "Cantidad de grupos",
    y       = "Altura",
    caption = fuente_txt
  )
print(alturas_plot)

# Silueta (Anexo)
k      <- 5
grupos <- cutree(hc_ward, k = k)
table(grupos)

sil <- silhouette(grupos, d)
silhouette_plot <- fviz_silhouette(sil, label = FALSE) +
  labs(
    title   = "Gráfico de siluetas",
    y       = "Ancho de silueta",
    caption = fuente_txt
  )

print(silhouette_plot)
ggsave(file.path(ruta_pca, "silhouette.png"), plot = silhouette_plot, width = 12, height = 7, dpi = 300)

# Países con menor ancho de silueta 
wdi_pca |>
  mutate(grupo = grupos, silueta = sil[, "sil_width"]) |>
  arrange(silueta) |>
  select(pais, grupo, silueta) |>
  head(10)

# --- 3.2 HCPC: Clustering jerárquico sobre los componentes del PCA ------------

hcpc <- HCPC(res, nb.clust = 5, consol = TRUE, graph = FALSE)

# Dendrograma y ganancia de inercia (Anexo)
par(mfrow = c(1, 1))
plot(hcpc, choice = "bar")


# Mapa de los 5 clusters en el plano PC1-PC2 (Presentación)
hcpc5 <- fviz_cluster(
  hcpc,
  repel           = TRUE,
  labelsize       = 7.5,
  show.clust.cent = TRUE,
  main            = "HCPC: 5 grupos en el plano PC1-PC2"
) +
  labs(caption = fuente_txt)

print(hcpc5)
ggsave(file.path(ruta_pca, "hcpc5.png"), plot = hcpc5, width = 12, height = 7, dpi = 300)

# --- 3.3 Caracterización e interpretación de los 5 grupos (Anexo) -------------
grupo_hcpc <- hcpc$data.clust$clust

table(grupo_hcpc)                          # Cantidad de países por cluster
table(Ward = grupos, HCPC = grupo_hcpc)    # Consistencia entre Ward directo y HCPC
split(wdi_pca$pais, grupo_hcpc)            # Listado de países por cluster

hcpc$desc.var$quanti                       # Variables significativas por grupo (v.test)
hcpc$desc.ind$para                         # Paragons: países más representativos
hcpc$desc.ind$dist                         # Países más distintos dentro de su grupo

