#CLUSTERING

# install.packages(c("FactoMineR", "factoextra", "ggrepel", "cluster", "mclust", "lmtest", "sandwich", "car", "modelsummary" ))


library(readxl)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)      
library(ggrepel)
library(mclust)






# ==============================================================================
# TP
# ==============================================================================
rm(list = ls())

options(scipen = 999)
theme_set(theme_minimal(base_size = 12))
set.seed(2026)

wdi_final <- read.csv("input/wdi_final.csv")

wdi_pca <- wdi_final |> 
  filter(anio == 2022) |> 
  mutate(ln_pbi_pc = log(pbi_per_capita), 
         ln_va_trabajador = log(valor_agregado_trabajador), 
         ln_industria_per_capita = log(industria_per_capita),
         ln_exportacion_manufactura_brutas = log(exportacion_manufactura_brutas))

vars <- c("crecimiento_industria", "ln_industria_per_capita", "indice_nivel_precios", 
          "empleo_industrial", "ln_exportacion_manufactura_brutas", "gasto_gobierno_pbi",
          "ln_va_trabajador", "ahorro_bruto_pbi", "credito_privado_pbi", 
          "recursos_naturales", "ln_pbi_pc")

X <- wdi_pca |> select(all_of(vars)) |> as.data.frame()
rownames(X) <- wdi_pca$pais


heatmap <- cor(X) |>
  as_tibble(rownames = "v1") |>
  pivot_longer(-v1, names_to = "v2", values_to = "r") |>
  ggplot(aes(v1, v2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, title = "Correlaciones entre las variables activas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(heatmap)



#ANEXO
ggsave(file.path("output/graficos", "heatmap.png"), 
       plot = heatmap, width = 12, height = 7, units = "in", dpi = 300)


# 4. PCA CON FactoMineR (la herramienta que vamos a usar)
# ==============================================================================
# FactoMineR::PCA() estandariza por defecto (scale.unit = TRUE) y devuelve,

res <- PCA(X, scale.unit = TRUE, graph = FALSE)
coord_pc1 <- res$ind$coord[, 1]
# ---- 4.1 Cuantos componentes mirar ----
res$eig
screeplot <- fviz_eig(res, addlabels = TRUE)
print(screeplot)


#ANEXO
ggsave(file.path("output/graficos", "screeplot.png"), 
       plot = screeplot, width = 12, height = 7, units = "in", dpi = 300)

rm(screeplot)


#Pensar por que se estandariza (reducir varianza de posibles variables)
#Con cuantos PCA nos quedamos y por que, pensar que tenemos muchas variables
#Por la variabilidad (le gusta al profesor) por autovalores > 1...




tema_agrandado <- theme(
  axis.text = element_text(size = 13),  # Agranda las etiquetas (los nombres de las variables y los números)
  axis.title = element_text(size = 13)  # Agranda los títulos ("Contributions (%)", "Variables")
)
pc1 <- fviz_contrib(res, choice = "var", axes = 1)  + tema_agrandado # quien arma PC1
pc2 <- fviz_contrib(res, choice = "var", axes = 2) + tema_agrandado  # quien arma PC2
pc3 <- fviz_contrib(res, choice = "var", axes = 3)  + tema_agrandado # quien arma PC3
pc4 <- fviz_contrib(res, choice = "var", axes = 4) + tema_agrandado  # quien arma PC4

print(pc1)
print(pc2)
print(pc3)
print(pc4)


#ANEXO
ggsave(file.path("output/graficos", "pc1.png"), 
       plot = pc1, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos", "pc2.png"), 
       plot = pc2, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos", "pc3.png"), 
       plot = pc3, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos", "pc4.png"), 
       plot = pc4, width = 12, height = 7, units = "in", dpi = 300)



mapa_paises <- fviz_pca_ind(res, repel = TRUE, labelsize = 3, col.ind = "cos2",
                            gradient.cols = c("gray80", "steelblue", "firebrick"))
print(mapa_paises)


#PRESENTACION o ANEXO
ggsave(file.path("output/graficos", "mapa_paises.png"), 
       plot = mapa_paises, width = 12, height = 7, units = "in", dpi = 300)


#Ver solo LATAM y ASIA
wdi_pca <- wdi_pca |>
  mutate(
    region_resaltada = case_when(
      region %in% c("América Latina y el Caribe", "Este de Asia y Pacífico") ~ region,
      TRUE ~ "Otros"))


mapa_regiones <- fviz_pca_ind(
  res, 
  label = "none", 
  habillage = factor(wdi_pca$region_resaltada),
  addEllipses = TRUE, 
  ellipse.level = 0.68,
  # Asegurate de que el orden de los colores coincida con el orden alfabético de tus factores
  # Por ejemplo: 1. East Asia, 2. Latin America, 3. Otros, 4. South Asia (revisá los niveles de tu factor)
  palette = c("#1B9E77", "#D95F02", "gray85") 
)
print(mapa_regiones)


#PRESENTACION o ANEXO
#ESTE O EL ANTERIOR, NO LOS DOS
#VER SI SE PUEDE HACER UN POCO MAS LINDO
ggsave(file.path("output/graficos", "mapa_regiones.png"), 
       plot = mapa_regiones, width = 12, height = 7, units = "in", dpi = 300)








sensibilidad <- map_dfr(vars, function(v) {
  r <- PCA(X[, setdiff(vars, v)], graph = FALSE)
  tibble(sin = v,
         var_pc1 = r$eig[1, 2],
         cor_pc1 = abs(cor(r$ind$coord[, 1], coord_pc1, method = "spearman")))
})
sensibilidad



Z <- scale(X)
d <- dist(Z, method = "euclidean")


#ANEXO
# Quien se parece mas a Argentina?
as.matrix(d)["Argentina", ] |> sort() |> head(6)
# Quien se parece mas a China?
as.matrix(d)["China", ] |> sort() |> head(6)



# ---- A.2 Aglomerar con distintos criterios de linkage ----
hc_ward     <- hclust(d, method = "ward.D2")


k <- 4
grupos <- cutree(hc_ward, k = k)
table(grupos)


# Que paises hay en cada grupo?
split(wdi_pca$pais, grupos)



# Silueta de cada pais: quien esta "en el borde" entre dos grupos?
sil <- silhouette(grupos, d)

silhouette <- fviz_silhouette(sil, label = FALSE)

print(silhouette)

#ANEXO
ggsave(file.path("output/graficos", "silhouette.png"), 
       plot = silhouette, width = 12, height = 7, units = "in", dpi = 300)

#ANEXO CON SILUETA
wdi_pca |>
  mutate(grupo = grupos, silueta = sil[, "sil_width"]) |>
  arrange(silueta) |>
  select(pais, grupo, silueta) |>
  head(10)
#Que paises se salen de cada grupo



res_pca <- PCA(X, ncp = 5, scale.unit = TRUE, graph = FALSE)
res_pca$eig[1:5, ]



par(mfrow = c(1, 1))

# ---- B.3 HCPC con k = 4 ----
res_hcpc <- HCPC(res_pca, nb.clust = 4, consol = TRUE, graph = FALSE)

# png("output/graficos/dendograma.png", width = 8, height = 6, units = "in", res = 300)

dendograma <- plot(res_hcpc, choice = "tree") #CLUSTER         # dendrograma con el corte

# dev.off()

print(dendograma)
plot(res_hcpc, choice = "bar")           # ganancia de inercia por cada fusion
#La incercia nos diria que debemos elegir 9 clusters
#Lo cual por un lado puede indicar por que no es facil identificar a los
#grupos de los paises del componente 1 y 3, sin embargo agregar muchos
#tambien puede dificultar la interpretabilidad de los grupos
rm(dendograma)

hcpc4 <- fviz_cluster(res_hcpc, repel = TRUE, labelsize = 7, show.clust.cent = TRUE,
                      main = "HCPC: 4 grupos en el plano PC1-PC2")


print(hcpc4)

#PRESENTACION VER SI 3 O 4 GRUPOS
ggsave(file.path("output/graficos", "hcpc4.png"), 
       plot = hcpc4, width = 12, height = 7, units = "in", dpi = 300)



grupo_hcpc <- res_hcpc$data.clust$clust
table(grupo_hcpc)

res_hcpc$desc.var$quanti

#Mucho muy importante, ver que variables pesan en cada grupo
#Ayuda a interpretar como se conforma cada uno
#Si es positivo (gral > 2) es porque es algo que tiene ese grupo
#Si aparece como negativo es porque le falta
#Ejemplo grupo 1 tiene todo negativo, es decir le falta todo, representa a los paises mas pobres
#Grupo 4 tiene todo muy positivo, son los paises ricos




#ANEXO
res_hcpc$desc.ind$para
res_hcpc$desc.ind$dist
