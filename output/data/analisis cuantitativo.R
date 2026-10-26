# Script Analisis cuantitativo - Regresiones y Clustering

# install.packages(c("FactoMineR", "factoextra", "ggrepel", "cluster", "mclust", "lmtest", "sandwich", "car", "modelsummary" ))

library(readxl)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)      
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(mclust)
library(lmtest)       
library(sandwich)     
library(car)           
library(modelsummary)  


rm(list = ls())

wdi_final <- read.csv("output/data/wdi_final.csv")

wdi_final_2022 <- wdi_final |> 
  filter(anio == 2022)

#Regresiones

#Primero aplicar sobre una variable sola

r1_2022 <- lm(industria_pbi ~ indice_nivel_precios, data = wdi_final_2022)

summary(r1_2022)
ggplot(r1_2022)

r2_2022 <- lm(industria_pbi ~ indice_nivel_precios + recursos_naturales + 
                credito_privado_pbi + log(pbi_per_capita) + gasto_gobierno_pbi, data = wdi_final_2022)
summary(r2_2022)

r3_2022 <- lm(industria_pbi ~ indice_nivel_precios + recursos_naturales + 
                ahorro_bruto_pbi + log(pbi_per_capita) + gasto_gobierno_pbi, data = wdi_final_2022)

summary(r3_2022)

r4_2022 <- lm(industria_pbi ~ indice_nivel_precios + recursos_naturales + 
                ahorro_bruto_pbi + log(pbi_per_capita) +I(log(pbi_per_capita)^2), data = wdi_final_2022)

summary(r4_2022)

r6_2022 <- lm(industria_pbi ~ recursos_naturales + ahorro_bruto_pbi + log(pbi_per_capita) +I(log(pbi_per_capita)^2), data = wdi_final_2022)
summary(r5_2022)




variables2 <- c(
  "crecimiento_industria",
  "industria_pbi",
  "indice_nivel_precios",
  "empleo_industrial",
  "exportacion_manufactura",
  "gasto_gobierno_pbi",
  "valor_agregado_trabajador",
  "ahorro_bruto_pbi",
  "credito_privado_pbi",
  "recursos_naturales",
  "pbi_per_capita",
  "pbi_millones")

modelplot(r3_2022, vcov = "HC1", coef_omit = "Intercept|edad") +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Coeficiente (log-puntos)", title = "log(ingreso horario), EPH 1T2026")


r6_2022 <- lm(industria_pbi ~ log(pbi_per_capita) + I(log(pbi_per_capita)^2), data = wdi_final_2022)

summary(r6_2022)

avPlots(r6_2022, terms = ~ log(pbi_per_capita))


library(car)

avPlots(r1_2022, terms = ~ indice_nivel_precios)

avPlots(r2_2022, terms = ~ indice_nivel_precios)

avPlots(r3_2022, terms = ~ indice_nivel_precios)

avPlots(r4_2022, terms = ~ indice_nivel_precios)

#Despues agregar los controles

#Significativo no es lo mismo que importante. 
#Mirar siempre la magnitud y preguntarse si es económicamente relevante.


## 1.8 Diagnóstico: chequear los supuestos y corregir

#  | Problema | Qué rompe | Test en R | Solución |
#  |:--|:--|:--|:--|
#  | Forma funcional incorrecta | Coeficientes (sesgo) | `resettest()` | Logs, cuadrados, interacciones |
#  | Multicolinealidad | Nada, pero infla los EE | `vif()` | Quitar o combinar variables |
#  | Heteroscedasticidad | EE, tests $t$ y $F$ | `bptest()` | Errores robustos: `vcovHC()` |
#  | No normalidad | Tests con $n$ chico | `jarque.bera.test()` | Con $n$ grande, nada. Si no, logs |
#  | Observaciones influyentes | Coeficientes | `cooks.distance()` | Revisar y reportar con y sin |
  

### Primera mirada: los gráficos de `plot()`

```{r fig.height=7}
par(mfrow = c(2, 2))
plot(regresion, pch = 16, col = rgb(0, 0, 1, 0.1))
par(mfrow = c(1, 1))
```

#  | Gráfico | Qué mirar |
#  |:--|:--|
#  | Residuals vs Fitted | Nube sin forma alrededor de 0. Una curva sugiere mala forma funcional |
#  | Normal Q-Q | Puntos sobre la diagonal: residuos aproximadamente normales |
#  | Scale-Location | Línea plana: varianza constante |
#  | Residuals vs Leverage | Puntos fuera de las líneas de Cook: muy influyentes |
  


### Multicolinealidad: VIF

#No genera sesgo: con regresoras muy correlacionadas entre sí, 
#los errores estándar se inflan. Regla práctica: VIF > 10 merece atención.


vif(r4_2022)
vif(r5_2022)
vif(r6_2022)






### Heteroscedasticidad: detectar

#La varianza del error cambia con las regresoras. 
#Los coeficientes siguen bien, pero **los errores estándar están mal**, 
#y con ellos los tests y los intervalos.

{r6_2022 fig.height=3.5}
plot(fitted(r6_2022), residuals(r6_2022), pch = 16, col = rgb(0, 0, 1, 0.1),
     xlab = "Valores predichos", ylab = "Residuos")
abline(h = 0, lty = 2)








**Test de Breusch-Pagan**: regresa los residuos al cuadrado contra las regresoras. $H_0$: varianza constante.

bptest(r6_2022)

bptest(r5_2022)


### Heteroscedasticidad: corregir con errores robustos

#Errores estándar **robustos de White**: mismos coeficientes, 
#errores estándar recalculados con una fórmula válida con o sin heteroscedasticidad. 

coeftest(regresion)                                        # errores clásicos
coeftest(regresion, vcov = vcovHC(regresion, type = "HC1"))       # errores robustos






### Normalidad de los residuos

#No afecta los coeficientes. 
#Solo hace falta para que los tests sean exactos con **muestras chicas**; 
#con más de 100 o 200 observaciones funcionan bien igual (teorema central del límite).

{r fig.height=3.5}
qqnorm(residuals(r4_2022), pch = 16, col = rgb(0, 0, 1, 0.1))
qqline(residuals(r4_2022), col = "firebrick", lwd = 2)
tseries::jarque.bera.test(residuals(r4_2022))





## 1.10 Comparar modelos y guardar la tabla

#Con distinta cantidad de variables, comparar con **R² ajustado** o **AIC** (menor es mejor),
#nunca con R² a secas. Solo son comparables modelos con **la misma variable dependiente 
#y la misma muestra** (no se puede comparar `e1` con `e1_niveles`).

AIC(regresion1, regresion2, regresion3)
sapply(list(e0 = e0, e1 = e1, e2 = e2), \(m) summary(m)$adj.r.squared)





#Un gráfico de coeficientes comunica mejor que la tabla frente a públicos no técnicos:
  
  ```{r fig.height=4}
modelplot(e1, vcov = "HC1", coef_omit = "Intercept|edad") +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = "Coeficiente (log-puntos)", title = "log(ingreso horario), EPH 1T2026")
```


aa

# **Errores frecuentes de interpretación**
  
#  - Leer causalidad donde hay asociación.
#  - Extrapolar fuera del rango de los datos (el modelo no sabe nada de personas de 90 años).
#  - Confundir significancia estadística con magnitud relevante.
#  - Leer un coeficiente con interacción como si fuera el efecto general (en `e2`, `sexoVaron` es la brecha solo en "hasta primario").
#  - Leer solo el término lineal cuando hay un cuadrado.









# ==============================================================================
# 1. ARMAR LA BASE
# ==============================================================================
# Misma base que la clase 15: una fila por pais, una columna por variable.
# leer_ad() busca primero el archivo en la carpeta de datos de las diapositivas;
# si no esta, lo baja de Argendata y lo guarda ahi.

base_ad   <- "https://raw.githubusercontent.com/argendatafundar/data/main/"
carpeta_ad <- file.path("..", "diapositivas", "datos", "argendata")
leer_ad <- function(ruta) {
  local <- file.path(carpeta_ad, basename(ruta))
  if (!file.exists(local)) {
    dir.create(carpeta_ad, recursive = TRUE, showWarnings = FALSE)
    download.file(paste0(base_ad, ruta), local, mode = "wb", quiet = TRUE)
  }
  read_csv(local, show_col_types = FALSE)
}

anio_ref <- 2022
# Corte transversal: filtra el anio y deja el codigo de pais + la(s) columna(s)
corte <- function(d, ...) d |> filter(anio == anio_ref) |> select(iso = geocodigoFundar, ...)

fec <- leer_ad("DEMOGR/tasa_fecundidad_adolescente_paises.csv") |>
  filter(anio == anio_ref) |>
  group_by(iso = geocodigoFundar) |>
  arrange(fuente != "World Population Prospects (UN)", .by_group = TRUE) |>
  slice(1) |>                                   # una fuente por pais
  ungroup() |>
  select(iso, fec_adol = tgf_adolescente)

paises <- leer_ad("DESHUM/expectativa_vida.csv") |>
  filter(es_agregacion == 0, anio == anio_ref) |>
  select(iso = geocodigoFundar, pais = geonombreFundar,
         continente = continente_fundar, esp_vida = expectativa_vida) |>
  inner_join(corte(leer_ad("DESHUM/expectativa_educ.csv"), esc_esperada = expectativa_educ), by = "iso") |>
  inner_join(corte(leer_ad("DESHUM/anios_prom_educ.csv"),  esc_promedio = anios_prom_educ),  by = "iso") |>
  inner_join(corte(leer_ad("DESHUM/inb_pc.csv"), inb_pc), by = "iso") |>
  left_join(leer_ad("DESIGU/ISA_mundo_i1.csv") |>
              transmute(iso = geocodigoFundar, gini = as.numeric(gini)), by = "iso") |>
  left_join(leer_ad("CAMCLI/emisiones_per_cap.csv") |> filter(anio == anio_ref) |>
              transmute(iso = geocodigoFundar, emis_pc = as.numeric(valor_per_cap)), by = "iso") |>
  left_join(fec, by = "iso") |>
  left_join(corte(leer_ad("DESHUM/indice_desarrollo_humano.csv"), idh), by = "iso")

# Cuantos faltantes hay por variable? (clase 12: mirar antes de borrar)
colSums(is.na(paises))

# PCA no acepta NA. Nos quedamos con los paises completos en las 7 variables.
# OJO: esto deja afuera sobre todo a paises sin dato de Gini.
paises <- paises |>
  drop_na(esp_vida, esc_esperada, esc_promedio, inb_pc, gini, emis_pc, fec_adol) |>
  mutate(log_inb = log(inb_pc), log_emis = log(emis_pc))
nrow(paises)

# Matriz con las variables ACTIVAS (las que construyen los componentes).
# Los nombres de fila son los paises: asi aparecen en los graficos.
vars <- c("esp_vida", "esc_esperada", "esc_promedio", "log_inb", "gini", "log_emis", "fec_adol")
X <- paises |> select(all_of(vars)) |> as.data.frame()
rownames(X) <- paises$pais

summary(X)

# EN TU TP: una fila por unidad (pais, provincia, empresa) y una columna por
# variable NUMERICA. Decidir que hacer con los NA antes del PCA (eliminar o
# imputar, clase 12) y dejarlo escrito.


#Clustering
# ==============================================================================
# TP
# ==============================================================================
rm(list = ls())


wdi_final <- read.csv("output/data/wdi_final.csv")

wdi_pca <- wdi_final |> 
  filter(anio == 2022) |> 
  mutate(ln_pbi_pc = log(pbi_per_capita), ln_va_trabajador = log(valor_agregado_trabajador))

vars <- c("crecimiento_industria", "industria_pbi", "indice_nivel_precios", 
      "empleo_industrial", "exportacion_manufactura", "gasto_gobierno_pbi",
    "ln_va_trabajador", "ahorro_bruto_pbi", "credito_privado_pbi", 
                    "recursos_naturales", "ln_pbi_pc")

X <- wdi_pca |> select(all_of(vars)) |> as.data.frame()
rownames(X) <- wdi_pca$pais
# ==============================================================================
# ==============================================================================







# 2. ANTES DEL PCA: CORRELACIONES
# ==============================================================================
# PCA resume variables CORRELACIONADAS. Si las variables no se correlacionan,
# no hay nada que resumir y cada componente va a ser casi una variable.

round(cor(X), 2)

# Version grafica: un heatmap con ggplot.
heatmap <- cor(X) |>
  as_tibble(rownames = "v1") |>
  pivot_longer(-v1, names_to = "v2", values_to = "r") |>
  ggplot(aes(v1, v2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "steelblue", limits = c(-1, 1)) +
  labs(x = NULL, y = NULL, title = "Correlaciones entre las variables activas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path("output/graficos", "heatmap.png"), 
       plot = heatmap, width = 12, height = 7, units = "in", dpi = 300)


# ==============================================================================
# ==============================================================================
# ==============================================================================




# 3. PCA CON R BASE: prcomp()
# ==============================================================================



# 4. PCA CON FactoMineR (la herramienta que vamos a usar)
# ==============================================================================
# FactoMineR::PCA() estandariza por defecto (scale.unit = TRUE) y devuelve,
# ademas de lo anterior, medidas para interpretar: cos2 y contribuciones.
# Tambien permite variables SUPLEMENTARIAS (ver seccion 7).

res <- PCA(X, scale.unit = TRUE, graph = FALSE)

# ---- 4.1 Cuantos componentes mirar ----
res$eig
fviz_eig(res, addlabels = TRUE)


#   Criterio de Kaiser: autovalor > 1 (con variables estandarizadas, un
#   componente que resume menos que UNA variable original no vale la pena).
# Son heuristicas: elegir tambien por interpretabilidad.

#Pensar por que se estandariza (reducir varianza de posibles variables)
#Con cuantos PCA nos quedamos y por que, pensar que tenemos muchas variables
#Por la variabilidad (le gusta al profesor) por autovalores > 1...



# ---- 4.2 Variables: coordenadas, cos2 y contribuciones ----
res$var$coord[, 1:4]     # correlacion de cada variable con cada componente
res$var$cos2[, 1:4]      # calidad de representacion (coord al cuadrado)
res$var$contrib[, 1:4]   # % con que cada variable construye cada componente

fviz_pca_var(res, repel = TRUE, col.var = "contrib",
             gradient.cols = c("gray70", "steelblue", "firebrick"))

pc1 <- fviz_contrib(res, choice = "var", axes = 1)   # quien arma PC1
pc2 <- fviz_contrib(res, choice = "var", axes = 2)   # quien arma PC2
pc3 <- fviz_contrib(res, choice = "var", axes = 3)   # quien arma PC3
pc4 <- fviz_contrib(res, choice = "var", axes = 4)   # quien arma PC4

ggsave(file.path("output/graficos", "pc1.png"), 
       plot = pc1, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos", "pc2.png"), 
       plot = pc2, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos", "pc3.png"), 
       plot = pc3, width = 12, height = 7, units = "in", dpi = 300)
ggsave(file.path("output/graficos", "pc4.png"), 
       plot = pc4, width = 12, height = 7, units = "in", dpi = 300)



#Tratar de pensar que representa cada PCA, es decir darle un nombre
#Para facilitar la interpretabilidad, sobretodo en los primeros dos
#Ya que son los que capturan la mayor varianza



# ---- 4.3 Descripcion automatica de los ejes ----
dimdesc(res, axes = 1:4)

# COMO SE LEE: lista las variables con correlacion significativa con cada eje,
# ordenadas. Es un buen punto de partida para ponerle nombre a un componente.
# El nombre ("desarrollo") lo ponemos nosotros: PCA solo da numeros.





# 5. LAS OBSERVACIONES: EL MAPA DE PAISES
# ==============================================================================

mapa_paises <- fviz_pca_ind(res, repel = TRUE, labelsize = 3, col.ind = "cos2",
             gradient.cols = c("gray80", "steelblue", "firebrick"))
ggsave(file.path("output/graficos", "mapa_paises.png"), 
       plot = mapa_paises, width = 12, height = 7, units = "in", dpi = 300)


#Grafico importante, ayuda a ver a que PCA esta mas cerca cada pais
#Interpretar por ejemplo Iraq tiene muchos recursos naturales (Dim2) pero poco pbi_pc
#Hong Kong pocos rec_nat y un pbi_pc muy alto
#Malawi no tiene nada de nada




# Colorear por continente (habillage = variable de agrupamiento)
fviz_pca_ind(res, label = "none", habillage = factor(wdi_pca$region),
             addEllipses = TRUE, ellipse.level = 0.68)







#Ver solo LATAM y ASIA
wdi_pca <- wdi_pca |>
  mutate(
    region_resaltada = case_when(
      region %in% c("Latin America & Caribbean", "East Asia & Pacific", "South Asia") ~ region,
      TRUE ~ "Otros"))


mapa_regiones <- fviz_pca_ind(
  res, 
  label = "none", 
  habillage = factor(wdi_pca$region_resaltada),
  addEllipses = TRUE, 
  ellipse.level = 0.68,
  # Asegurate de que el orden de los colores coincida con el orden alfabético de tus factores
  # Por ejemplo: 1. East Asia, 2. Latin America, 3. Otros, 4. South Asia (revisá los niveles de tu factor)
  palette = c("#1B9E77", "#D95F02", "gray85", "#7570B3") 
)

ggsave(file.path("output/graficos", "mapa_regiones.png"), 
       plot = mapa_regiones, width = 12, height = 7, units = "in", dpi = 300)







# Filtramos solo las regiones que nos interesan y cruzamos con el cluster asignado
distribucion_regional <- wdi_pca |>
  mutate(cluster = res_hcpc$data.clust$clust) |>
  filter(region %in% c("Latin America & Caribbean", "East Asia & Pacific", "South Asia")) |>
  count(region, cluster) |>
  group_by(region) |>
  mutate(porcentaje = round(n / sum(n) * 100, 1))

# Ver el resultado
print(distribucion_regional)



# COMO SE LEE: paises cercanos tienen perfiles parecidos en las 7 variables.
# Un pais con cos2 bajo esta mal representado en el plano: su posicion en el
# grafico puede enganiar (su diferencia esta en PC3 o mas alla).










pc1 <- res$ind$coord[, 1]
if (cor(pc1, paises$log_inb) < 0) pc1 <- -pc1

# ---- 8.2 Sensibilidad: sacar una variable ----
# QUE HACE: rehace el PCA sin cada variable y compara el nuevo PC1 con el
#   original (correlacion de rangos).
sensibilidad <- map_dfr(vars, function(v) {
  r <- PCA(X[, setdiff(vars, v)], graph = FALSE)
  tibble(sin = v,
         var_pc1 = r$eig[1, 2],
         cor_pc1 = abs(cor(r$ind$coord[, 1], pc1, method = "spearman")))
})
sensibilidad

# COMO SE LEE: si PC1 casi no cambia al sacar cualquier variable (cor ~ 1),
# la dimension es robusta. Mirar en cambio que pasa con PC2 (ejercicio 4).


#Importante
# EN TU TP: reportar (1) que variables entran y por que, (2) si se
# estandarizo, (3) % de varianza de los componentes que se usan, (4) loadings
# y la interpretacion propuesta, (5) alguna prueba de sensibilidad.


#Del analisis concluimos
#Elegimos 11 variables (sacamos pbi_millones, preferimos pbi_pc)
#Aplicamos log a pbi_pc y valor_agregado_trabajador para que no alteren el analisis
#Estandarizamos variables
#Los primeros 4 PC capturan el 79% de la variabilidad
#Ademas los 4 tienen eugenvalues mayores a 1
#Debemos ponerle nombre/identificar cada PC por la varianza de las variables
#Ver varios graficos de paises y regiones (similitud/cercania)
#Analisis de sensibilidad, ver como cambian los PC si sacamos alguna variables (Robustez)

































# ==============================================================================
# Clase 16 - Practica 3: Cluster jerarquico y HCPC
# Ciencia de Datos para Economia y Negocios - FCE-UBA
#
# QUE ES ESTE SCRIPT
# Agrupar paises con cluster jerarquico, en dos etapas:
#   (A) "a mano": estandarizar, distancias, hclust(), dendrograma, corte,
#       caracterizacion. Para entender cada decision.
#   (B) con HCPC() de FactoMineR: PCA -> Ward sobre los componentes ->
#       consolidacion con k-means -> descripcion automatica de los grupos.
#       Es el flujo que conviene usar en el trabajo final.
# Al final, HCPC sobre un MCA (ocupados de la EPH): el mismo flujo con
# variables categoricas.
#
# Cada paso esta comentado en tres niveles:
#   QUE HACE    -> que calcula el bloque
#   COMO SE LEE -> que hay que mirar en el resultado
#   EN TU TP    -> como adaptarlo a la base del trabajo final
#
# Conviene haber hecho antes los scripts 01 (PCA) y 02 (MCA).
#
# PAQUETES
#   install.packages(c("FactoMineR", "factoextra", "cluster", "mclust"))
# ==============================================================================


# ---- 0. Preparacion ------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(cluster)      # silhouette()

options(scipen = 999)
theme_set(theme_minimal(base_size = 12))
set.seed(2026)


# ==============================================================================
# 1. LA BASE (la misma del script 01)
# ==============================================================================

base_ad   <- "https://raw.githubusercontent.com/argendatafundar/data/main/"
carpeta_ad <- file.path("..", "diapositivas", "datos", "argendata")
leer_ad <- function(ruta) {
  local <- file.path(carpeta_ad, basename(ruta))
  if (!file.exists(local)) {
    dir.create(carpeta_ad, recursive = TRUE, showWarnings = FALSE)
    download.file(paste0(base_ad, ruta), local, mode = "wb", quiet = TRUE)
  }
  read_csv(local, show_col_types = FALSE)
}
anio_ref <- 2022
corte <- function(d, ...) d |> filter(anio == anio_ref) |> select(iso = geocodigoFundar, ...)

fec <- leer_ad("DEMOGR/tasa_fecundidad_adolescente_paises.csv") |>
  filter(anio == anio_ref) |>
  group_by(iso = geocodigoFundar) |>
  arrange(fuente != "World Population Prospects (UN)", .by_group = TRUE) |>
  slice(1) |> ungroup() |>
  select(iso, fec_adol = tgf_adolescente)

paises <- leer_ad("DESHUM/expectativa_vida.csv") |>
  filter(es_agregacion == 0, anio == anio_ref) |>
  select(iso = geocodigoFundar, pais = geonombreFundar,
         continente = continente_fundar, esp_vida = expectativa_vida) |>
  inner_join(corte(leer_ad("DESHUM/expectativa_educ.csv"), esc_esperada = expectativa_educ), by = "iso") |>
  inner_join(corte(leer_ad("DESHUM/anios_prom_educ.csv"),  esc_promedio = anios_prom_educ),  by = "iso") |>
  inner_join(corte(leer_ad("DESHUM/inb_pc.csv"), inb_pc), by = "iso") |>
  left_join(leer_ad("DESIGU/ISA_mundo_i1.csv") |>
              transmute(iso = geocodigoFundar, gini = as.numeric(gini)), by = "iso") |>
  left_join(leer_ad("CAMCLI/emisiones_per_cap.csv") |> filter(anio == anio_ref) |>
              transmute(iso = geocodigoFundar, emis_pc = as.numeric(valor_per_cap)), by = "iso") |>
  left_join(fec, by = "iso") |>
  drop_na(esp_vida, esc_esperada, esc_promedio, inb_pc, gini, emis_pc, fec_adol) |>
  mutate(log_inb = log(inb_pc), log_emis = log(emis_pc))

vars <- c("esp_vida", "esc_esperada", "esc_promedio", "log_inb", "gini", "log_emis", "fec_adol")
X <- paises |> select(all_of(vars)) |> as.data.frame()
rownames(X) <- paises$pais


# ==============================================================================
# PARTE A: CLUSTER JERARQUICO "A MANO"
# ==============================================================================

# ---- A.1 Estandarizar y calcular distancias ----
# QUE HACE: scale() resta la media y divide por el desvio cada columna.
#   dist() calcula la distancia euclidea entre cada par de paises.
Z <- scale(X)
d <- dist(Z, method = "euclidean")

# La matriz de distancias tiene n(n-1)/2 elementos:
length(d)

# Quien se parece mas a Argentina?
as.matrix(d)["Argentina", ] |> sort() |> head(6)
# Quien se parece mas a China?
as.matrix(d)["China", ] |> sort() |> head(6)

# COMO SE LEE: el primer valor es 0 (Argentina consigo misma). Los siguientes
# son los paises con perfil mas parecido EN ESTAS 7 VARIABLES ESTANDARIZADAS.
# Probar sin estandarizar: dist(X). Cambia la lista? (ejercicio 1)

#TP
#Los mas parecidos a Arg, son Uruguay, Colombia, Albania, Ecuador y Brasil
# 4 de los 5 son LATAM (interesante)


# ---- A.2 Aglomerar con distintos criterios de linkage ----
hc_ward     <- hclust(d, method = "ward.D2")
hc_complete <- hclust(d, method = "complete")

# Dendrogramas rapidos con R base
par(mfrow = c(2, 2), mar = c(1, 4, 2, 1))
plot(hc_ward, labels = FALSE, main = "Ward")
plot(hc_complete, labels = FALSE, main = "Complete")
par(mfrow = c(1, 1))



#Utilizar el Ward el mas lindo y el recomendado

# DECIMOS. Es una decision analitica, no un descubrimiento.
#Utilizamos 4 grupos
#Si usamos 3 agrupa demasiados en el primer grupo lo cual es dificl de interpretar
#Si usamos 5, el 5to esta formado por un solo pais (Timor Oriental), no tiene sentido


k <- 4
grupos <- cutree(hc_ward, k = k)
table(grupos)

# ---- A.6 Caracterizar los grupos ----
# QUE HACE: promedios de las variables ORIGINALES (no estandarizadas) por grupo.
perfil <- wdi_pca |>
  mutate(grupo = factor(grupos)) |>
  group_by(grupo) |>
  summarise(n = n(),
            crecimiento_industria_prom = mean(crecimiento_industria), 
            industria_pbi_prom = mean(industria_pbi),
            indice_nivel_precios_median = median(indice_nivel_precios), 
            mean_empleo_industrial = mean(empleo_industrial),
            mean_exportacion_manufactura = median(exportacion_manufactura), 
            mean_gasto_gobierno_pbi = mean(gasto_gobierno_pbi),
ln_va_trabajador_median = median(ln_va_trabajador),
mean_ahorro_bruto_pbi = mean(ahorro_bruto_pbi),
mean_credito_privado_pbi = mean(credito_privado_pbi),
median_recursos_naturales = median(recursos_naturales),
median_ln_pbi_pc = median(ln_pbi_pc))|>
  arrange(industria_pbi_prom)
perfil



# Que paises hay en cada grupo?
split(wdi_pca$pais, grupos)

#Claramente en el segundo grupo estan los paises desarrollados
#En el cuarto lo que estan un pasito abajo
#Entre el primero y tercero no te sabria decir la diferecia

# Silueta de cada pais: quien esta "en el borde" entre dos grupos?
sil <- silhouette(grupos, d)

silhouette <- fviz_silhouette(sil, label = FALSE)

ggsave(file.path("output/graficos", "silhouette.png"), 
       plot = silhouette, width = 12, height = 7, units = "in", dpi = 300)

#Se ve cmo en general el segundo y cuarto parecen estar bien
#El tercero es un poco mas disperso, no sabria decir que criterio tiene
#El primero es mas grande y tiene algunos paises que se salen

wdi_pca |>
  mutate(grupo = grupos, silueta = sil[, "sil_width"]) |>
  arrange(silueta) |>
  select(pais, grupo, silueta) |>
  head(10)

#Que paises se salen de cada grupo

# COMO SE LEE: silueta negativa = el pais esta, en promedio, mas cerca de otro
# grupo que del suyo. Son los casos dudosos: conviene nombrarlos al presentar.

# EN TU TP: una tabla de perfiles en unidades originales, una lista de
# ejemplos por grupo y los casos con silueta mas baja.












#Hacer lo mismo de antes pero con otro paquete que parece ser mejor
# ==============================================================================
# PARTE B: HCPC (Hierarchical Clustering on Principal Components)
# ==============================================================================
# QUE HACE HCPC():
#   1. parte de un PCA (o MCA) y se queda con los primeros ncp componentes
#      (saca el "ruido" de los ultimos componentes);
#   2. hace Ward sobre esos componentes;
#   3. corta el arbol (sugiere un k, o le damos uno);
#   4. CONSOLIDA con k-means, usando los centros de Ward como punto de partida
#      (algunos paises pueden cambiar de grupo);
#   5. describe cada grupo con tests sobre las variables y los ejes.

# ---- B.1 El PCA de partida ----
res_pca <- PCA(X, ncp = 5, scale.unit = TRUE, graph = FALSE)
res_pca$eig[1:5, ]

# COMO SE LEE: con 5 componentes conservamos ~96% de la varianza. Usar
# ncp = Inf es equivalente a Ward sobre las variables estandarizadas.

# ---- B.2 Dejar que HCPC sugiera k ----
res_auto <- HCPC(res_pca, nb.clust = -1, graph = FALSE)   # -1 = corte automatico
table(res_auto$data.clust$clust)

# COMO SE LEE: el corte automatico elige el k que maximiza la ganancia
# relativa de inercia entre grupos. Suele elegir pocos grupos.

par(mfrow = c(1, 1))

# ---- B.3 HCPC con k = 4 ----
res_hcpc <- HCPC(res_pca, nb.clust = 4, consol = TRUE, graph = FALSE)

res_hcpc <- HCPC(res_pca)

rm(dendograma)
# png("output/graficos/dendograma.png", width = 8, height = 6, units = "in", res = 300)

dendograma <- plot(res_hcpc, choice = "tree") #CLUSTER         # dendrograma con el corte


# dev.off()

plot(res_hcpc, choice = "bar")           # ganancia de inercia por cada fusion
#La incercia nos diria que debemos elegir 9 clusters
#Lo cual por un lado puede indicar por que no es facil identificar a los
#grupos de los paises del componente 1 y 3, sin embargo agregar muchos
#tambien puede dificultar la interpretabilidad de los grupos

hcpc4 <- fviz_cluster(res_hcpc, repel = TRUE, labelsize = 7, show.clust.cent = TRUE,
             main = "HCPC: 4 grupos en el plano PC1-PC2")

ggsave(file.path("output/graficos", "hcpc4.png"), 
       plot = hcpc4, width = 12, height = 7, units = "in", dpi = 300)


#Interesantes graphs

grupo_hcpc <- res_hcpc$data.clust$clust
table(grupo_hcpc)

# ---- B.4 Descripcion de los grupos ----
# (a) por las variables
res_hcpc$desc.var$quanti

#Mucho muy importante, ver que variables pesan en cada grupo
#Ayuda a interpretar como se conforma cada uno
#Si es positivo (gral > 2) es porque es algo que tiene ese grupo
#Si aparece como negativo es porque le falta
#Ejemplo grupo 1 tiene todo negativo, es decir le falta todo, representa a los paises mas pobres
#Grupo 4 tiene todo muy positivo, son los paises ricos


# COMO SE LEE: para cada grupo, las variables cuyo promedio en el grupo es
# significativamente distinto del promedio general. v.test > 2: el grupo
# tiene valores MAS ALTOS que el promedio; v.test < -2: mas bajos.
# "Mean in category" vs "Overall mean" da la magnitud en unidades originales.

# (c) paragons: los paises mas cercanos al centro de cada grupo (los mas
#     "tipicos") y los mas alejados de los otros grupos (los mas "distintivos")
res_hcpc$desc.ind$para
res_hcpc$desc.ind$dist

#IMPORTANTE

# COMO SE LEE: los paragons son los mejores ejemplos para NOMBRAR un grupo en
# una presentacion. Los distintivos muestran que lo hace unico.

# EN TU TP: reportar ncp, k y si hubo consolidacion. Presentar la tabla de
# desc.var con las magnitudes en unidades originales y los paragons.


#Importante
# 7. EN TU TP: aplicar HCPC a tu base (sobre un PCA o un MCA) y presentar:
#    k elegido y por que, tabla de perfiles, paragons, y una prueba de
#    sensibilidad (otro k u otro ncp).


