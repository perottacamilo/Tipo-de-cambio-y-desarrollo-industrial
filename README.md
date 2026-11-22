# Trabajo Práctico Integrador — Ciencia de Datos para Economía y Negocios
**Facultad de Ciencias Económicas — Universidad de Buenos Aires (FCE-UBA)**  
**Segundo Cuatrimestre 2026**

---

## 1. Objetivo del Trabajo e Hipótesis de Investigación

### Contexto y Problemática
El presente trabajo de investigación aplicada analiza la relación empírica entre el **nivel relativo de precios** (Índice de Nivel de Precios relativo a EE. UU., medido a través del factor de conversión PPA sobre tipo de cambio nominal) y el desempeño de la estructura productiva en términos de **PBI industrial per cápita** y **exportaciones manufactureras per cápita** a escala internacional.

### Hipótesis de Trabajo
1. **Curva de desarrollo y competitividad precio:** En línea con las teorías estructuralistas y la hipótesis Balassa-Samuelson, existe una relación positiva entre el nivel de ingresos y el nivel de precios. No obstante, desviaciones de precios apreciadas respecto de la tendencia del ingreso relativo encarecen la estructura de costos y desincentivan la inserción exportadora de manufacturas de alto valor agregado.
2. **Divergencia regional (América Latina vs. Asia Oriental):** A lo largo de las últimas décadas (evaluadas en los cortes de **2001**, **2008** y **2022**), los países de **Asia Oriental** lograron desacoplarse virtuosamente hacia mayores densidades industriales per cápita y mayor inserción exportadora, mientras que **América Latina** (y **Argentina** en particular) exhibe trayectorias de rezago productivo y vulnerabilidad externa.

---

## 2. Justificación de las Bases de Datos Elegidas

Para contrastar la hipótesis con solidez metodológica se utilizaron dos fuentes públicas complementarias:

1. **World Development Indicators (WDI) — Banco Mundial:**
   - Provee series macroeconómicas armonizadas internacionalmente con cobertura para más de 200 países desde 1960.
   - Variables clave extraídas: PBI per cápita (USD constantes 2015), Índice de Nivel de Precios (EE. UU. = 100), Empleo industrial (% del empleo total), Tasa de crecimiento del PBI y Exportaciones de manufacturas per cápita.
2. **Argendata (Fundar) / Schteingart, D. y Sidicaro, N. (2025):**
   - Base complementaria sectorial que sistematiza y armoniza las series de **PBI industrial per cápita** a precios constantes internacionales (en base a UNSTATS, INDEC, CEPAL y Fundación Norte y Sur).
   - Su incorporación resulta indispensable para resolver rupturas estadísticas y saltos espurios en las cuentas de manufactura de países clave (como China y economías del Cono Sur).

---

## 3. Descripción de lo Realizado y Flujo Metodológico

El proyecto está diseñado bajo una arquitectura modular y 100% reproducible en R, organizada en etapas secuenciales:

1. **Adquisición (`script/01.datos.R`):**
   - Descarga automatizada vía API de WDI y consolidación de las fuentes primarias sin procesar en la carpeta `raw/`.
2. **Filtrado y Curaduría (`script/02.filtrado.R`):**
   - Exclusión de países con población inferior a 1 millón de habitantes (eliminando microestados para evitar sesgos provocados por paraísos fiscales o economías minúsculas).
   - Depuración de agregados institucionales y regionales del Banco Mundial.
   - Generación de la base limpia `input/wdi_limpio.csv`.
3. **Tratamiento de Datos Faltantes e Imputación (`script/03.datos faltantes.R`):**
   - Diagnóstico sistemático de patrones de no-respuesta.
   - Imputación mediante interpolación temporal por vecinos adyacentes (`utils/imputar_vecinos.R`) para los años 2001, 2008 y 2022, generando la base analítica consolidada `input/wdi_filtrado.csv`.
4. **Visualizaciones y Storytelling (`script/04.graficos.R` y `script/graficos entrega 3.R`):**
   - Distribuciones empíricas de variables clave con histogramas estandarizados (`utils/hacer_hist.R`).
   - Mosaico exploratorio multianual estilo Our World in Data (`utils/panel_latam.R`, `utils/juntar.R`, `utils/theme_owid.R`).
   - Gráfico comunicacional de series de tiempo con eventos históricos de quiebre estructural (ingreso de China a la OMC en 2001, Crisis Financiera Global de 2008). Exportados en `output/graficos/`.
5. **Clustering y Reducción Dimensional (`script/05.clustering.R`):**
   - Análisis de Componentes Principales (PCA) para reducir dimensionalidad e identificar factores de desarrollo.
   - Agrupamiento no supervisado jerárquico y por K-Means para segmentar tipologías de desarrollo industrial a nivel global y ubicar el posicionamiento relativo de Argentina.
6. **Modelado Econométrico y Diagnósticos (`script/06.regresion.R`):**
   - Estimación de modelos log-log para Industria y Exportaciones Manufactureras per cápita en 2001, 2008 y 2022.
   - Comparación de especificaciones: Modelos Simples, Con Controles (PBI pc, Crecimiento, Empleo) y Modelos Robustos Sin Atípicos.
   - Batería exhaustiva de diagnósticos econométricos:
     - Test de Heterocedasticidad de Breusch-Pagan (`lmtest::bptest`).
     - Test adaptativo de especificación de Ramsey RESET (`utils/test_reset_adaptativo.R`), corrigiendo con matriz robusta $HC1$ ante heterocedasticidad.
     - Diagnóstico de multicolinealidad mediante Factores de Inflación de la Varianza (VIF).
     - Detección de observaciones influyentes por Distancia de Cook (`utils/obtener_outliers_cook.R`) e isocontornos críticos ($D_i > 0,15$) (`utils/panel_rvsl.R`, `utils/graficar_residuos_leverage.R`, `utils/juntar_rvsl.R`).
   - Exportación de tablas en formato HTML/GT en `output/tablas/`.

---

## 4. Estructura de Carpetas del Repositorio

El repositorio respeta estrictamente la estructura estandarizada requerida por la cátedra:

```
Trabajo-Practico-Ciencia-de-Datos/
├── raw/            # Datos originales tal como fueron descargados, sin modificar
│   ├── wdi_completo.csv
│   └── datos_argendata.xlsx
├── auxiliar/       # Bases de datos complementarias o archivos de apoyo
│   └── diccionario_variables.csv
├── input/          # Datos procesados y listos para el análisis cuantitativo
│   ├── wdi_limpio.csv
│   ├── wdi2.csv
│   └── wdi_filtrado.csv
├── output/
│   ├── tablas/     # Tablas de resultados generadas por los scripts (HTML / GT)
│   │   ├── tabla_resumen_modelos.html
│   │   ├── tabla_diagnosticos_resumen.html
│   │   ├── tabla_vif_final.html
│   │   ├── tabla_bp_final.html
│   │   ├── tabla_reset_comparativa.html
│   │   └── tabla_sin_outliers.html
│   └── graficos/   # Visualizaciones generadas por los scripts (PNG / JPG)
│       ├── grupo_01_exploratoriolala.jpg
│       ├── grupo_01_comunicativolala.jpg
│       └── regresion/
│           ├── rvsl_exploratorio_completo.jpg
│           ├── rvsl_exploratorio_completo.png
│           ├── rvsl_industria_2001.png
│           ├── rvsl_industria_2008.png
│           ├── rvsl_industria_2022.png
│           ├── rvsl_exportaciones_2001.png
│           ├── rvsl_exportaciones_2008.png
│           └── rvsl_exportaciones_2022.png
├── script/         # Scripts de R con objetivo específico por etapa
│   ├── 01.datos.R
│   ├── 02.filtrado.R
│   ├── 03.datos faltantes.R
│   ├── 04.graficos.R
│   ├── 05.clustering.R
│   ├── 06.regresion.R
│   └── graficos entrega 3.R
├── utils/          # Funciones propias modularizadas (un script por función)
│   ├── armar_modelos_resumen.R
│   ├── graficar_residuos_leverage.R
│   ├── hacer_hist.R
│   ├── imputar_vecinos.R
│   ├── juntar.R
│   ├── juntar_rvsl.R
│   ├── obtener_outliers_cook.R
│   ├── panel_latam.R
│   ├── panel_rvsl.R
│   ├── pintar.R
│   ├── resumen_stats.R
│   ├── test_reset_adaptativo.R
│   ├── theme_owid.R
│   ├── top_cook.R
│   └── traducir_pais.R
└── README.md       # Descripción del proyecto y guía de reproducción
```

---

## 5. Instrucciones para Reproducir el Análisis

### Requisitos previos
- R versión >= 4.3.0 (desarrollado y testeado en R 4.6.0).
- RStudio (recomendado para visualización interactiva).

### Instalación de librerías necesarias
Ejecutar la siguiente instrucción en la consola de R para instalar cualquier dependencia faltante:
```r
paquetes <- c(
  "tidyverse", "readxl", "scales", "ggtext", "ggrepel", "showtext",
  "patchwork", "modelsummary", "gt", "car", "sandwich", "lmtest",
  "tseries", "factoextra", "cluster", "mclust", "webshot2"
)
paquetes_faltantes <- setdiff(paquetes, installed.packages()[, "Package"])
if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}
```

### Ejecución secuencial de scripts
Para reproducir todo el flujo de trabajo desde los datos brutos hasta las salidas finales, ejecutar los scripts en orden:

```r
# 1. Descarga y extracción de datos originales
source("script/01.datos.R")

# 2. Limpieza inicial y filtros poblacionales
source("script/02.filtrado.R")

# 3. Diagnóstico e imputación temporal de valores faltantes
source("script/03.datos faltantes.R")

# 4. Generación de estadísticas descriptivas y gráficos principales
source("script/04.graficos.R")
source("script/graficos entrega 3.R")

# 5. Análisis multivariado de componentes principales y clustering
source("script/05.clustering.R")

# 6. Modelado econométrico, diagnósticos y tablas de regresión
source("script/06.regresion.R")
```
