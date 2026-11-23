# Tipo de cambio y desarrollo industrial
### Trabajo Práctico Integrador — Ciencia de Datos para Economía y Negocios
**Facultad de Ciencias Económicas — Universidad de Buenos Aires (FCE-UBA)**  
**Docente:** Nicolás Sidicaro — **Ayudante:** Maida Beltrán  
**2do Cuatrimestre 2026**

---

## 1. Título del Proyecto
**Tipo de cambio y desarrollo industrial**

---

## 2. Integrantes del Grupo
- **Camilo Perotta**
- **Ignacio Keena**
- **Thomas Hannon**

---

## 3. Objetivo del Trabajo e Hipótesis de Investigación

### Contexto y Problemática
El presente trabajo de investigación aplicada analiza la relación empírica entre el **nivel relativo de precios** (Índice de Nivel de Precios relativo a EE. UU., medido a través del factor de conversión PPA sobre tipo de cambio nominal) y el desempeño de la estructura productiva en términos de **PBI industrial per cápita** y **exportaciones manufactureras per cápita** a escala internacional.

### Hipótesis Central
El desarrollo industrial del Este de Asia y el Pacífico (principalmente China) está influenciado por un tipo de cambio real depreciado (índice de nivel de precios bajo). Esto le permitió mejorar su PBI industrial y sus exportaciones manufactureras entre 2001 y 2022. En contraste, en Latinoamérica, el encarecimiento relativo de sus economías provocó una pérdida de competitividad. Esto derivó en un estancamiento de su PBI industrial durante el mismo período.

---

## 4. Justificación de las Bases de Datos Elegidas

Para contrastar la hipótesis con solidez metodológica se utilizaron dos fuentes públicas complementarias:

1. **World Development Indicators (WDI) — Banco Mundial:**
   - Provee series macroeconómicas armonizadas internacionalmente con cobertura para más de 200 países desde 1960.
   - Variables clave extraídas: PBI per cápita (USD constantes 2015), Índice de Nivel de Precios (`PA.NUS.GDP.PLI`, EE. UU. = 100), Empleo industrial (% del empleo total), Tasa de crecimiento del PBI, Ahorro bruto (% PBI), Rentas de recursos naturales (% PBI), Crédito privado (% PBI) y Exportaciones de manufacturas per cápita.
2. **Argendata (Fundar) / Schteingart, D. y Sidicaro, N. (2025):**
   - Base complementaria sectorial que sistematiza y armoniza las series de **PBI industrial per cápita** a precios constantes internacionales (en base a UNSTATS, INDEC, CEPAL y Fundación Norte y Sur).
   - Su incorporación resulta indispensable para resolver rupturas estadísticas y saltos espurios en las cuentas de manufactura de países clave (como China y economías del Cono Sur, contemplando el empalme metodológico argentino a precios de 2004).

---

## 5. Descripción de lo Realizado y Flujo Metodológico

El proyecto está diseñado bajo una arquitectura modular y 100% reproducible en R, organizada en etapas secuenciales:

1. **Adquisición (`script/01.datos.R`):**
   - Descarga automatizada vía API de WDI y consolidación con la base sectorial de Argendata (`auxiliar/pib_indust_per_capita_comparado.csv`), generando la base cruda `raw/wdi_tabla_completa.csv`.
2. **Filtrado y Curaduría (`script/02.filtrado.R`):**
   - Depuración de agregados institucionales y regionales del Banco Mundial.
   - Estandarización de variables, traducción de factores (regiones y niveles de ingreso) y generación del diccionario visual `output/tablas/diccionario_imagen.png`.
   - Generación de las bases intermedias `input/wdi_limpio.csv`, `input/wdi2.csv` (cortes 2001, 2008 y 2022) y `input/wdi3.csv` (con años adyacentes ±1 para interpolación).
3. **Tratamiento de Datos Faltantes e Imputación (`script/03.datos faltantes.R`):**
   - Exclusión de microestados con población inferior a 1 millón de habitantes (para evitar sesgos provocados por paraísos fiscales o economías minúsculas con alta volatilidad per cápita).
   - Diagnóstico sistemático de patrones de no-respuesta por variable.
   - Imputación mediante interpolación temporal por vecinos adyacentes (`utils/imputar_vecinos.R`) para los años 2001, 2008 y 2022, generando la base analítica consolidada `input/wdi_filtrado.csv` y la base de casos completos `input/wdi_final.csv`.
4. **Visualizaciones y Storytelling (`script/04.graficos.R`):**
   - Gráfico comunicacional de series de tiempo (`output/graficos/grafico_comunicativo.png`) que ilustra la trayectoria divergente del PBI industrial per cápita (1990–2023) entre China, Asia Oriental, América Latina y Argentina, identificando eventos históricos de quiebre (ingreso de China a la OMC en 2001 y la crisis financiera global de 2008).
   - Diagnóstico exploratorio univariado y de cobertura con histogramas estandarizados (`utils/hacer_hist.R`) exportados en `output/graficos/eda/`.
   - Mosaico exploratorio multianual estilo Our World in Data (`output/graficos/grafico_exploratorio.png`) articulado mediante `utils/panel_latam.R`, `utils/juntar.R` y `utils/theme_owid.R`.
5. **Análisis de Componentes Principales y Clustering (`script/05. PCA-Clustering.R`):**
   - Matriz de correlaciones entre las 11 variables activas estandarizadas (`output/graficos/clustering/heatmap.png`).
   - Reducción dimensional vía PCA con FactoMineR (`screeplot.png`, contribuciones factoriales `pc1.png` a `pc4.png`, análisis de sensibilidad de componentes).
   - Proyección de países en el plano factorial PC1-PC2 (`mapa_paises.png` y elipsoides regionales en `mapa_regiones.png`).
   - Clustering Jerárquico (Ward.D2) y Clasificación Jerárquica sobre Componentes Principales (HCPC) en 5 grupos (`silhouette.png`, `hcpc5.png`), caracterizando perfiles estructurales e identificando la posición relativa de Argentina y China.
6. **Modelado Econométrico y Diagnósticos (`script/06.regresion.R`):**
   - Estimación de modelos log-log para Industria y Exportaciones Manufactureras per cápita en 2001, 2008 y 2022.
   - Comparación de especificaciones: Modelos Simples, Con Controles (log(PBI per cápita), Ahorro bruto (% PBI) y Rentas de recursos naturales (% PBI)) y Modelos Robustos Sin Atípicos.
   - Batería exhaustiva de diagnósticos econométricos:
     - Test de Heterocedasticidad de Breusch-Pagan (`lmtest::bptest`).
     - Test adaptativo de especificación de Ramsey RESET (`utils/test_reset_adaptativo.R`), corrigiendo con matriz robusta $HC1$ ante heterocedasticidad.
     - Diagnóstico de multicolinealidad mediante Factores de Inflación de la Varianza (VIF < 5 en todas las covariables).
     - Detección de observaciones influyentes por Distancia de Cook (`utils/obtener_outliers_cook.R`) e isocontornos críticos ($D_i > 0,15$) plasmados en el panel de Residuos vs. Apalancamiento (`output/graficos/regresion/rvsl_exploratorio_completo.png`).
   - Exportación de tablas con formato de publicación (HTML y PNG) en `output/tablas/`.

---

## 6. Estructura de Carpetas del Repositorio

El repositorio respeta estrictamente la arquitectura modular requerida:

```
Trabajo-Practico-Ciencia-de-Datos/
├── raw/            # Datos originales descargados sin modificar
│   ├── wdi_tabla_completa.csv
│   └── pib_indust_per_capita_comparado.csv
├── auxiliar/       # Fuentes sectoriales complementarias de apoyo
│   └── pib_indust_per_capita_comparado.csv
├── input/          # Datos procesados y listos para los modelos cuantitativos
│   ├── wdi_limpio.csv
│   ├── wdi2.csv
│   ├── wdi3.csv
│   ├── wdi_filtrado.csv
│   ├── wdi_final.csv
│   └── wdi_regresion.csv
├── output/
│   ├── tablas/     # Salidas tabulares generadas en R (HTML y PNG)
│   │   ├── diccionario_imagen.png
│   │   ├── tabla_2001.html / .png
│   │   ├── tabla_2008.html / .png
│   │   ├── tabla_2022.html / .png
│   │   ├── tabla_resumen_modelos.html / .png
│   │   ├── tabla_diagnosticos_resumen.html / .png
│   │   ├── tabla_vif_final.html / .png
│   │   ├── tabla_bp_final.html / .png
│   │   ├── tabla_reset_comparativa.html / .png
│   │   └── tabla_sin_outliers.html / .png
│   └── graficos/   # Visualizaciones generadas por los scripts (PNG)
│       ├── grafico_comunicativo.png
│       ├── grafico_exploratorio.png
│       ├── eda/
│       │   ├── hist_exportaciones.png
│       │   ├── hist_industria.png
│       │   ├── hist_precios.png
│       │   ├── datos_faltanes_vars.png
│       │   ├── variacion_region.png
│       │   └── variacion_ingreso.png
│       ├── clustering/
│       │   ├── heatmap.png
│       │   ├── screeplot.png
│       │   ├── pc1.png a pc4.png
│       │   ├── mapa_paises.png
│       │   ├── mapa_regiones.png
│       │   ├── silhouette.png
│       │   └── hcpc5.png
│       └── regresion/
│           └── rvsl_exploratorio_completo.png
├── script/         # Scripts modulares de R ejecutables en orden cronológico
│   ├── 01.datos.R
│   ├── 02.filtrado.R
│   ├── 03.datos faltantes.R
│   ├── 04.graficos.R
│   ├── 05. PCA-Clustering.R
│   └── 06.regresion.R
├── utils/          # Funciones auxiliares propias modularizadas (un script por función)
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
│   ├── test_reset_adaptativo.R
│   ├── theme_owid.R
│   ├── top_cook.R
│   └── traducir_pais.R
└── README.md       # Descripción integral del proyecto y guía de reproducción
```

---

## 7. Instrucciones para Reproducir el Análisis

### Requisitos previos
- R versión >= 4.3.0 (desarrollado y testeado en R 4.3+ / 4.4+).
- RStudio (recomendado para visualización interactiva y renderizado de tablas `gt`).

### Instalación de librerías necesarias
Ejecutar la siguiente instrucción en la consola de R para instalar cualquier dependencia faltante:
```r
paquetes <- c(
  "tidyverse", "WDI", "scales", "ggtext", "ggrepel", "showtext",
  "patchwork", "modelsummary", "gt", "car", "sandwich", "lmtest",
  "FactoMineR", "factoextra", "cluster", "webshot2"
)
paquetes_faltantes <- setdiff(paquetes, installed.packages()[, "Package"])
if (length(paquetes_faltantes) > 0) {
  install.packages(paquetes_faltantes)
}
```

### Ejecución secuencial de scripts
Para reproducir todo el flujo de trabajo desde la descarga de datos primarios hasta las tablas y gráficos finales, ejecutar en la raíz del proyecto:

```r
# 1. Descarga y extracción de datos originales (WDI + Argendata)
source("script/01.datos.R")

# 2. Limpieza inicial, factores regionales y diccionario de datos
source("script/02.filtrado.R")

# 3. Filtro poblacional (>1M hab.) e imputación temporal por vecinos adyacentes
source("script/03.datos faltantes.R")

# 4. Generación de gráficos principales (comunicativo, exploratorio y EDA)
source("script/04.graficos.R")

# 5. Análisis multivariado de Componentes Principales (PCA) y Clustering (HCPC)
source("script/05. PCA-Clustering.R")

# 6. Modelado econométrico, diagnósticos de especificación y tablas
source("script/06.regresion.R")
```

---

## 8. Conclusiones Principales

La evidencia respalda parcialmente la hipótesis inicial. Si bien el gráfico de dispersión con curva de tendencia y las regresiones simples muestran una relación positiva entre el índice de nivel de precios, la producción industrial y las exportaciones manufactureras, al controlar por ingreso, recursos naturales y ahorro bruto la relación se invierte: en algunos años pierde significatividad, pero en otros confirma que, a igualdad de ingreso y estructura, un encarecimiento relativo del país perjudica las exportaciones manufactureras y, en menor medida, la producción industrial. Sin embargo, el tipo de cambio real no explica por sí solo la brecha entre regiones: en los gráficos exploratorios se observa que los países asiáticos sostienen altos niveles industriales y exportadores en distintos niveles de precios —incluso encareciéndose relativamente, como ocurre con China. En la misma línea, el PCA y el clustering jerárquico de 2022 muestran que Asia no es un bloque homogéneo, sino que presenta una fuerte dispersión a lo largo de las dimensiones (con economías como China, Japón y Corea del Sur ubicadas en el grupo desarrollado y otras en el intermedio, variando entre mayor desarrollo y mayores tasas de crecimiento y ahorro), mientras que América Latina permanece concentrada en torno a valores medios y agrupada en un perfil industrial intermedio, sin destacar en desarrollo estructural (Dim. 1) ni en ahorro y crecimiento industrial (Dim. 2).
