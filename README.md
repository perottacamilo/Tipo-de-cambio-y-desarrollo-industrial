# Tipo de cambio y desarrollo industrial

**Ciencia de Datos para Economía y Negocios** — Facultad de Ciencias Económicas, Universidad de Buenos Aires  
**Docente:** Nicolás Sidicaro — **Ayudante:** Maida Beltrán — 2do cuatrimestre 2026

## Integrantes

- Camilo Perotta
- Ignacio Keena
- Thomas Hannon

## Objetivo

Analizar la relación empírica entre el nivel relativo de precios (Índice de Nivel de Precios relativo a EE. UU.) y el desempeño productivo en términos de PBI industrial per cápita y exportaciones manufactureras per cápita a escala internacional.

**Hipótesis central:** El desarrollo industrial del Este de Asia y el Pacífico (principalmente China) está influenciado por un tipo de cambio real depreciado (índice de nivel de precios bajo). Esto le permitió mejorar su PBI industrial y sus exportaciones manufactureras entre 2001 y 2022. En contraste, en Latinoamérica, el encarecimiento relativo de sus economías provocó una pérdida de competitividad. Esto derivó en un estancamiento de su PBI industrial durante el mismo período.

## Datos

- **Fuente principal:** [World Development Indicators (WDI) — Banco Mundial](https://databank.worldbank.org/source/world-development-indicators) (descarga vía API con el paquete `WDI`).
- **Fuente complementaria:** [Industria — Argendata (Fundar)](https://argendata.fund.ar), Schteingart y Sidicaro (2025), para armonizar el PBI industrial per cápita y evitar rupturas estadísticas.
- **Período:** 1960–2025 (evolución 1990–2023 y cortes analíticos en 2001, 2008 y 2022).
- **Unidad de análisis:** países con población superior a 1 millón de habitantes.
- **Variables principales:** PBI industrial per cápita (`industria_per_capita`, USD const. 2015, Argendata `gdp_indust_pc` + WDI `NV.IND.MANF.ZS`), exportaciones manufactureras per cápita (`exportacion_manufactura_pc`, USD corr., construida con `TX.VAL.MANF.ZS.UN`, `TX.VAL.MRCH.CD.WT` y `SP.POP.TOTL`) e índice de nivel de precios (`indice_nivel_precios`, EE. UU. = 100, `PA.NUS.GDP.PLI`).
- **Variables estructurales y de control:** PBI per cápita (`pbi_per_capita`, `NY.GDP.PCAP.KD`), PBI en millones (`pbi_millones`, `NY.GDP.MKTP.KD`), crecimiento industrial (`crecimiento_industria`, `NV.IND.TOTL.KD.ZG`), empleo industrial (`empleo_industrial`, `SL.IND.EMPL.ZS`), valor agregado por trabajador industrial (`valor_agregado_trabajador`, `NV.IND.EMPL.KD`), rentas de recursos naturales (`recursos_naturales`, `NY.GDP.TOTL.RT.ZS`), ahorro bruto (`ahorro_bruto_pbi`, `NY.GNS.ICTR.ZS`), gasto de consumo del gobierno (`gasto_gobierno_pbi`, `NE.CON.GOVT.ZS`) y crédito privado (`credito_privado_pbi`, `FS.AST.PRVT.GD.ZS`).
- **Identificación y filtro:** `pais`, `iso3c`, `anio`, `region`, `ingreso` y población en millones (`poblacion_mill`, `SP.POP.TOTL`).
- **Variables descartadas (>40% de faltantes):** tipo de cambio real efectivo (`tipo_cambio_real_efectivo`, `PX.REX.REER`) y gasto total del gobierno (`gastos_totales_gob_pbi`, `GC.XPN.TOTL.GD.ZS`).

## Análisis realizado

1. Descarga de indicadores WDI vía API y empalme con la serie de PBI industrial per cápita de Argendata.
2. Limpieza de agregados, traducción de regiones e ingresos, construcción de variables per cápita y generación del diccionario de variables.
3. Exclusión de microestados (<1 millón de habitantes), depuración de variables y países con alta proporción de faltantes e interpolación por años adyacentes (±1) para 2001, 2008 y 2022.
4. Análisis exploratorio y visualizaciones: distribuciones, faltantes, gráfico de dispersión con curva de tendencia (2001, 2008 y 2022) y evolución del PBI industrial per cápita (1990–2023).
5. Análisis de Componentes Principales (PCA) sobre 11 variables estandarizadas y clustering jerárquico (Ward y HCPC en 5 grupos) para 2022.
6. Regresiones log-log (simples, con controles y sin atípicos) para industria y exportaciones en 2001, 2008 y 2022, con diagnósticos de heterocedasticidad (Breusch-Pagan, errores robustos HC1), multicolinealidad (VIF), especificación (RESET) y observaciones influyentes (Distancia de Cook > 0,15).

## Estructura del repositorio

```
Trabajo-Practico-Ciencia-de-Datos/
├── raw/              # Bases originales descargadas (WDI y Argendata)
├── auxiliar/         # Base complementaria de Argendata
├── input/            # Bases limpias, imputadas y listas para el análisis
├── output/
│   ├── tablas/       # Diccionario, regresiones y diagnósticos (HTML y PNG)
│   └── graficos/     # Gráficos comunicativo, exploratorio, EDA, clustering y regresión
├── script/
│   ├── 01.datos.R
│   ├── 02.filtrado.R
│   ├── 03.datos faltantes.R
│   ├── 04.graficos.R
│   ├── 05. PCA-Clustering.R
│   └── 06.regresion.R
├── utils/            # Funciones auxiliares propias (gráficos, imputación y diagnósticos)
└── README.md
```

## Reproducción

### Paquetes necesarios

```r
install.packages(c(
  "tidyverse", "WDI", "readxl", "scales", "ggtext", "ggrepel", "showtext",
  "patchwork", "FactoMineR", "factoextra", "cluster", "mclust",
  "lmtest", "sandwich", "car", "tseries", "modelsummary", "gt", "webshot2"
))
```

### Orden de ejecución

1. `script/01.datos.R` — Descarga los datos de WDI, los cruza con `auxiliar/` y guarda la base cruda en `raw/`.
2. `script/02.filtrado.R` — Limpia la base, construye variables per cápita, exporta el diccionario en `output/tablas/` y guarda las bases intermedias en `input/`.
3. `script/03.datos faltantes.R` — Aplica filtros de población y faltantes, interpola años adyacentes y genera `wdi_filtrado.csv` y `wdi_final.csv` en `input/`.
4. `script/04.graficos.R` — Genera los gráficos exploratorio, comunicativo y de EDA en `output/graficos/`.
5. `script/05. PCA-Clustering.R` — Ejecuta PCA y clustering jerárquico (Ward y HCPC) y exporta las visualizaciones en `output/graficos/clustering/`.
6. `script/06.regresion.R` — Estima los modelos log-log, realiza diagnósticos y exporta tablas y gráficos en `output/tablas/` y `output/graficos/regresion/`.

## Conclusiones principales

La evidencia respalda parcialmente la hipótesis inicial. Si bien el gráfico de dispersión con curva de tendencia y las regresiones simples muestran una relación positiva entre el índice de nivel de precios, la producción industrial y las exportaciones manufactureras, al controlar por ingreso, recursos naturales y ahorro bruto la relación se invierte: en algunos años pierde significatividad, pero en otros confirma que, a igualdad de ingreso y estructura, un encarecimiento relativo del país perjudica las exportaciones manufactureras y, en menor medida, la producción industrial. Sin embargo, el tipo de cambio real no explica por sí solo la brecha entre regiones: en los gráficos exploratorios se observa que los países asiáticos sostienen altos niveles industriales y exportadores en distintos niveles de precios —incluso encareciéndose relativamente, como ocurre con China. En la misma línea, el PCA y el clustering jerárquico de 2022 muestran que Asia no es un bloque homogéneo, sino que presenta una fuerte dispersión a lo largo de las dimensiones (con economías como China, Japón y Corea del Sur ubicadas en el grupo desarrollado y otras en el intermedio, variando entre mayor desarrollo y mayores tasas de crecimiento y ahorro), mientras que América Latina permanece concentrada en torno a valores medios y agrupada en un perfil industrial intermedio, sin destacar en desarrollo estructural (Dim. 1) ni en ahorro y crecimiento industrial (Dim. 2).
