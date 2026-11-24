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

### Diccionario de variables

| Variable | Descripción | Tipo | Unidad | Rango teórico | NA totales | NA 2001 | NA 2008 | NA 2022 |
| :--- | :--- | :---: | :---: | :---: | :---: | :---: | :---: | :---: |
| `pais` | Nombre del país | Categórica | - | - | 0 | 0 | 0 | 0 |
| `iso3c` | Código ISO de 3 letras (identificador único) | Categórica | - | - | 0 | 0 | 0 | 0 |
| `region` | Región geográfica del Banco Mundial | Categórica | - | 7 categorías | 0 | 0 | 0 | 0 |
| `anio` | Año de la observación | Numérica | Anual | 66 años | 0 | 0 | 0 | 0 |
| `pbi_millones` | PBI del país en millones de dólares | Numérica | Dólares constantes 2015 | > 0 | 2789 | 18 | 9 | 9 |
| `pbi_per_capita` | PBI per cápita | Numérica | Dólares constantes 2015 | > 0 | 2789 | 18 | 9 | 9 |
| `indice_nivel_precios` | Factor de Paridad de Poder Adquisitivo (PPA) sobre el tipo de cambio nominal multiplicado por 100 | Numérica | Índice (EE.UU. = 100) | > 0 | 7299 | 23 | 17 | 18 |
| `industria_per_capita` | PBI industrial per cápita | Numérica | Dólares constantes 2015 | > 0 | 3047 | 12 | 8 | 6 |
| `crecimiento_industria` | Variación anual del sector industrial | Numérica | % de crecimiento | Sin límite | 6232 | 48 | 35 | 24 |
| `industria_pbi` | Valor agregado de la industria manufacturera como porcentaje del PBI | Numérica | % del PBI | 0 a 100 | 6069 | 47 | 29 | 28 |
| `tipo_cambio_real_efectivo` | Nivel de competitividad con base = 2010 | Numérica | Índice | > 0 | 10219 | 123 | 123 | 126 |
| `recursos_naturales` | Rentas económicas provenientes de los recursos naturales | Numérica | % del PBI | 0 a 100 | 5087 | 17 | 10 | 217 |
| `empleo_industrial` | Empleo en el sector industrial | Numérica | % del total de empleados | 0 a 100 | 7791 | 30 | 30 | 31 |
| `ingreso` | Clasificación de ingreso per cápita del país | Categórica | - | 5 categorías | 0 | 0 | 0 | 0 |
| `exportacion_manufactura` | Porcentaje de exportaciones manufactureras sobre el total de mercancías | Numérica | % del total | 0 a 100 | 6282 | 53 | 50 | 53 |
| `gasto_gobierno_pbi` | Gasto de consumo final del gobierno | Numérica | % del PBI | 0 a 100 | 5593 | 58 | 46 | 41 |
| `gastos_totales_gob_pbi` | Gasto total del gobierno como % del PBI (incluye subsidios) | Numérica | % del PBI | 0 a 100 | 9870 | 126 | 100 | 106 |
| `valor_agregado_trabajador` | Valor agregado por trabajador industrial | Numérica | Dólares constantes 2015 | > 0 | 8735 | 61 | 46 | 44 |
| `ahorro_bruto_pbi` | Ahorro bruto nacional | Numérica | % del PBI | 0 a 100 | 7845 | 89 | 61 | 58 |
| `credito_privado_pbi` | Crédito interno al sector privado | Numérica | % del PBI | 0 a 100 | 6648 | 76 | 54 | 55 |
| `industria_pc` | PBI industrial per cápita (serie Argendata) | Numérica | Dólares constantes 2015 | > 0 | 3882 | 15 | 11 | 11 |
| `poblacion_mill` | Población del país en millones de habitantes | Numérica | Millones de habitantes | > 0 | 0 | 0 | 0 | 0 |
| `industria_pc_wdi` | PBI industrial per cápita estimado con indicadores WDI | Numérica | Dólares constantes 2015 | > 0 | 6167 | 50 | 30 | 29 |
| `exportacion_manufactura_pc` | Exportaciones de manufacturas per cápita | Numérica | Dólares corrientes | >= 0 | 6406 | 57 | 53 | 54 |

*Nota: en `script/03.datos faltantes.R` se descartan `tipo_cambio_real_efectivo` y `gastos_totales_gob_pbi` por superar el 40% de faltantes.*

## Análisis realizado

1. Descarga de indicadores WDI vía API y empalme con la serie de PBI industrial per cápita de Argendata.
2. Limpieza de agregados, traducción de regiones e ingresos, construcción de variables per cápita y generación del diccionario de variables.
3. Exclusión de microestados y paises con menos de un millón de habitantes, depuración de variables y países con alta proporción de faltantes e interpolación por años adyacentes (±1) para 2001, 2008 y 2022.
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
│   ├── 01_datos.R
│   ├── 02_filtrado.R
│   ├── 03_datos_faltantes.R
│   ├── 04_graficos.R
│   ├── 05_pca_clustering.R
│   └── 06_regresion.R
├── utils/            # Funciones auxiliares propias (gráficos, imputación y diagnósticos)
└── README.md
```

## Reproducción

El procesamiento de datos, análisis cuantitativo y generación de visualizaciones se realizaron íntegramente en **R** (mediante **RStudio**), para la correcta ejecución del trabajo:

1. Clonar el repositorio.
2. Abrir el archivo `.Rproj` (necesario para que funcionen las rutas relativas).
3. Instalar los paquetes necesarios si hace falta:

```r
install.packages(c(
  "tidyverse", "WDI", "scales", "ggtext", "ggrepel", "showtext",
  "patchwork", "FactoMineR", "factoextra", "cluster", "here",
  "lmtest", "sandwich", "car", "modelsummary", "gt", "webshot2"
))
```

### Orden de ejecución

1. `script/01_datos.R` — Descarga los datos de WDI, los cruza con `auxiliar/` y guarda la base cruda en `raw/`.
2. `script/02_filtrado.R` — Limpia la base, construye variables per cápita, exporta el diccionario en `output/tablas/` y guarda las bases intermedias en `input/`.
3. `script/03_datos_faltantes.R` — Aplica filtros de población y faltantes, interpola años adyacentes y genera `wdi_filtrado.csv` y `wdi_final.csv` en `input/`.
4. `script/04_graficos.R` — Genera los gráficos exploratorio, comunicativo y de EDA en `output/graficos/`.
5. `script/05_pca_clustering.R` — Ejecuta PCA y clustering jerárquico (Ward y HCPC) y exporta las visualizaciones en `output/graficos/clustering/`.
6. `script/06_regresion.R` — Estima los modelos log-log, realiza diagnósticos y exporta tablas y gráficos en `output/tablas/` y `output/graficos/regresion/`.

## Conclusiones principales

La evidencia respalda parcialmente la hipótesis inicial. Si bien el gráfico de dispersión con curva de tendencia y las regresiones simples muestran una relación positiva entre el índice de nivel de precios, la producción industrial y las exportaciones manufactureras, al controlar por ingreso, recursos naturales y ahorro bruto la relación se invierte: en algunos años pierde significatividad, pero en otros confirma que, a igualdad de ingreso y estructura, un encarecimiento relativo del país perjudica las exportaciones manufactureras y, en menor medida, la producción industrial. Sin embargo, el tipo de cambio real no explica por sí solo la brecha entre regiones: en los gráficos exploratorios se observa que los países asiáticos sostienen altos niveles industriales y exportadores en distintos niveles de precios —incluso encareciéndose relativamente, como ocurre con China. En la misma línea, el PCA y el clustering jerárquico de 2022 muestran que Asia no es un bloque homogéneo, sino que presenta una fuerte dispersión a lo largo de las dimensiones (con economías como China, Japón y Corea del Sur ubicadas en el grupo desarrollado y otras en el intermedio, variando entre mayor desarrollo y mayores tasas de crecimiento y ahorro), mientras que América Latina permanece concentrada en torno a valores medios y agrupada en un perfil industrial intermedio, sin destacar en desarrollo estructural (Dim. 1) ni en ahorro y crecimiento industrial (Dim. 2).
