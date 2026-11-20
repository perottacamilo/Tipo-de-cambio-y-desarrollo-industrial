#Script Datos Faltantes (NAs)

# install.packages(c("dplyr", "tidyr"))

library(dplyr)
library(tidyr)
library(readxl)


rm(list = ls())

wdi2 <- read.csv("input/wdi2.csv")#2001 2008 2022

wdi3 <- read.csv("input/wdi3.csv")# 2000, 2001, 2002, 2007, 2008, 2009, 2021, 2022, 2023

#Creamos una base para cada anio
#Y eliminamos a los paises con menos de un millon de habitantes
#Debido a que incluye a muchos paises que no son relevantes para el trabajo (por que?)

wdi2 <- wdi2 |> 
filter(poblacion_mill > 1)

base_2001 <- wdi2 |>
  filter(anio == 2001)
base_2008 <- wdi2 |>
  filter(anio == 2008)
base_2022<- wdi2 |>
  filter(anio == 2022)


#Creamos bases para los bloques de tres anios
wdi3 <- wdi3 |> 
  filter(poblacion_mill > 1)

na_2001 <- wdi3 |> 
  filter(anio %in% c(2000, 2001, 2002))
na_2008 <- wdi3 |> 
  filter(anio %in% c(2007, 2008, 2009))
na_2022 <- wdi3 |> 
  filter(anio %in% c(2021, 2022, 2023))

variables <- c(
  "crecimiento_industria",
  "industria_per_capita",
  "tipo_cambio_real_efectivo",
  "indice_nivel_precios",
  "empleo_industrial",
  "exportacion_manufactura_brutas",
  "gasto_gobierno_pbi",
  "gastos_totales_gob_pbi",
  "valor_agregado_trabajador",
  "ahorro_bruto_pbi",
  "credito_privado_pbi",
  "recursos_naturales",
  "pbi_per_capita",
  "pbi_millones")





#Que porcentaje de NA tiene cada varible (en los tres anios elegidos)
faltantes <- wdi2 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))



#Proceentaje de datos faltantes por variable por cada anio
faltantes_2001 <- base_2001 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))
faltantes_2008 <- base_2008 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))

faltantes_2022 <- base_2022 |>
  summarise(across(everything(), ~ mean(is.na(.x)) * 100)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "pct_na") |>
  arrange(desc(pct_na))


#De la tabla "faltantes" vemos que hay dos variables que les falta mas del 40% de los datos
#tipo_cambio_real_efectivo -> 51.2%
#gastos_totales_gob_pbi -> 41.9%
#La segunda variable puede ser prescindible, aparte de sus datos faltantes, por ser muy parecida
#A otra variable tambien incluida, gasto_gobierno_pbi, que tiene menos cantidad de datos faltantes
#En cuanto a tipo_cambio_real_efectivo es necesaria su eliminacion ya que requeriria interpolar mas de la
#Mitad de los datos, lo cual implicaria que la mitad de los datos no serian "verdaderos" 
#Por lo que es mejor eliminarla, ademas de que no es escencial en nuestro analisis posterior













#Analisis de Paises sin datos en las variables seleccionadas -> industria_per_capita e indice_nivel_precios
wdi_analisis <- wdi2 |> filter(!is.na(industria_per_capita))
wdi_analisis2 <- wdi2 |> filter(!is.na(indice_nivel_precios))
wdi_analisis_3_ipbi <- wdi3 |> filter(!is.na(industria_per_capita))
wdi_analisis_3_inp <- wdi3 |> filter(!is.na(indice_nivel_precios))


#Cuales son aquellos paises que se no tienen industria_per_capita en ninguno de los tres anios
setdiff(wdi2$pais, wdi_analisis$pais)

#Cuales son aquellos paises que se no tienen indice_nivel_precios en ninguno de los tres anios
setdiff(wdi2$pais, wdi_analisis2$pais) 

setdiff(wdi3$pais, wdi_analisis_3_ipbi$pais) #Que paises no tienen industria_per_capita ni en ninguno de los 9 anios  

setdiff(wdi3$pais, wdi_analisis_3_inp$pais) #Que paises no tienen indice_nivel_precios ni en ninguno de los 9 anios  



#En primer lugar creo que podriamos quedarnos solo con los comandos que agrupan tanto (2001,2008,2022) como los otros 9 anios a la vez, en vez de analizar por separado anio a anio
#Ya que el pais resulta problematico si le falta tanto en los 3 como en los 9, no en uno en especifico
#El analisis en bloque como en los anios particulares arroja los mismos resultados
#Corea del Norte no tienen datos de industria_per_capita en ninguno de los 9 anios
#Por lo que no se puede interpolar sus datos usando los anios cercanos
#En cuanto a indice_nivel_precios ni Corea del Norte ni Cuba tienen datos en esos 9 anios
#Por lo tanto decidimos eliminarlos de la base


#Eliminando tipo_cambio_real_efectivo y gastos_totales_gob_pbi

wdi2.1 <- wdi2 |> 
  select(-tipo_cambio_real_efectivo, -gastos_totales_gob_pbi) |> 
  filter(!pais %in% c("Korea, Dem. People's Rep.", "Cuba"))

wdi3.1 <- wdi3 |> 
  select(-tipo_cambio_real_efectivo, -gastos_totales_gob_pbi) |> 
  filter(!pais %in% c("Korea, Dem. People's Rep.", "Cuba"))


base_2001.1 <- wdi2.1 |>
  filter(anio == 2001)
base_2008.1 <- wdi2.1 |>
  filter(anio == 2008)
base_2022.1<- wdi2.1 |>
  filter(anio == 2022)


na_2001.1 <- wdi3.1 |> 
  filter(anio %in% c(2000, 2001, 2002))
na_2008.1 <- wdi3.1 |> 
  filter(anio %in% c(2007, 2008, 2009))
na_2022.1 <- wdi3.1 |> 
  filter(anio %in% c(2021, 2022, 2023))



variables2 <- c(
  "crecimiento_industria",
  "industria_per_capita",
  "indice_nivel_precios",
  "empleo_industrial",
  "exportacion_manufactura_brutas",
  "gasto_gobierno_pbi",
  "valor_agregado_trabajador",
  "ahorro_bruto_pbi",
  "credito_privado_pbi",
  "recursos_naturales",
  "pbi_per_capita",
  "pbi_millones")





#Cantidad de variables faltantes por pais por cada anio (2001, 2008, 2022)
#Luego de eliminar las variables y los paises anteriores

faltantes_pais_2001 <- base_2001.1 |>
group_by(pais) |>
summarise(ingreso = first(ingreso2),
    observaciones = n() * length(variables2),
    faltantes = sum(
          across(all_of(variables2), ~ sum(is.na(.x)))),
          pct_faltantes = 100 * faltantes / observaciones,
          .groups = "drop") |>
arrange(desc(pct_faltantes))

print(faltantes_pais_2001, n = Inf)


faltantes_pais_2008 <- base_2008.1 |>
group_by(pais) |>
summarise(ingreso = first(ingreso2),
      observaciones = n() * length(variables2),
      faltantes = sum(
            across(all_of(variables2), ~ sum(is.na(.x)))),
            pct_faltantes = 100 * faltantes / observaciones,
            .groups = "drop") |>
  arrange(desc(pct_faltantes))

print(faltantes_pais_2008, n = Inf)


faltantes_pais_2022 <- base_2022.1 |>
group_by(pais) |>
summarise(ingreso = first(ingreso2),
      observaciones = n() * length(variables2),
      faltantes = sum(
            across(all_of(variables2), ~ sum(is.na(.x)))),
            pct_faltantes = 100 * faltantes / observaciones,
            .groups = "drop") |>
  arrange(desc(pct_faltantes))

print(faltantes_pais_2022, n = Inf)

head(faltantes_pais_2001)
head(faltantes_pais_2008)
head(faltantes_pais_2022)



#Aquellos paises que tengan un porcentaje alto de faltantes es necesario eliminarlo ya que implicaria
#Tener que interpolar una gran cantidad de datos del pais, lo que haria algo "artifical" al pais
#Para eso decidimos descartar aquellos paises con 6 o mas faltantes, lo que significa el 50% de las varibles
#En el caso del anio 2001 aquellos paises son Kosovo, Sudan del Sur, Afghanistan y Angola
#Para el anio 2008 todos los paise superan ese humbral, es decir, tienen como maximo 5 faltantes
#En 2022, quienes lo superan son Eritrea, Yemen, Sudan del Sur y Venezuela
#No incluyo Puerto Rico porque en este anio todos tienen faltante la variable recursos naturales
#Por un problema del World Bank, por lo que en realidad solo tendria 5 faltantes


wdi2.2 <- wdi2.1 |> 
  filter(!pais %in% c("Kosovo", "South Sudan", "Afghanistan", "Angola", "Yemen", "Eritrea", "Venezuela, RB"))

wdi3.2 <- wdi3.1 |> 
  filter(!pais %in% c("Kosovo", "South Sudan", "Afghanistan", "Angola", "Yemen", "Eritrea", "Venezuela, RB"))


base_2001.2 <- wdi2.2 |>
  filter(anio == 2001)
base_2008.2 <- wdi2.2 |>
  filter(anio == 2008)
base_2022.2<- wdi2.2 |>
  filter(anio == 2022)


na_2001.2 <- wdi3.2 |> 
  filter(anio %in% c(2000, 2001, 2002))
na_2008.2 <- wdi3.2 |> 
  filter(anio %in% c(2007, 2008, 2009))
na_2022.2 <- wdi3.2 |> 
  filter(anio %in% c(2021, 2022, 2023))

#Con el resto de NAs vamos a interpolar los anios de interes con el promedio de los anios lindantes
#Si alguno de los dos anios lindantes llegara a tener tambien NA, entonces la observacion va a ser descartada
# Promedio de vecinos; si falta uno, usa el otro; si faltan ambos, NA
#nuevo
imputar_vecinos <- function(x, anio) {
  ant <- if_else(lag(anio) == anio - 1, lag(x), NA_real_)
  sig <- if_else(lead(anio) == anio + 1, lead(x), NA_real_)
  case_when(
    !is.na(ant) & !is.na(sig) ~ (ant + sig) / 2,
    !is.na(ant)               ~ ant,
    !is.na(sig)               ~ sig,
    TRUE                      ~ NA_real_
  )
}


wdi_filtrado <- wdi3.2 |> 
  group_by(pais) |> 
  arrange(pais, anio) |> 
  mutate(
    across(
      .cols = c(
        crecimiento_industria,
        industria_per_capita,
        indice_nivel_precios,
        empleo_industrial,
        exportacion_manufactura_brutas,
        gasto_gobierno_pbi,
        valor_agregado_trabajador,
        ahorro_bruto_pbi,
        credito_privado_pbi,
        pbi_per_capita,
        pbi_millones,
        industria_pc),
      .fns = ~ if_else(
        condition = is.na(.x) & anio %in% c(2001, 2008, 2022),
        true = imputar_vecinos(.x, anio),
        false = .x)),
    recursos_naturales = case_when(
      anio == 2022 ~ if_else(lag(anio) == anio - 1, lag(recursos_naturales), NA_real_),
      is.na(recursos_naturales) & anio %in% c(2001, 2008) ~ imputar_vecinos(recursos_naturales, anio),
      TRUE ~ recursos_naturales)) |> 
  ungroup() |> 
  filter(anio %in% c(2001, 2008, 2022))

write.csv(wdi_filtrado, "input/wdi_filtrado.csv")


wdi_final <- wdi_filtrado |> 
  drop_na(
    crecimiento_industria,
    industria_per_capita,
    indice_nivel_precios,
    empleo_industrial,
    exportacion_manufactura_brutas,
    gasto_gobierno_pbi,
    valor_agregado_trabajador,
    ahorro_bruto_pbi,
    credito_privado_pbi,
    recursos_naturales,
    pbi_per_capita,
    pbi_millones)

write.csv(wdi_final, "input/wdi_final.csv")



reporte_eliminados <- anti_join(wdi_filtrado, wdi_final, by = c("pais", "anio")) |> 
  select(pais, anio, 
         crecimiento_industria,
         industria_per_capita,
         indice_nivel_precios,
         empleo_industrial,
         exportacion_manufactura_brutas,
         gasto_gobierno_pbi,
         valor_agregado_trabajador,
         ahorro_bruto_pbi,
         credito_privado_pbi,
         recursos_naturales,
         pbi_per_capita,
         pbi_millones, 
         region, 
         ingreso) |> 
  arrange(pais, anio)

#Decidir si cuando se hace drop.na() se eligen todas las variables
#En tal caso la base wdi_final no va a tener ningun NA
#O si se elimina si hay NA en las variables de interes (i.e. las de la regresion)

#En el caso del clustering si es necesario que no haya ningun NA

print(reporte_eliminados, n = Inf)


colSums(is.na(wdi_filtrado))


#Filtramos (drop.na) solo en las variables que ustilizamos en la regresion
#Para quedarnos con la mayor cantidad de observaciones posibles

#Elegir vars regresion
wdi_regresion <- wdi_filtrado |> 
  drop_na(
    crecimiento_industria,
    industria_per_capita,
    indice_nivel_precios,
    empleo_industrial,
    exportacion_manufactura_brutas,
    gasto_gobierno_pbi,
    valor_agregado_trabajador,
    ahorro_bruto_pbi,
    credito_privado_pbi,
    recursos_naturales,
    pbi_per_capita,
    pbi_millones)

write.csv(wdi_regresion, "input/wdi_regresion.csv")

