# ==============================================================================
# Función: traducir_pais
# Objetivo: Traducir y abreviar nombres de países del Banco Mundial (WDI) al español
# ==============================================================================

paises_espanol <- c(
  "Congo, Rep."               = "Congo",
  "Congo, Dem. Rep."          = "R. D. Congo",
  "Sierra Leone"              = "Sierra Leona",
  "Sudan"                     = "Sudán",
  "Hong Kong SAR, China"      = "Hong Kong",
  "Iraq"                      = "Irak",
  "Libya"                     = "Libia",
  "Gambia, The"               = "Gambia",
  "Yemen, Rep."               = "Yemen",
  "Algeria"                   = "Argelia",
  "Cambodia"                  = "Camboya",
  "Oman"                      = "Omán",
  "Timor-Leste"               = "Timor Oriental",
  "Botswana"                  = "Botsuana",
  "Burundi"                   = "Burundi",
  "Paraguay"                  = "Paraguay",
  "Nepal"                     = "Nepal",
  "Zambia"                    = "Zambia",
  "Egypt, Arab Rep."          = "Egipto",
  "Iran, Islamic Rep."        = "Irán",
  "Syrian Arab Republic"      = "Siria",
  "Russian Federation"        = "Rusia",
  "United States"             = "Estados Unidos",
  "United Kingdom"            = "Reino Unido",
  "Germany"                   = "Alemania",
  "France"                    = "Francia",
  "Italy"                     = "Italia",
  "Spain"                     = "España",
  "Korea, Rep."               = "Corea del Sur",
  "Korea, Dem. People's Rep." = "Corea del Norte",
  "Venezuela, RB"             = "Venezuela"
)

traducir_pais <- function(x) {
  unname(ifelse(x %in% names(paises_espanol), paises_espanol[x], x))
}
