# =============================================================================
# Extracción de coberturas de servicios públicos
# Fuente: Anuario Estadístico de Antioquia 2018, capítulo 13
# Municipios: corredor de Urabá (11 municipios)
#
# AJUSTE ENTRE AÑOS: 2018 corresponde al año del Censo Nacional de Población
# y Vivienda. El formato de los archivos cambió completamente respecto a
# 2016/2017:
#   1. No hay columna de código DANE; la identificación es por nombre.
#   2. Solo se reportan 3 variables (Total, Cabecera, Resto) sin complemento
#      "sin servicio". Los valores SIN se calculan como 100 - CON.
#   3. El denominador son viviendas ocupadas (Censo 2018), no estimadas.
#      Esto introduce una ruptura en la comparabilidad de la serie.
# =============================================================================

library(readxl)
library(dplyr)
library(openxlsx)

# -----------------------------------------------------------------------------
# AJUSTE ENTRE AÑOS: cambiar ruta_entrada y anio al adaptar para otros años
# -----------------------------------------------------------------------------
ruta_entrada <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/13-servicios-publicos-2018_2026-06-11/13-servicios-publicos-2018/"
ruta_salida  <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/extraccion_2018/"
anio <- 2018

dir.create(ruta_salida, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Municipios objetivo
# DECISIÓN METODOLÓGICA: en 2018 los archivos no contienen código DANE,
# solo el nombre del municipio (columna 1). El código se asigna desde el
# diccionario interno, no del archivo fuente.
# -----------------------------------------------------------------------------
uraba_nombres <- c(
  "Apartadó", "Arboletes", "Carepa", "Chigorodó", "Murindó",
  "Mutatá", "Necoclí", "San Juan de Urabá", "San Pedro de Urabá",
  "Turbo", "Vigía del Fuerte"
)

dane_codes <- c(
  "Apartadó"           = "05045",
  "Arboletes"          = "05051",
  "Carepa"             = "05147",
  "Chigorodó"          = "05172",
  "Murindó"            = "05475",
  "Mutatá"             = "05480",
  "Necoclí"            = "05490",
  "San Juan de Urabá"  = "05659",
  "San Pedro de Urabá" = "05665",
  "Turbo"              = "05837",
  "Vigía del Fuerte"   = "05873"
)

# -----------------------------------------------------------------------------
# Lista de servicios
# AJUSTE ENTRE AÑOS: en 2018 los archivos usan punto (SP10.X.xlsx).
# El archivo de gas es SP10.6.xlsx (en 2017 era SP10.7.xlsx).
# No hay campo skip porque el filtro se hace por nombre, no por posición de fila.
# -----------------------------------------------------------------------------
servicios <- list(
  list(archivo = "SP10.1.xlsx", prefijo = "agua_potable"),
  list(archivo = "SP10.2.xlsx", prefijo = "acueducto"),
  list(archivo = "SP10.3.xlsx", prefijo = "alcantarillado"),
  list(archivo = "SP10.4.xlsx", prefijo = "energia"),
  list(archivo = "SP10.6.xlsx", prefijo = "gas")
)

# -----------------------------------------------------------------------------
# Función de extracción
# DECISIÓN METODOLÓGICA: se lee el archivo sin encabezados (col_names=FALSE)
# y se filtra por nombre de municipio (columna 1 tras str_trim). Esto es
# robusto al número de filas de encabezado, que varía entre archivos.
# DECISIÓN METODOLÓGICA: columna 2 = % Total, columna 3 = % Cabecera (urbano),
# columna 4 = % Resto (rural). Las variables SIN se calculan como 100 - CON
# porque la fuente solo reporta coberturas positivas.
# ADVERTENCIA: los valores pct_urb_sin, pct_rur_sin y pct_tot_sin son
# calculados, no reportados directamente por la fuente.
# -----------------------------------------------------------------------------
extraer_servicio <- function(path, prefijo) {

  raw <- read_xlsx(path, sheet = 1, col_names = FALSE)

  # Filtrar filas cuyo nombre de municipio coincide exactamente con uraba_nombres
  nombre_col <- trimws(as.character(raw[[1]]))
  filas_validas <- nombre_col %in% uraba_nombres

  df <- data.frame(
    municipio   = nombre_col[filas_validas],
    pct_tot_con = round(as.numeric(raw[[2]][filas_validas]), 4),
    pct_urb_con = round(as.numeric(raw[[3]][filas_validas]), 4),
    pct_rur_con = round(as.numeric(raw[[4]][filas_validas]), 4),
    stringsAsFactors = FALSE
  )

  # Calcular complementos (variables no reportadas directamente en 2018)
  df <- df %>%
    mutate(
      pct_tot_sin = round(100 - pct_tot_con, 4),
      pct_urb_sin = round(100 - pct_urb_con, 4),
      pct_rur_sin = round(100 - pct_rur_con, 4)
    )

  # Asignar dane_code desde diccionario interno
  df$dane_code <- dane_codes[df$municipio]

  # Validación: municipios faltantes
  faltantes <- setdiff(uraba_nombres, df$municipio)
  if (length(faltantes) > 0) {
    warning(sprintf(
      "[%s] Municipios no encontrados en %s: %s",
      prefijo, basename(path), paste(faltantes, collapse = ", ")
    ))
  }

  # Validación: porcentajes de entrada fuera de rango
  for (col in c("pct_tot_con", "pct_urb_con", "pct_rur_con")) {
    vals  <- df[[col]]
    fuera <- which(!is.na(vals) & (vals < 0 | vals > 100))
    if (length(fuera) > 0) {
      warning(sprintf(
        "[%s] Valores fuera de [0,100] en columna %s, municipios: %s",
        prefijo, col, paste(df$municipio[fuera], collapse = ", ")
      ))
    }
  }

  # Seleccionar y renombrar con prefijo (orden final: urb, rur, tot)
  df_out <- df %>%
    select(dane_code,
           pct_urb_con, pct_urb_sin,
           pct_rur_con, pct_rur_sin,
           pct_tot_con, pct_tot_sin)

  names(df_out)[names(df_out) != "dane_code"] <-
    paste0(prefijo, "_", names(df_out)[names(df_out) != "dane_code"])

  df_out
}

# -----------------------------------------------------------------------------
# Bloque principal: extraer, unir y exportar
# -----------------------------------------------------------------------------
lista_dfs <- lapply(servicios, function(s) {
  path <- file.path(ruta_entrada, s$archivo)
  message(sprintf("Procesando: %s  →  prefijo '%s'", s$archivo, s$prefijo))
  extraer_servicio(path, s$prefijo)
})

# DECISIÓN METODOLÓGICA: se parte de dane_codes para garantizar las 11 filas
# aunque algún servicio no tenga datos de un municipio.
base_df <- data.frame(
  dane_code = unname(dane_codes),
  municipio = names(dane_codes),
  stringsAsFactors = FALSE
)

resultado <- Reduce(
  function(x, y) left_join(x, y, by = "dane_code"),
  c(list(base_df), lista_dfs)
)

# Agregar anio y reordenar columnas
resultado <- resultado %>%
  mutate(anio = anio) %>%
  select(
    dane_code, municipio, anio,
    starts_with("agua_potable_"),
    starts_with("acueducto_"),
    starts_with("alcantarillado_"),
    starts_with("energia_"),
    starts_with("gas_")
  )

# -----------------------------------------------------------------------------
# Diagnóstico
# -----------------------------------------------------------------------------
message("\n--- Vista previa del resultado ---")
dplyr::glimpse(resultado)
message(sprintf("\nDimensiones: %d filas × %d columnas", nrow(resultado), ncol(resultado)))

# -----------------------------------------------------------------------------
# Exportar a Excel
# AJUSTE ENTRE AÑOS: cambiar nombre del archivo y de la hoja
# -----------------------------------------------------------------------------
archivo_salida <- file.path(ruta_salida, "coberturas_SP_2018.xlsx")

write.xlsx(resultado, file = archivo_salida, sheetName = "coberturas_2018", overwrite = TRUE)

message(sprintf("\nArchivo exportado: %s", archivo_salida))
