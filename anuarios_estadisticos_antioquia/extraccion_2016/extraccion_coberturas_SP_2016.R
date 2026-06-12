# =============================================================================
# Extracción de coberturas de servicios públicos
# Fuente: Anuario Estadístico de Antioquia 2016, capítulo 13
# Municipios: corredor de Urabá (11 municipios)
# =============================================================================

library(readxl)
library(dplyr)
library(openxlsx)

# -----------------------------------------------------------------------------
# AJUSTE ENTRE AÑOS: cambiar ruta_entrada y anio al adaptar para otros años
# -----------------------------------------------------------------------------
ruta_entrada <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/13-servicios-publicos-2016_2026-06-11/13-servicios-publicos-2016/"
ruta_salida  <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/extraccion_2016/"
anio <- 2016

dir.create(ruta_salida, recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# Municipios objetivo
# -----------------------------------------------------------------------------
uraba_codes <- c("05045", "05051", "05147", "05172", "05475",
                 "05480", "05490", "05659", "05665", "05837", "05873")

uraba_nombres <- c(
  "05045" = "Apartadó",
  "05051" = "Arboletes",
  "05147" = "Carepa",
  "05172" = "Chigorodó",
  "05475" = "Murindó",
  "05480" = "Mutatá",
  "05490" = "Necoclí",
  "05659" = "San Juan de Urabá",
  "05665" = "San Pedro de Urabá",
  "05837" = "Turbo",
  "05873" = "Vigía del Fuerte"
)

# -----------------------------------------------------------------------------
# Lista de servicios: archivo fuente y prefijo de columnas
# AJUSTE ENTRE AÑOS: los nombres de archivo deberían mantenerse igual
# -----------------------------------------------------------------------------
servicios <- list(
  list(archivo = "SE13101.xlsx", prefijo = "agua_potable"),
  list(archivo = "SE13102.xlsx", prefijo = "acueducto"),
  list(archivo = "SE13103.xlsx", prefijo = "alcantarillado"),
  list(archivo = "SE13104.xlsx", prefijo = "energia"),
  list(archivo = "SE13106.xlsx", prefijo = "gas")
)

# -----------------------------------------------------------------------------
# Función de extracción
# DECISIÓN METODOLÓGICA: se leen columnas por posición (no por nombre) porque
# los encabezados del Excel tienen saltos de línea y espacios inconsistentes.
# Columnas: 4=dane_code, 7=pct_urb_con, 9=pct_urb_sin, 11=pct_rur_con,
#           13=pct_rur_sin, 15=pct_tot_con, 17=pct_tot_sin
# AJUSTE ENTRE AÑOS: verificar que las posiciones de columna no hayan cambiado
# -----------------------------------------------------------------------------
extraer_servicio <- function(path, prefijo) {

  raw <- read_xlsx(path, sheet = 1, skip = 1, col_names = TRUE)

  # DECISIÓN METODOLÓGICA: el código DANE viene como numérico desde Excel
  # (ej. 5045). Se convierte a string de 5 dígitos con cero a la izquierda.
  dane_raw <- raw[[4]]
  dane_code <- suppressWarnings(sprintf("%05d", as.integer(dane_raw)))

  # Filtrar solo filas con código DANE válido (descartar encabezados/totales)
  filas_validas <- !is.na(as.integer(dane_raw)) & dane_code %in% uraba_codes

  df <- data.frame(
    dane_code       = dane_code[filas_validas],
    pct_urb_con     = round(as.numeric(raw[[7]][filas_validas]),  4),
    pct_urb_sin     = round(as.numeric(raw[[9]][filas_validas]),  4),
    pct_rur_con     = round(as.numeric(raw[[11]][filas_validas]), 4),
    pct_rur_sin     = round(as.numeric(raw[[13]][filas_validas]), 4),
    pct_tot_con     = round(as.numeric(raw[[15]][filas_validas]), 4),
    pct_tot_sin     = round(as.numeric(raw[[17]][filas_validas]), 4),
    stringsAsFactors = FALSE
  )

  # Validación: municipios faltantes
  faltantes <- setdiff(uraba_codes, df$dane_code)
  if (length(faltantes) > 0) {
    warning(sprintf(
      "[%s] Municipios no encontrados en %s: %s",
      prefijo, basename(path), paste(faltantes, collapse = ", ")
    ))
  }

  # Validación: porcentajes fuera de rango
  cols_pct <- c("pct_urb_con", "pct_urb_sin", "pct_rur_con",
                "pct_rur_sin", "pct_tot_con", "pct_tot_sin")
  for (col in cols_pct) {
    vals <- df[[col]]
    fuera <- which(!is.na(vals) & (vals < 0 | vals > 100))
    if (length(fuera) > 0) {
      warning(sprintf(
        "[%s] Valores fuera de [0,100] en columna %s, filas: %s",
        prefijo, col, paste(df$dane_code[fuera], collapse = ", ")
      ))
    }
  }

  # Renombrar columnas con prefijo del servicio
  names(df)[names(df) != "dane_code"] <- paste0(prefijo, "_", names(df)[names(df) != "dane_code"])

  df
}

# -----------------------------------------------------------------------------
# Bloque principal: extraer, unir y exportar
# -----------------------------------------------------------------------------
lista_dfs <- lapply(servicios, function(s) {
  path <- file.path(ruta_entrada, s$archivo)
  message(sprintf("Procesando: %s  →  prefijo '%s'", s$archivo, s$prefijo))
  extraer_servicio(path, s$prefijo)
})

# DECISIÓN METODOLÓGICA: se usa left_join partiendo de uraba_codes para
# garantizar las 11 filas aunque algún servicio no tenga datos de un municipio.
base_df <- data.frame(dane_code = uraba_codes, stringsAsFactors = FALSE)

resultado <- Reduce(function(x, y) left_join(x, y, by = "dane_code"), c(list(base_df), lista_dfs))

# Agregar municipio y anio
resultado <- resultado %>%
  mutate(
    municipio = uraba_nombres[dane_code],
    anio      = anio
  ) %>%
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
# -----------------------------------------------------------------------------
archivo_salida <- file.path(ruta_salida, "coberturas_SP_2016.xlsx")

# AJUSTE ENTRE AÑOS: cambiar nombre del archivo y de la hoja
write.xlsx(resultado, file = archivo_salida, sheetName = "coberturas_2016", overwrite = TRUE)

message(sprintf("\nArchivo exportado: %s", archivo_salida))
