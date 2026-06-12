# =============================================================================
# Extracción de coberturas de servicios públicos
# Fuente: Anuario Estadístico de Antioquia 2019, capítulo 13
# Municipios: corredor de Urabá (11 municipios)
# =============================================================================

library(readxl)
library(dplyr)
library(openxlsx)

# -----------------------------------------------------------------------------
# AJUSTE ENTRE AÑOS: cambiar ruta_entrada y anio al adaptar para otros años
# -----------------------------------------------------------------------------
ruta_entrada <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/13-servicios-publicos-2019_2026-06-11/13-servicios-publicos-2019/"
ruta_salida  <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/extraccion_2019/"
anio <- 2019

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
# Lista de servicios
#
# AJUSTE ENTRE AÑOS: en 2019 los archivos usan punto (SP.10.X.xlsx).
# AJUSTE ENTRE AÑOS: la fila de encabezado varía por archivo (skip).
# ADVERTENCIA: SP.10.4.xlsx (energía) reporta coberturas como proporción
#   (0–1), no como porcentaje (0–100). Se usa factor = 100 para convertir.
#   Además, las columnas de cobertura están en posiciones 7/8/9 en lugar
#   de 13/14/15 como en los demás archivos.
# -----------------------------------------------------------------------------
servicios <- list(
  list(archivo = "SP.10.1.xlsx", prefijo = "agua_potable",   skip = 2,
       col_tot = 13, col_urb = 14, col_rur = 15, factor = 1),
  list(archivo = "SP.10.2.xlsx", prefijo = "acueducto",      skip = 2,
       col_tot = 13, col_urb = 14, col_rur = 15, factor = 1),
  list(archivo = "SP.10.3.xlsx", prefijo = "alcantarillado", skip = 2,
       col_tot = 13, col_urb = 14, col_rur = 15, factor = 1),
  list(archivo = "SP.10.4.xlsx", prefijo = "energia",        skip = 1,
       col_tot = 7,  col_urb = 8,  col_rur = 9,  factor = 100),
  list(archivo = "SP.10.6.xlsx", prefijo = "gas",            skip = 2,
       col_tot = 13, col_urb = 14, col_rur = 15, factor = 1)
)

# -----------------------------------------------------------------------------
# Función de extracción
# DECISIÓN METODOLÓGICA: se leen columnas por posición porque los encabezados
# tienen formato heterogéneo (saltos de línea, celdas combinadas).
# DECISIÓN METODOLÓGICA: el parámetro `factor` permite normalizar la energía,
# cuyas coberturas están expresadas como proporción 0-1 en el archivo fuente.
# DECISIÓN METODOLÓGICA: las variables SIN servicio se calculan como
#   pct_sin = 100 - pct_con, no son reportadas directamente por la fuente.
# -----------------------------------------------------------------------------
extraer_servicio <- function(path, prefijo, skip, col_tot, col_urb, col_rur, factor) {

  raw <- read_xlsx(path, sheet = 1, skip = skip, col_names = TRUE)

  # DECISIÓN METODOLÓGICA: el código DANE (columna 5) se lee como string y se
  # normaliza a 5 dígitos con cero a la izquierda.
  dane_raw  <- raw[[5]]
  dane_code <- suppressWarnings(sprintf("%05d", as.integer(dane_raw)))

  filas_validas <- !is.na(suppressWarnings(as.integer(dane_raw))) &
                   dane_code %in% uraba_codes

  df <- data.frame(
    dane_code   = dane_code[filas_validas],
    pct_tot_con = round(as.numeric(raw[[col_tot]][filas_validas]) * factor, 4),
    pct_urb_con = round(as.numeric(raw[[col_urb]][filas_validas]) * factor, 4),
    pct_rur_con = round(as.numeric(raw[[col_rur]][filas_validas]) * factor, 4),
    stringsAsFactors = FALSE
  )

  # Calcular complementos
  df <- df %>%
    mutate(
      pct_tot_sin = round(100 - pct_tot_con, 4),
      pct_urb_sin = round(100 - pct_urb_con, 4),
      pct_rur_sin = round(100 - pct_rur_con, 4)
    )

  # Validación: municipios faltantes
  faltantes <- setdiff(uraba_codes, df$dane_code)
  if (length(faltantes) > 0) {
    warning(sprintf(
      "[%s] Municipios no encontrados en %s: %s",
      prefijo, basename(path), paste(faltantes, collapse = ", ")
    ))
  }

  # Validación: porcentajes (tras aplicar factor) fuera de rango
  for (col in c("pct_tot_con", "pct_urb_con", "pct_rur_con")) {
    vals  <- df[[col]]
    fuera <- which(!is.na(vals) & (vals < 0 | vals > 100))
    if (length(fuera) > 0) {
      warning(sprintf(
        "[%s] Valores fuera de [0,100] en %s, municipios: %s",
        prefijo, col, paste(df$dane_code[fuera], collapse = ", ")
      ))
    }
  }

  # Seleccionar y renombrar con prefijo (orden: urb, rur, tot)
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
  message(sprintf("Procesando: %s  →  prefijo '%s'  (skip=%d, cols=%d/%d/%d, factor=%g)",
                  s$archivo, s$prefijo, s$skip,
                  s$col_tot, s$col_urb, s$col_rur, s$factor))
  extraer_servicio(path, s$prefijo, s$skip, s$col_tot, s$col_urb, s$col_rur, s$factor)
})

# DECISIÓN METODOLÓGICA: se parte de uraba_codes para garantizar las 11 filas
# aunque algún servicio no tenga datos de un municipio.
base_df <- data.frame(dane_code = uraba_codes, stringsAsFactors = FALSE)

resultado <- Reduce(
  function(x, y) left_join(x, y, by = "dane_code"),
  c(list(base_df), lista_dfs)
)

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
# AJUSTE ENTRE AÑOS: cambiar nombre del archivo y de la hoja
# -----------------------------------------------------------------------------
archivo_salida <- file.path(ruta_salida, "coberturas_SP_2019.xlsx")

write.xlsx(resultado, file = archivo_salida, sheetName = "coberturas_2019", overwrite = TRUE)

message(sprintf("\nArchivo exportado: %s", archivo_salida))
