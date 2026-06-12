# =============================================================================
# Extracción de coberturas de servicios públicos
# Fuente: Anuario Estadístico de Antioquia 2020, capítulo 13
# Municipios: corredor de Urabá (11 municipios)
# =============================================================================

library(readxl)
library(dplyr)
library(openxlsx)

# -----------------------------------------------------------------------------
# AJUSTE ENTRE AÑOS: cambiar ruta_entrada y anio al adaptar para otros años
# -----------------------------------------------------------------------------
ruta_entrada <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/13-servicios-publicos-2020_2026-06-11/13-servicios-publicos-2020/"
ruta_salida  <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/extraccion_2020/"
anio <- 2020

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
# AJUSTE ENTRE AÑOS: en 2020 los archivos se llaman 13.6.X.xlsx (con puntos).
# AJUSTE ENTRE AÑOS: la fila de encabezado varía:
#   - Agua, acueducto, alcantarillado: skip = 1 (encabezado en fila 2)
#   - Energía, gas:                    skip = 2 (encabezado en fila 3)
# AJUSTE ENTRE AÑOS: el código DANE está en columna 1 (en 2019 estaba en col 5).
# Las coberturas están en columnas 5 (Total), 6 (Cabecera), 7 (Resto)
# en todos los archivos, y ya vienen como porcentaje (0–100).
# ADVERTENCIA: Murindó (05475) y Vigía del Fuerte (05873) tienen "…"
#   en el archivo de energía; se importan como NA automáticamente.
# -----------------------------------------------------------------------------
servicios <- list(
  list(archivo = "13.6.1.xlsx", prefijo = "agua_potable",   skip = 1),
  list(archivo = "13.6.2.xlsx", prefijo = "acueducto",      skip = 1),
  list(archivo = "13.6.3.xlsx", prefijo = "alcantarillado", skip = 1),
  list(archivo = "13.6.4.xlsx", prefijo = "energia",        skip = 2),
  list(archivo = "13.6.6.xlsx", prefijo = "gas",            skip = 2)
)

# -----------------------------------------------------------------------------
# Función de extracción
# DECISIÓN METODOLÓGICA: se leen columnas por posición (col 1 = DANE,
#   cols 5/6/7 = % Total/Cabecera/Resto). Esto es robusto a los encabezados
#   variables entre archivos.
# DECISIÓN METODOLÓGICA: las variables SIN servicio se calculan como
#   complemento (100 - pct_con). Para filas con NA (energía en Murindó
#   y Vigía del Fuerte), el complemento también queda NA sin imputación.
# -----------------------------------------------------------------------------
extraer_servicio <- function(path, prefijo, skip) {

  raw <- read_xlsx(path, sheet = 1, skip = skip, col_names = TRUE)

  # DECISIÓN METODOLÓGICA: código DANE en columna 1; se normaliza a 5 dígitos.
  dane_raw  <- raw[[1]]
  dane_code <- suppressWarnings(sprintf("%05d", as.integer(dane_raw)))

  filas_validas <- !is.na(suppressWarnings(as.integer(dane_raw))) &
                   dane_code %in% uraba_codes

  df <- data.frame(
    dane_code   = dane_code[filas_validas],
    pct_tot_con = round(as.numeric(raw[[5]][filas_validas]), 4),
    pct_urb_con = round(as.numeric(raw[[6]][filas_validas]), 4),
    pct_rur_con = round(as.numeric(raw[[7]][filas_validas]), 4),
    stringsAsFactors = FALSE
  )

  # Calcular complementos (NA se propaga automáticamente)
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

  # Validación: NA por dato faltante en la fuente
  cols_con <- c("pct_tot_con", "pct_urb_con", "pct_rur_con")
  for (col in cols_con) {
    na_muns <- df$dane_code[is.na(df[[col]])]
    if (length(na_muns) > 0) {
      message(sprintf(
        "[%s] NA en %s para: %s  (dato no disponible en la fuente)",
        prefijo, col, paste(na_muns, collapse = ", ")
      ))
    }
  }

  # Validación: porcentajes fuera de rango (solo valores no-NA)
  for (col in cols_con) {
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
  message(sprintf("Procesando: %s  →  prefijo '%s'  (skip = %d)",
                  s$archivo, s$prefijo, s$skip))
  extraer_servicio(path, s$prefijo, s$skip)
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

# Resumen de NA
na_count <- sum(is.na(resultado))
if (na_count > 0) {
  message(sprintf("\nTotal de celdas NA en el resultado: %d", na_count))
  na_cols <- colSums(is.na(resultado))
  message("Columnas con NA:")
  print(na_cols[na_cols > 0])
}

# -----------------------------------------------------------------------------
# Exportar a Excel
# AJUSTE ENTRE AÑOS: cambiar nombre del archivo y de la hoja
# -----------------------------------------------------------------------------
archivo_salida <- file.path(ruta_salida, "coberturas_SP_2020.xlsx")

write.xlsx(resultado, file = archivo_salida, sheetName = "coberturas_2020", overwrite = TRUE)

message(sprintf("\nArchivo exportado: %s", archivo_salida))
