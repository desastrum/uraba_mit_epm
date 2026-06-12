# =============================================================================
# Extracción de coberturas de servicios públicos
# Fuente: Anuario Estadístico de Antioquia 2023, capítulo 12
# Municipios: corredor de Urabá (11 municipios)
# =============================================================================

library(readxl)
library(dplyr)
library(openxlsx)

# -----------------------------------------------------------------------------
# AJUSTE ENTRE AÑOS: cambiar ruta_entrada y anio al adaptar para otros años
# -----------------------------------------------------------------------------
ruta_entrada <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/12-servicios-publicos-2023_2026-06-11/12-servicios-publicos-2023/"
ruta_salida  <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/extraccion_2023/"
anio <- 2023

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
# AJUSTE ENTRE AÑOS: en 2023 los archivos se llaman SP12.6.X_2023.xlsx.
# AJUSTE ENTRE AÑOS: en 2023 se agregan dos columnas nuevas (RPG y PAP) en
#   posiciones 4 y 5, desplazando las coberturas de cols 5/6/7 (2022) a
#   cols 7/8/9 (2023). El código DANE sigue en col 1, skip = 1 para todos.
# ADVERTENCIA: el archivo de gas (SP12.6.6_2023.xlsx) solo reporta cobertura
#   total (col 7). No hay desagregación Cabecera/Resto en la fuente 2023.
#   Las columnas gas_pct_urb_* y gas_pct_rur_* quedan como NA.
# DECISIÓN METODOLÓGICA: el nombre del municipio se asigna desde uraba_nombres.
#   Turbo aparece en la fuente como "Distrito Portuario..." desde 2021;
#   se normaliza a "Turbo" para consistencia con todos los años del panel.
# -----------------------------------------------------------------------------
servicios <- list(
  list(archivo = "SP12.6.1_2023.xlsx", prefijo = "agua_potable",   solo_total = FALSE),
  list(archivo = "SP12.6.2_2023.xlsx", prefijo = "acueducto",      solo_total = FALSE),
  list(archivo = "SP12.6.3_2023.xlsx", prefijo = "alcantarillado", solo_total = FALSE),
  list(archivo = "SP12.6.4_2023.xlsx", prefijo = "energia",        solo_total = FALSE),
  list(archivo = "SP12.6.6_2023.xlsx", prefijo = "gas",            solo_total = TRUE)
)

# -----------------------------------------------------------------------------
# Función de extracción
# DECISIÓN METODOLÓGICA: columnas por posición (no por nombre):
#   col 1 = DANE, col 7 = % Total, col 8 = % Cabecera (urbano),
#   col 9 = % Centro poblado y rural disperso (rural).
# DECISIÓN METODOLÓGICA: para gas (solo_total = TRUE), col 8 y 9 contienen
#   recuentos de viviendas (no coberturas), por lo que se ignoran y las
#   variables urb/rur se asignan como NA explícitamente.
# DECISIÓN METODOLÓGICA: las variables SIN servicio se calculan como
#   complemento (100 - pct_con). NA se propaga sin imputación.
# -----------------------------------------------------------------------------
extraer_servicio <- function(path, prefijo, solo_total) {

  raw <- read_xlsx(path, sheet = 1, skip = 1, col_names = TRUE)

  # DECISIÓN METODOLÓGICA: código DANE en columna 1; se normaliza a 5 dígitos.
  dane_raw  <- raw[[1]]
  dane_code <- suppressWarnings(sprintf("%05d", as.integer(dane_raw)))

  filas_validas <- !is.na(suppressWarnings(as.integer(dane_raw))) &
                   dane_code %in% uraba_codes

  if (!solo_total) {
    df <- data.frame(
      dane_code   = dane_code[filas_validas],
      pct_tot_con = round(as.numeric(raw[[7]][filas_validas]), 4),
      pct_urb_con = round(as.numeric(raw[[8]][filas_validas]), 4),
      pct_rur_con = round(as.numeric(raw[[9]][filas_validas]), 4),
      stringsAsFactors = FALSE
    )
    df <- df %>%
      mutate(
        pct_tot_sin = round(100 - pct_tot_con, 4),
        pct_urb_sin = round(100 - pct_urb_con, 4),
        pct_rur_sin = round(100 - pct_rur_con, 4)
      )
  } else {
    # Gas 2023: solo cobertura total disponible en la fuente
    df <- data.frame(
      dane_code   = dane_code[filas_validas],
      pct_tot_con = round(as.numeric(raw[[7]][filas_validas]), 4),
      pct_tot_sin = NA_real_,
      pct_urb_con = NA_real_,
      pct_urb_sin = NA_real_,
      pct_rur_con = NA_real_,
      pct_rur_sin = NA_real_,
      stringsAsFactors = FALSE
    )
    df$pct_tot_sin <- round(100 - df$pct_tot_con, 4)
  }

  # Validación: municipios faltantes
  faltantes <- setdiff(uraba_codes, df$dane_code)
  if (length(faltantes) > 0) {
    warning(sprintf(
      "[%s] Municipios no encontrados en %s: %s",
      prefijo, basename(path), paste(faltantes, collapse = ", ")
    ))
  }

  # Validación: NA inesperados (solo para columnas que deberían tener datos)
  cols_esperadas <- if (solo_total) "pct_tot_con" else c("pct_tot_con", "pct_urb_con", "pct_rur_con")
  for (col in cols_esperadas) {
    na_muns <- df$dane_code[is.na(df[[col]])]
    if (length(na_muns) > 0) {
      message(sprintf(
        "[%s] NA en %s para: %s  (dato no disponible en la fuente)",
        prefijo, col, paste(na_muns, collapse = ", ")
      ))
    }
  }

  # Validación: porcentajes fuera de rango
  for (col in cols_esperadas) {
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
  message(sprintf("Procesando: %s  →  prefijo '%s'  (solo_total = %s)",
                  s$archivo, s$prefijo, s$solo_total))
  extraer_servicio(path, s$prefijo, s$solo_total)
})

base_df <- data.frame(dane_code = uraba_codes, stringsAsFactors = FALSE)

resultado <- Reduce(
  function(x, y) left_join(x, y, by = "dane_code"),
  c(list(base_df), lista_dfs)
)

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

na_count <- sum(is.na(resultado))
if (na_count > 0) {
  message(sprintf("\nTotal de celdas NA: %d", na_count))
  na_cols <- colSums(is.na(resultado))
  message("Columnas con NA:")
  print(na_cols[na_cols > 0])
}

# -----------------------------------------------------------------------------
# Exportar a Excel
# AJUSTE ENTRE AÑOS: cambiar nombre del archivo y de la hoja
# -----------------------------------------------------------------------------
archivo_salida <- file.path(ruta_salida, "coberturas_SP_2023.xlsx")

write.xlsx(resultado, file = archivo_salida, sheetName = "coberturas_2023", overwrite = TRUE)

message(sprintf("\nArchivo exportado: %s", archivo_salida))
