# =============================================================================
# Extracción de coberturas de servicios públicos
# Fuente: Anuario Estadístico de Antioquia 2022, capítulo 12
# Municipios: corredor de Urabá (11 municipios)
# =============================================================================

library(readxl)
library(dplyr)
library(openxlsx)

# -----------------------------------------------------------------------------
# AJUSTE ENTRE AÑOS: cambiar ruta_entrada y anio al adaptar para otros años
# -----------------------------------------------------------------------------
ruta_entrada <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/12-servicios-publicos-2022_2026-06-11/12-servicios-publicos-2022/"
ruta_salida  <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia/extraccion_2022/"
anio <- 2022

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
# AJUSTE ENTRE AÑOS: en 2022 los archivos se llaman SP12.6.X_2022.xlsx.
# AJUSTE ENTRE AÑOS: skip = 1 para todos (idéntico a 2021).
# DECISIÓN METODOLÓGICA: el nombre del municipio se asigna siempre desde
#   uraba_nombres, no desde la fuente. Turbo aparece como "Distrito Portuario..."
#   desde 2021; se normaliza a "Turbo" para consistencia en el panel.
# -----------------------------------------------------------------------------
servicios <- list(
  list(archivo = "SP12.6.1_2022.xlsx", prefijo = "agua_potable"),
  list(archivo = "SP12.6.2_2022.xlsx", prefijo = "acueducto"),
  list(archivo = "SP12.6.3_2022.xlsx", prefijo = "alcantarillado"),
  list(archivo = "SP12.6.4_2022.xlsx", prefijo = "energia"),
  list(archivo = "SP12.6.6_2022.xlsx", prefijo = "gas")
)

# -----------------------------------------------------------------------------
# Función de extracción
# DECISIÓN METODOLÓGICA: estructura idéntica a 2021. Columnas por posición:
#   col 1 = DANE, col 5 = % Total, col 6 = % Cabecera (urbano),
#   col 7 = % Centro poblado y rural disperso (rural).
# DECISIÓN METODOLÓGICA: las variables SIN servicio se calculan como
#   complemento (100 - pct_con). NA se propaga sin imputación.
# -----------------------------------------------------------------------------
extraer_servicio <- function(path, prefijo) {

  raw <- read_xlsx(path, sheet = 1, skip = 1, col_names = TRUE)

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

  # Validación: NA inesperados
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

  # Validación: porcentajes fuera de rango
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
  message(sprintf("Procesando: %s  →  prefijo '%s'", s$archivo, s$prefijo))
  extraer_servicio(path, s$prefijo)
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
  print(na_cols[na_cols > 0])
}

# -----------------------------------------------------------------------------
# Exportar a Excel
# AJUSTE ENTRE AÑOS: cambiar nombre del archivo y de la hoja
# -----------------------------------------------------------------------------
archivo_salida <- file.path(ruta_salida, "coberturas_SP_2022.xlsx")

write.xlsx(resultado, file = archivo_salida, sheetName = "coberturas_2022", overwrite = TRUE)

message(sprintf("\nArchivo exportado: %s", archivo_salida))
