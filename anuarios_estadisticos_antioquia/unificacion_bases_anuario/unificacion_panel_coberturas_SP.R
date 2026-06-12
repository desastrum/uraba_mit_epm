# =============================================================================
# Panel longitudinal de coberturas de servicios públicos — Urabá 2016–2023
# Anuario Estadístico de Antioquia
# =============================================================================
# Salida: panel_coberturas_SP_2016_2023.xlsx (88 filas × 33 columnas)
# Clave del panel: dane_code + anio
# =============================================================================

library(readxl)
library(dplyr)
library(purrr)
library(openxlsx)


# -----------------------------------------------------------------------------
# 1. Constantes
# -----------------------------------------------------------------------------

ruta_base <- "C:/Users/jimen/Documents/anuario_estadistico_antioquia"

archivos <- list(
  "2016" = file.path(ruta_base, "extraccion_2016", "coberturas_SP_2016.xlsx"),
  "2017" = file.path(ruta_base, "extraccion_2017", "coberturas_SP_2017.xlsx"),
  "2018" = file.path(ruta_base, "extraccion_2018", "coberturas_SP_2018.xlsx"),
  "2019" = file.path(ruta_base, "extraccion_2019", "coberturas_SP_2019.xlsx"),
  "2020" = file.path(ruta_base, "extraccion_2020", "coberturas_SP_2020.xlsx"),
  "2021" = file.path(ruta_base, "extraccion_2021", "coberturas_SP_2021.xlsx"),
  "2022" = file.path(ruta_base, "extraccion_2022", "coberturas_SP_2022.xlsx"),
  "2023" = file.path(ruta_base, "extraccion_2023", "coberturas_SP_2023.xlsx")
)

ruta_salida <- file.path(ruta_base, "unificacion_bases_anuario")

columnas_esperadas <- c(
  "dane_code", "municipio", "anio",
  "agua_potable_pct_urb_con", "agua_potable_pct_urb_sin",
  "agua_potable_pct_rur_con", "agua_potable_pct_rur_sin",
  "agua_potable_pct_tot_con", "agua_potable_pct_tot_sin",
  "acueducto_pct_urb_con", "acueducto_pct_urb_sin",
  "acueducto_pct_rur_con", "acueducto_pct_rur_sin",
  "acueducto_pct_tot_con", "acueducto_pct_tot_sin",
  "alcantarillado_pct_urb_con", "alcantarillado_pct_urb_sin",
  "alcantarillado_pct_rur_con", "alcantarillado_pct_rur_sin",
  "alcantarillado_pct_tot_con", "alcantarillado_pct_tot_sin",
  "energia_pct_urb_con", "energia_pct_urb_sin",
  "energia_pct_rur_con", "energia_pct_rur_sin",
  "energia_pct_tot_con", "energia_pct_tot_sin",
  "gas_pct_urb_con", "gas_pct_urb_sin",
  "gas_pct_rur_con", "gas_pct_rur_sin",
  "gas_pct_tot_con", "gas_pct_tot_sin"
)

municipios_uraba <- c(
  "05045", "05051", "05147", "05172", "05475",
  "05480", "05490", "05659", "05665", "05837", "05873"
)


# -----------------------------------------------------------------------------
# 2. Lectura y apilado
# -----------------------------------------------------------------------------

# DECISIÓN METODOLÓGICA: la unificación se hace con bind_rows() sobre los
# 8 data frames leídos secuencialmente. No se realiza ningún merge ni
# transformación de variables — cada archivo ya tiene estructura idéntica.

panel <- archivos %>%
  imap(function(path, anio) {
    df <- read_xlsx(path, sheet = 1)

    # DECISIÓN METODOLÓGICA: dane_code se mantiene como string con cero a la
    # izquierda (5 dígitos) en todo el panel. Al leer desde Excel puede
    # importarse como numérico; se reconvierte con sprintf("%05d", as.integer()).
    df$dane_code <- sprintf("%05d", as.integer(df$dane_code))

    df
  }) %>%
  bind_rows()


# -----------------------------------------------------------------------------
# 3. Ordenamiento
# -----------------------------------------------------------------------------

panel <- panel %>%
  arrange(dane_code, anio)


# -----------------------------------------------------------------------------
# 4. Validaciones post-unificación
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 60), "\n")
cat("VALIDACIONES DEL PANEL UNIFICADO\n")
cat(strrep("=", 60), "\n\n")

# 4.1 Dimensiones
n_filas   <- nrow(panel)
n_columnas <- ncol(panel)

cat(sprintf("Dimensiones: %d filas × %d columnas\n", n_filas, n_columnas))

if (n_filas != 88) {
  warning(sprintf("Se esperaban 88 filas, se obtuvieron %d.", n_filas))
} else {
  cat("  OK: 88 filas (11 municipios × 8 años)\n")
}

if (n_columnas != 33) {
  warning(sprintf("Se esperaban 33 columnas, se obtuvieron %d.", n_columnas))
} else {
  cat("  OK: 33 columnas\n")
}

# 4.2 Unicidad de la clave dane_code + anio
n_duplicados <- panel %>%
  count(dane_code, anio) %>%
  filter(n > 1) %>%
  nrow()

cat(sprintf("\nUnicidad de clave (dane_code + anio): %d duplicado(s)\n", n_duplicados))
if (n_duplicados == 0) {
  cat("  OK: clave única en todo el panel\n")
} else {
  warning("Existen combinaciones duplicadas de dane_code + anio.")
  panel %>%
    count(dane_code, anio) %>%
    filter(n > 1) %>%
    print()
}

# 4.3 Presencia de los 11 municipios en los 8 años
municipios_panel  <- sort(unique(panel$dane_code))
municipios_faltantes <- setdiff(municipios_uraba, municipios_panel)
municipios_extra     <- setdiff(municipios_panel, municipios_uraba)

cat(sprintf("\nMunicipios presentes: %d de 11 esperados\n", length(municipios_panel)))
if (length(municipios_faltantes) == 0) {
  cat("  OK: los 11 municipios de Urabá están en el panel\n")
} else {
  warning("Municipios faltantes: ", paste(municipios_faltantes, collapse = ", "))
}
if (length(municipios_extra) > 0) {
  warning("Municipios inesperados: ", paste(municipios_extra, collapse = ", "))
}

annios_panel    <- sort(unique(panel$anio))
annios_esperados <- 2016:2023
annios_faltantes <- setdiff(annios_esperados, annios_panel)

cat(sprintf("Años presentes: %s\n", paste(annios_panel, collapse = ", ")))
if (length(annios_faltantes) == 0) {
  cat("  OK: los 8 años (2016–2023) están en el panel\n")
} else {
  warning("Años faltantes: ", paste(annios_faltantes, collapse = ", "))
}

# 4.4 Conteo de NA por variable
cat("\nConteo de NA por variable:\n")
na_por_variable <- colSums(is.na(panel))
na_con_valores  <- na_por_variable[na_por_variable > 0]

if (length(na_con_valores) == 0) {
  cat("  (sin valores faltantes)\n")
} else {
  for (var in names(na_con_valores)) {
    cat(sprintf("  %-40s %2d NA(s)\n", var, na_con_valores[[var]]))
  }
}

# ADVERTENCIA: NA documentados y verificados contra los archivos fuente:
#   - energia_pct_* para Murindó (05475) y Vigía del Fuerte (05873) en 2020:
#     dato no disponible en la fuente original.
#   - gas_pct_urb_* y gas_pct_rur_* para todos los municipios en 2023:
#     desagregación no publicada por el DAP ese año.

# 4.5 Columnas del panel
columnas_faltantes <- setdiff(columnas_esperadas, names(panel))
columnas_extra     <- setdiff(names(panel), columnas_esperadas)

cat("\nColumnas del panel:\n")
if (length(columnas_faltantes) == 0 && length(columnas_extra) == 0) {
  cat("  OK: las 33 columnas esperadas están presentes y no hay columnas extra\n")
} else {
  if (length(columnas_faltantes) > 0)
    warning("Columnas faltantes: ", paste(columnas_faltantes, collapse = ", "))
  if (length(columnas_extra) > 0)
    warning("Columnas inesperadas: ", paste(columnas_extra, collapse = ", "))
}

# 4.6 Vista general del panel
cat("\nGlimpse del panel final:\n")
glimpse(panel)

cat("\n", strrep("=", 60), "\n\n")


# -----------------------------------------------------------------------------
# 5. Exportación
# -----------------------------------------------------------------------------

dir.create(ruta_salida, recursive = TRUE, showWarnings = FALSE)

write.xlsx(
  panel,
  file      = file.path(ruta_salida, "panel_coberturas_SP_2016_2023.xlsx"),
  sheetName = "panel_2016_2023",
  overwrite = TRUE
)

cat(sprintf(
  "Panel exportado: %s\n",
  file.path(ruta_salida, "panel_coberturas_SP_2016_2023.xlsx")
))
