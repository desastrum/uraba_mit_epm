library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# ── Rutas ─────────────────────────────────────────────────────────────────────
ruta_xlsx <- "C:/Users/jimen/Documents/encuesta-calidad-vida-2019_2026-06-04/encuesta-calidad-vida-2019/IndicadoresECV2019.xlsx"
ruta_out  <- "C:/Users/jimen/Documents/extraccion_ecv_2019/ecv_2019_wide.xlsx"

# ── Municipios ────────────────────────────────────────────────────────────────
municipios_uraba <- c(
  "Apartadó", "Arboletes", "Carepa", "Chigorodó", "Murindó",
  "Mutatá", "Necoclí", "San Juan de Urabá", "San Pedro de Urabá",
  "Turbo", "Vigía del Fuerte"
)

# ── Indicadores: nombre original → nombre estandarizado ──────────────────────
indicadores_map <- c(
  "Indicador de calidad de vida multidimensional"    = "IMCV",
  "D1_V1: Estrato"                                   = "estrato",
  "D1_V2: Materiales inadecuados"                    = "calidad_vivienda",
  "D2_V1: Número de servicios públicos instalados"   = "num_servicios_pub",
  "D2_V2: Número de servicios públicos suspendidos"  = "num_servicios_suspendidos",
  "Tasa de desempleo"                                = "tasa_desempleo"
)

nombres_originales     <- names(indicadores_map)
nombres_estandarizados <- unname(indicadores_map)

# ── 2. Lectura ────────────────────────────────────────────────────────────────
raw <- read_excel(ruta_xlsx, skip = 13)

# ── DIAGNÓSTICO: nombres reales de columna e indicadores en el archivo ────────
cat("\n--- Columnas del archivo ---\n")
print(names(raw))

cat("\n--- Indicadores únicos encontrados (Urabá) ---\n")
print(
  unique(raw$`Nombre del Indicador`[raw$Territorio %in% municipios_uraba])
)

# ── Detectar columna CV automáticamente (robusto a encoding) ──────────────────
col_cv <- names(raw)[grepl("CV|coeficiente|variaci", names(raw), ignore.case = TRUE)][1]
cat("\n--- Columna CV detectada: '", col_cv, "' ---\n", sep = "")

# ── 3-7. Filtrado, CV y estandarización ──────────────────────────────────────
df <- raw |>
  filter(Territorio %in% municipios_uraba) |>                               # 3
  filter(`Nombre del Indicador` %in% nombres_originales) |>                  # 4
  mutate(cv_num = as.numeric(.data[[col_cv]])) |>                            # 5a: CV a numeric
  mutate(                                                                     # 7: nombres antes del CV
    `Nombre del Indicador` = indicadores_map[`Nombre del Indicador`]
  ) |>
  # DECISIÓN METODOLÓGICA: Sin filtro de CV para tasa_desempleo y estrato.
  # tasa_desempleo: CV supera el 15% en la mayoría de municipios de Urabá en
  # 2021 y 2023 por tamaño muestral reducido.
  # estrato: el filtro eliminaba demasiadas observaciones, dejando cobertura
  # insuficiente para el análisis longitudinal.
  # Ambas variables se incluyen sin filtro y deben interpretarse con precaución.
  mutate(                                                                     # 5b + 6
    Valor = if_else(
      !`Nombre del Indicador` %in% c("tasa_desempleo", "estrato") & !is.na(cv_num) & cv_num > 15,
      NA_real_,
      as.numeric(Valor)
    )
  ) |>
  mutate(dane_code = as.character(Codigo)) |>                                 # 11
  select(Territorio, dane_code, `Nombre del Indicador`, Zona, Valor)

cat("\n--- Filas después del filtro (esperado: 11×6×3 = 198): ", nrow(df), " ---\n")

# ── 8. Primer pivot: Zona → columnas Total / Urbano / Rural ──────────────────
df_zona <- df |>
  pivot_wider(
    id_cols     = c(Territorio, dane_code, `Nombre del Indicador`),
    names_from  = Zona,
    values_from = Valor
  )

# ── 9. Renombrar a minúsculas ─────────────────────────────────────────────────
df_zona <- df_zona |>
  rename(total = Total, urbano = Urbano, rural = Rural)

# ── 10. Segundo pivot: indicador → prefijo de columna ────────────────────────
df_wide <- df_zona |>
  pivot_wider(
    id_cols     = c(Territorio, dane_code),
    names_from  = `Nombre del Indicador`,
    values_from = c(total, urbano, rural),
    names_glue  = "{`Nombre del Indicador`}_{.value}"
  )

# ── 12-13. Año, renombre y orden de columnas ──────────────────────────────────
cols_indicadores <- as.vector(
  outer(nombres_estandarizados, c("_total", "_urbano", "_rural"), paste0)
)

df_wide <- df_wide |>
  rename(municipio = Territorio) |>
  mutate(anio = 2019L) |>                                                     # 12
  select(dane_code, municipio, anio, all_of(cols_indicadores))                # 13

# ── 14. Exportar ──────────────────────────────────────────────────────────────
df_wide <- df_wide |>
  mutate(dane_code = formatC(as.integer(dane_code), width = 5, flag = "0"))

write_xlsx(df_wide, ruta_out)
message("Archivo exportado: ", ruta_out)

# ── Verificación ──────────────────────────────────────────────────────────────
cat("\ndim(df_wide):\n")
print(dim(df_wide))
cat("\nnames(df_wide):\n")
print(names(df_wide))
cat("\ndf_wide[, 1:5]:\n")
print(df_wide[, 1:5])
