library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# ── Rutas ─────────────────────────────────────────────────────────────────────
ruta_xlsx <- "C:/Users/jimen/Documents/encuesta_calidad_vida_antioquia/encuesta-calidad-vida-2023_2026-06-04/encuesta-calidad-vida-2023/Indicadores_ECV2023.xlsx"
ruta_out  <- "C:/Users/jimen/Documents/encuesta_calidad_vida_antioquia/extraccion_ecv_2023/ecv_2023_wide.xlsx"

# ── Municipios ────────────────────────────────────────────────────────────────
municipios_uraba <- c(
  "Apartadó", "Arboletes", "Carepa", "Chigorodó", "Murindó",
  "Mutatá", "Necoclí", "San Juan de Urabá", "San Pedro de Urabá",
  "Turbo", "Vigía del Fuerte"
)

# ── Indicadores: nombre original 2023 → nombre estandarizado ─────────────────
# Nota: "Tasa de desocupados" en 2023 se mapea a "tasa_desempleo" para
# mantener consistencia con el panel de otros años.
indicadores_map <- c(
  "Indicador de calidad de vida multidimensional"    = "IMCV",
  "D1_V1: Estrato"                                   = "estrato",
  "D1_V2: Materiales inadecuados"                    = "calidad_vivienda",
  "D2_V1: Número de servicios públicos instalados"   = "num_servicios_pub",
  "D2_V2: Número de servicios públicos suspendidos"  = "num_servicios_suspendidos",
  "Tasa de desocupados"                              = "tasa_desempleo",
  "Índice de dependencia económica"                  = "dep_economica"
)

# dep_economica: índice de dependencia económica. Mide la proporción de personas
# dependientes por cada 100 personas en el hogar. A mayor valor, mayor carga
# económica sobre los ocupados. Se aplica filtro de CV <= 15% para la
# desagregación urbano/rural. No es una excepción al filtro estándar.

nombres_originales     <- names(indicadores_map)
nombres_estandarizados <- unname(indicadores_map)

# ── 2. Lectura ────────────────────────────────────────────────────────────────
# Encabezados en fila 15 → skip = 14
raw <- read_excel(ruta_xlsx, skip = 14)

# ── DIAGNÓSTICO ───────────────────────────────────────────────────────────────
cat("\n--- Columnas del archivo ---\n")
print(names(raw))

# ── 3-7. Filtrado, CV y estandarización ──────────────────────────────────────
# CV en 2023 está en porcentaje (umbral > 15, no > 0.15 como en 2021)
df <- raw |>
  filter(Territorio %in% municipios_uraba) |>                               # 3
  filter(NomIndicador %in% nombres_originales) |>                            # 4
  mutate(cv_num = as.numeric(CV)) |>                                         # 5a
  mutate(NomIndicador = indicadores_map[NomIndicador]) |>                    # 7: nombres antes del CV
  # DECISIÓN METODOLÓGICA: Sin filtro de CV para tasa_desempleo y estrato.
  # tasa_desempleo: CV supera el 15% en la mayoría de municipios de Urabá en
  # 2021 y 2023 por tamaño muestral reducido.
  # estrato: el filtro eliminaba demasiadas observaciones, dejando cobertura
  # insuficiente para el análisis longitudinal.
  # Ambas variables se incluyen sin filtro y deben interpretarse con precaución.
  mutate(                                                                     # 5b + 6
    Valor = if_else(
      !NomIndicador %in% c("tasa_desempleo", "estrato") & !is.na(cv_num) & cv_num > 15,
      NA_real_,
      as.numeric(Valor)
    )
  ) |>
  mutate(dane_code = as.character(Codigo)) |>                                 # 10
  select(Territorio, dane_code, NomIndicador, Zona, Valor)

cat("\n--- Filas después del filtro (esperado: 11×7×3 = 231):", nrow(df), "---\n")

# ── 8. Primer pivot: Zona → columnas Total / Urbano / Rural ──────────────────
df_zona <- df |>
  pivot_wider(
    id_cols     = c(Territorio, dane_code, NomIndicador),
    names_from  = Zona,
    values_from = Valor
  )

# ── 9. Renombrar a minúsculas ─────────────────────────────────────────────────
df_zona <- df_zona |>
  rename(total = Total, urbano = Urbano, rural = Rural)

# ── Segundo pivot: indicador → prefijo de columna ────────────────────────────
df_wide <- df_zona |>
  pivot_wider(
    id_cols     = c(Territorio, dane_code),
    names_from  = NomIndicador,
    values_from = c(total, urbano, rural),
    names_glue  = "{NomIndicador}_{.value}"
  )

# ── 11-12. Año, renombre y orden de columnas ──────────────────────────────────
cols_indicadores <- as.vector(
  outer(nombres_estandarizados, c("_total", "_urbano", "_rural"), paste0)
)

df_wide <- df_wide |>
  rename(municipio = Territorio) |>
  mutate(anio = 2023L) |>                                                     # 11
  select(dane_code, municipio, anio, all_of(cols_indicadores))                # 12

# ── 13. Exportar ──────────────────────────────────────────────────────────────
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
