library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# ── Rutas ────────────────────────────────────────────────────────────────────
ruta_xlsx <- "C:/Users/jimen/Documents/encuesta-calidad-vida-2017_2026-06-04/encuesta-calidad-vida-2017/ConsolidadoECV2017.xlsx"
ruta_out  <- "C:/Users/jimen/Documents/extraccion_ecv_2017/ecv_2017_wide.xlsx"

# ── Municipios ────────────────────────────────────────────────────────────────
municipios_uraba <- c(
  "Apartadó", "Arboletes", "Carepa", "Chigorodó", "Murindó",
  "Mutatá", "Necoclí", "San Juan de Urabá", "San Pedro de Urabá",
  "Turbo", "Vigía del Fuerte"
)

# ── Códigos DANE ──────────────────────────────────────────────────────────────
dane_tbl <- tibble::tribble(
  ~dane_code, ~municipio,
  "05045", "Apartadó",
  "05051", "Arboletes",
  "05147", "Carepa",
  "05172", "Chigorodó",
  "05475", "Murindó",
  "05480", "Mutatá",
  "05490", "Necoclí",
  "05659", "San Juan de Urabá",
  "05665", "San Pedro de Urabá",
  "05837", "Turbo",
  "05873", "Vigía del Fuerte"
)

# ── Indicadores: nombre original → nombre estandarizado ──────────────────────
indicadores_map <- c(
  "Índice Multidimensional de Condiciones de Vida - IMCV (%)"                                                        = "IMCV",
  "Componentes - Índice Multidimensional de Condiciones de Vida (IMCV) - D1_V1 Estrato de la vivienda"               = "estrato",
  "Componentes - Índice Multidimensional de Condiciones de Vida (IMCV) - D1_V2 Calidad de la vivienda"               = "calidad_vivienda",
  "Componentes - Índice Multidimensional de Condiciones de Vida (IMCV) - D2_V3 N° de servicios públicos"             = "num_servicios_pub",
  "Componentes - Índice Multidimensional de Condiciones de Vida (IMCV) - D2_V4 N° de servicios públicos suspendidos" = "num_servicios_suspendidos",
  "Tasa de desempleo  (%)"                                                                                            = "tasa_desempleo"
)

nombres_originales     <- names(indicadores_map)
nombres_estandarizados <- unname(indicadores_map)

# ── 2. Lectura ────────────────────────────────────────────────────────────────
raw <- read_excel(ruta_xlsx, sheet = "Indicadores-ECV2017", skip = 9)

# ── 3-6. Limpieza y filtrado ──────────────────────────────────────────────────
df <- raw |>
  mutate(`Nombre del Indicador` = trimws(`Nombre del Indicador`)) |>   # 3
  filter(Territorio %in% municipios_uraba) |>                          # 4
  filter(`Nombre del Indicador` %in% nombres_originales) |>            # 5
  mutate(                                                               # 6
    Total  = as.numeric(Total),
    Urbano = as.numeric(Urbano),
    Rural  = as.numeric(Rural)
  ) |>
  mutate(`Nombre del Indicador` = indicadores_map[`Nombre del Indicador`]) |>  # 7
  select(Territorio, `Nombre del Indicador`, Total, Urbano, Rural)

# ── 8. Pivot a formato ancho ──────────────────────────────────────────────────
df_wide <- df |>
  pivot_wider(
    names_from  = `Nombre del Indicador`,
    values_from = c(Total, Urbano, Rural),
    names_glue  = "{`Nombre del Indicador`}_{.value}"
  ) |>
  rename_with(~ gsub("_Total$",  "_total",  .x)) |>
  rename_with(~ gsub("_Urbano$", "_urbano", .x)) |>
  rename_with(~ gsub("_Rural$",  "_rural",  .x))

# ── 9. Renombrar Territorio → municipio ──────────────────────────────────────
df_wide <- df_wide |>
  rename(municipio = Territorio)

# ── 10-12. DANE, año y orden de columnas ─────────────────────────────────────
cols_indicadores <- as.vector(outer(nombres_estandarizados, c("_total", "_urbano", "_rural"), paste0))

df_wide <- df_wide |>
  left_join(dane_tbl, by = "municipio") |>   # 10
  mutate(anio = 2017L) |>                    # 11
  select(dane_code, municipio, anio, all_of(cols_indicadores))  # 12

# ── 13. Exportar ──────────────────────────────────────────────────────────────
df_wide <- df_wide |>
  mutate(dane_code = formatC(as.integer(dane_code), width = 5, flag = "0"))

write_xlsx(df_wide, ruta_out)
message("Archivo exportado: ", ruta_out)

# ── Verificación ──────────────────────────────────────────────────────────────
cat("\ndim(df_wide):\n");    print(dim(df_wide))
cat("\nnames(df_wide):\n");  print(names(df_wide))
cat("\ndf_wide[, 1:5]:\n");  print(df_wide[, 1:5])
