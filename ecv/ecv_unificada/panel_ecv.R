library(readxl)
library(dplyr)
library(writexl)

# ADVERTENCIA: La variable tasa_desempleo no tiene filtro de coeficiente de
# variación aplicado en 2021 y 2023, dado que su CV supera el umbral del 15%
# en la mayoría de municipios de Urabá por razones de tamaño muestral.
# Los valores de tasa_desempleo en esos años deben interpretarse con precaución.
#
# Los componentes del IMCV (estrato, calidad_vivienda, num_servicios_pub,
# num_servicios_suspendidos) tienen NA en zonas urbana y rural para 2017,
# ya que la ECV de ese año solo reportó desagregación a nivel total para
# esos indicadores.

# ── Rutas ─────────────────────────────────────────────────────────────────────
archivos <- c(
  "C:/Users/jimen/Documents/encuesta_calidad_vida_antioquia/extraccion_ecv_2017/ecv_2017_wide.xlsx",
  "C:/Users/jimen/Documents/encuesta_calidad_vida_antioquia/extraccion_ecv_2019/ecv_2019_wide.xlsx",
  "C:/Users/jimen/Documents/encuesta_calidad_vida_antioquia/extraccion_ecv_2021/ecv_2021_wide.xlsx",
  "C:/Users/jimen/Documents/encuesta_calidad_vida_antioquia/extraccion_ecv_2023/ecv_2023_wide.xlsx"
)

ruta_out <- "C:/Users/jimen/Documents/encuesta_calidad_vida_antioquia/ecv_unificada/panel_ecv_uraba.xlsx"

# ── 2-3. Leer y unir ──────────────────────────────────────────────────────────
panel <- lapply(archivos, read_excel) |>
  bind_rows()

# ── 4. Corregir dane_code con cero inicial ────────────────────────────────────
panel <- panel |>
  mutate(dane_code = formatC(as.integer(dane_code), width = 5, flag = "0"))

# ── 5. Ordenar por municipio y año ───────────────────────────────────────────
panel <- panel |>
  arrange(dane_code, anio)

# ── 6. Exportar ───────────────────────────────────────────────────────────────
write_xlsx(panel, ruta_out)
message("Archivo exportado: ", ruta_out)

# ── Verificación ──────────────────────────────────────────────────────────────
cat("\ndim(panel):\n")
print(dim(panel))    # debe ser 44 24

cat("\npanel |> count(anio):\n")
print(dplyr::count(panel, anio))

cat("\npanel |> count(municipio):\n")
print(dplyr::count(panel, municipio))

cat("\npanel$dane_code[1]:\n")
print(panel$dane_code[1])

cat("\nhead(panel[, 1:5]):\n")
print(head(panel[, 1:5]))
