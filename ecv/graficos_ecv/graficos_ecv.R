# =============================================================================
# Gráficas ECV Urabá 2017-2023
# Panel longitudinal: 11 municipios × 4 años (2017, 2019, 2021, 2023)
# Fuente: Encuesta de Calidad de Vida, DAP Antioquia
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(scales)
library(patchwork)
library(officer)
library(flextable)

# Rutas
ruta_datos   <- "C:/Users/jimen/Documents/ecv_unificada/panel_ecv_uraba.xlsx"
ruta_graf    <- "C:/Users/jimen/Documents/graficos_ecv/graficas/"
ruta_doc     <- "C:/Users/jimen/Documents/metodologia_graficos_ecv/metodologia_graficos_ecv.docx"

dir.create(ruta_graf, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(ruta_doc), recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# CARGA Y PREPARACIÓN DE DATOS
# =============================================================================

panel <- read_excel(ruta_datos) |>
  mutate(anio = as.integer(anio))

# ADVERTENCIA METODOLÓGICA: estrato tiene cobertura insuficiente en 2021 y 2023
# (CV > 15% en la mayoría de municipios). Se excluye de las gráficas principales
# y se documenta como variable de contexto.

# DECISIÓN METODOLÓGICA: El filtro de CV <= 15% se aplica a todos los
# indicadores excepto tasa_desempleo, cuyo CV supera el umbral en la mayoría
# de municipios de Urabá en 2021 y 2023 por razones de tamaño muestral.
# Los valores de tasa_desempleo en esos años se incluyen sin filtro y deben
# interpretarse con precaución.

# ADVERTENCIA: Los cuantificadores ACP (estrato, calidad_vivienda,
# num_servicios_pub, num_servicios_suspendidos) no son directamente comparables
# entre años porque los ponderadores se recalibran en cada edición de la ECV.
# La comparación temporal es indicativa de tendencias, no de magnitudes exactas.

# ADVERTENCIA: dane_code quedó como entero en los archivos fuente (ej. 5045
# en lugar de 05045). Se corrige en el panel con formatC() al momento de unión.

# =============================================================================
# ESTADÍSTICAS DESCRIPTIVAS (no exportadas — referencia interna)
# =============================================================================

estadisticas <- panel |>
  group_by(anio) |>
  summarise(across(ends_with("_total"),
    list(media   = ~mean(.x, na.rm = TRUE),
         mediana = ~median(.x, na.rm = TRUE),
         sd      = ~sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"))

# =============================================================================
# PARÁMETROS VISUALES COMPARTIDOS
# =============================================================================


paleta_11 <- c(
  "Apartadó"           = "#4E79A7",
  "Arboletes"          = "#F28E2B",
  "Carepa"             = "#E15759",
  "Chigorodó"          = "#76B7B2",
  "Murindó"            = "#59A14F",
  "Mutatá"             = "#EDC948",
  "Necoclí"            = "#B07AA1",
  "San Juan de Urabá"  = "#FF9DA7",
  "San Pedro de Urabá" = "#9C755F",
  "Turbo"              = "#BAB0AC",
  "Vigía del Fuerte"   = "#D37295"
)

tema_base <- theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 8)),
    plot.caption  = element_text(size = 7.5, color = "grey50", hjust = 0),
    legend.position = "right",
    legend.key.size = unit(0.4, "cm"),
    legend.text   = element_text(size = 8),
    panel.grid.minor = element_blank(),
    axis.title    = element_text(size = 9)
  )

anios_breaks <- c(2017L, 2019L, 2021L, 2023L)

guardar <- function(nombre, ancho = 12, alto = 7) {
  ggsave(
    filename = file.path(ruta_graf, nombre),
    width = ancho, height = alto, dpi = 300, units = "in",
    bg = "white"
  )
  message("Guardado: ", nombre)
}

# =============================================================================
# G1 — Evolución temporal IMCV total por municipio
# =============================================================================

prom_anio <- panel |>
  group_by(anio) |>
  summarise(prom_corredor = mean(IMCV_total, na.rm = TRUE))

resaltar_g1 <- c("Murindó", "Arboletes")

g1_data <- panel |>
  left_join(prom_anio, by = "anio") |>
  mutate(resaltado = municipio %in% resaltar_g1)

# Etiquetas al final de la línea (año 2023)
etiquetas_g1 <- g1_data |>
  filter(anio == 2023, resaltado) |>
  select(anio, municipio, IMCV_total)

g1 <- ggplot(g1_data, aes(x = anio, y = IMCV_total, group = municipio)) +
  geom_line(data = filter(g1_data, !resaltado),
            aes(color = municipio), linewidth = 0.7, alpha = 0.6) +
  geom_line(data = filter(g1_data, resaltado),
            aes(color = municipio), linewidth = 1.4) +
  geom_point(data = filter(g1_data, resaltado),
             aes(color = municipio), size = 2.5) +
  geom_line(data = prom_anio, aes(x = anio, y = prom_corredor),
            linetype = "dashed", color = "grey30", linewidth = 0.8, inherit.aes = FALSE) +
  geom_label(data = etiquetas_g1,
             aes(label = municipio, color = municipio),
             hjust = -0.05, size = 3, linewidth = 0, fill = "white", show.legend = FALSE) +
  scale_color_manual(values = paleta_11) +
  scale_x_continuous(breaks = anios_breaks, expand = expansion(mult = c(0.02, 0.18))) +
  labs(
    title    = "Evolución del Índice Multidimensional de Condiciones de Vida (IMCV) — Corredor Urabá, 2017-2023",
    subtitle = "Líneas resaltadas: Murindó (mayor mejora) y Arboletes (mayor deterioro). Línea punteada: promedio del corredor.",
    x = "Año", y = "IMCV total", color = "Municipio",

  ) +
  tema_base

guardar("g1_imcv_evolucion_temporal.png")

# =============================================================================
# G2 — Distribución del IMCV por año (boxplot + jitter)
# =============================================================================

g2_data <- panel |>
  mutate(anio_f = factor(anio))

g2 <- ggplot(g2_data, aes(x = anio_f, y = IMCV_total)) +
  geom_boxplot(fill = "grey92", color = "grey40", outlier.shape = NA, width = 0.45) +
  geom_jitter(aes(color = municipio), width = 0.12, size = 2.5, alpha = 0.85) +
  ggrepel::geom_text_repel(
    data = g2_data |> filter(IMCV_total == max(IMCV_total) |
                              IMCV_total == min(IMCV_total) |
                              municipio %in% c("Murindó", "Arboletes")),
    aes(label = paste0(municipio, "\n", round(IMCV_total, 1)), color = municipio),
    size = 2.8, show.legend = FALSE, max.overlaps = 20,
    box.padding = 0.4, segment.size = 0.3
  ) +
  scale_color_manual(values = paleta_11) +
  labs(
    title    = "Distribución del IMCV entre municipios de Urabá por año",
    subtitle = "Cada punto representa un municipio. Las cajas muestran mediana e IQR.",
    x = "Año", y = "IMCV total", color = "Municipio",

  ) +
  tema_base

guardar("g2_imcv_distribucion_anual.png")

# =============================================================================
# G3 — Comparación IMCV entre municipios por año (barras horizontales facetadas)
# =============================================================================

prom_anio_imcv <- panel |>
  group_by(anio) |>
  summarise(prom = mean(IMCV_total, na.rm = TRUE))

g3_data <- panel |>
  left_join(prom_anio_imcv, by = "anio") |>
  mutate(
    sobre_prom = IMCV_total >= prom,
    municipio  = reorder(municipio, IMCV_total)
  )

g3 <- ggplot(g3_data, aes(x = IMCV_total, y = reorder(municipio, IMCV_total),
                           fill = sobre_prom)) +
  geom_col(width = 0.7) +
  geom_vline(data = prom_anio_imcv, aes(xintercept = prom),
             linetype = "dashed", color = "grey30", linewidth = 0.7) +
  scale_fill_manual(values = c("TRUE" = "#D4896A", "FALSE" = "#6A9EC2"),
                    labels = c("TRUE" = "Sobre el promedio", "FALSE" = "Bajo el promedio")) +
  facet_wrap(~anio, ncol = 4) +
  labs(
    title   = "Comparación del IMCV entre municipios por año de encuesta",
    subtitle = "Línea punteada: promedio del corredor. Color: posición relativa al promedio.",
    x = "IMCV total", y = NULL, fill = NULL,
  ) +
  tema_base +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

guardar("g3_imcv_comparacion_municipios.png", ancho = 14, alto = 7)

# =============================================================================
# G4 — Brecha urbano-rural IMCV
# =============================================================================

resaltar_g4 <- c("Mutatá", "Apartadó")

g4_data <- panel |>
  mutate(brecha_imcv = IMCV_urbano - IMCV_rural,
         resaltado   = municipio %in% resaltar_g4)

etiquetas_g4 <- g4_data |>
  filter(anio == 2023, resaltado) |>
  select(anio, municipio, brecha_imcv)

g4 <- ggplot(g4_data, aes(x = anio, y = brecha_imcv, group = municipio)) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.7) +
  geom_line(data = filter(g4_data, !resaltado),
            aes(color = municipio), linewidth = 0.7, alpha = 0.6) +
  geom_line(data = filter(g4_data, resaltado),
            aes(color = municipio), linewidth = 1.4) +
  geom_point(data = filter(g4_data, resaltado),
             aes(color = municipio), size = 2.5) +
  geom_label(data = etiquetas_g4,
             aes(label = municipio, color = municipio),
             hjust = -0.05, size = 3, linewidth = 0, fill = "white", show.legend = FALSE) +
  scale_color_manual(values = paleta_11) +
  scale_x_continuous(breaks = anios_breaks, expand = expansion(mult = c(0.02, 0.18))) +
  labs(
    title    = "Brecha urbano-rural del IMCV — Corredor Urabá, 2017-2023",
    subtitle = "Resaltados: Mutatá (único con brecha negativa en 2023) y Apartadó (brecha más alta sostenida).",
    x = "Año", y = "IMCV urbano − IMCV rural", color = "Municipio",

  ) +
  tema_base

guardar("g4_imcv_brecha_urbano_rural.png")

# =============================================================================
# G5 — Evolución num_servicios_pub total
# =============================================================================

# G5 — Small multiples: evolución num_servicios_pub_total por municipio
g5_data <- panel |>
  select(municipio, anio, num_servicios_pub_total)

# Promedio del corredor por año (línea de referencia en cada panel)
prom_serv <- panel |>
  group_by(anio) |>
  summarise(prom_corredor = mean(num_servicios_pub_total, na.rm = TRUE))

g5 <- ggplot(g5_data, aes(x = anio, y = num_servicios_pub_total)) +
  geom_line(data = prom_serv |> crossing(municipio = unique(g5_data$municipio)),
            aes(x = anio, y = prom_corredor), linetype = "dashed", color = "grey60",
            linewidth = 0.6, inherit.aes = FALSE) +
  geom_line(aes(color = municipio), linewidth = 1, na.rm = TRUE) +
  geom_point(aes(color = municipio), size = 2, na.rm = TRUE) +
  scale_color_manual(values = paleta_11, guide = "none") +
  scale_x_continuous(breaks = anios_breaks) +
  facet_wrap(~ municipio, ncol = 4) +
  labs(
    title    = "Evolución del número de servicios públicos instalados (cuantificador ACP) — Corredor Urabá, 2017-2023",
    subtitle = "Un panel por municipio. Línea punteada: promedio del corredor.",
    x = "Año", y = "Cuantificador ACP"
  ) +
  tema_base +
  theme(
    strip.text       = element_text(face = "bold", size = 9),
    panel.border     = element_rect(color = "grey85", fill = NA, linewidth = 0.4),
    panel.spacing    = unit(0.8, "lines")
  )

guardar("g5_servicios_pub_small_multiples.png", ancho = 14, alto = 10)

# =============================================================================
# G6 — Brecha urbano-rural num_servicios_pub (2019-2023, barras agrupadas)
# =============================================================================

# G6 — Brecha urbano-rural servicios públicos (ordenado por promedio 2019-2023)
g6_data <- panel |>
  filter(anio %in% c(2019L, 2021L, 2023L)) |>
  mutate(brecha_serv = num_servicios_pub_urbano - num_servicios_pub_rural) |>
  filter(!is.na(brecha_serv))

# Orden por promedio de los tres años
orden_g6 <- g6_data |>
  group_by(municipio) |>
  summarise(promedio = mean(brecha_serv, na.rm = TRUE)) |>
  arrange(promedio) |>
  pull(municipio)

g6_data <- g6_data |>
  mutate(
    municipio = factor(municipio, levels = orden_g6),
    anio_f    = factor(anio)
  )

g6 <- ggplot(g6_data, aes(x = municipio, y = brecha_serv, fill = anio_f)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 0, color = "grey30", linewidth = 0.6) +
  scale_fill_brewer(palette = "Set2") +
  coord_flip() +
  labs(
    title    = "Brecha urbano-rural en servicios públicos instalados — Corredor Urabá, 2019-2023",
    subtitle = "Valores positivos: mayor instalación urbana. Negativo: mayor instalación rural. Sin datos 2017.",
    x = NULL, y = "Cuantificador ACP (urbano − rural)", fill = "Año"
  ) +
  tema_base +
  theme(legend.position = "bottom")

guardar("g6_servicios_pub_brecha_urbano_rural.png")

# =============================================================================
# G7 — Evolución tasa de desempleo total con banda COVID
# =============================================================================

resaltar_g7 <- c("Apartadó", "Necoclí", "Carepa", "Turbo")

prom_desemp <- panel |>
  group_by(anio) |>
  summarise(prom_corredor = mean(tasa_desempleo_total, na.rm = TRUE))

g7_data <- panel |>
  left_join(prom_desemp, by = "anio") |>
  mutate(resaltado = municipio %in% resaltar_g7)

etiquetas_g7 <- g7_data |>
  filter(anio == 2023, resaltado) |>
  select(anio, municipio, tasa_desempleo_total)

g7 <- ggplot(g7_data, aes(x = anio, y = tasa_desempleo_total, group = municipio)) +
  # Banda COVID
  annotate("rect", xmin = 2019, xmax = 2021, ymin = -Inf, ymax = Inf,
           fill = "#FFF3CD", alpha = 0.6) +
  annotate("text", x = 2020, y = Inf, label = "Período\nCOVID",
           vjust = 1.5, size = 3, color = "grey50") +
  geom_line(data = filter(g7_data, !resaltado),
            aes(color = municipio), linewidth = 0.7, alpha = 0.6) +
  geom_line(data = filter(g7_data, resaltado),
            aes(color = municipio), linewidth = 1.4) +
  geom_point(data = filter(g7_data, resaltado),
             aes(color = municipio), size = 2.5) +
  geom_line(data = prom_desemp, aes(x = anio, y = prom_corredor),
            linetype = "dashed", color = "grey30", linewidth = 0.8, inherit.aes = FALSE) +
  geom_label(data = etiquetas_g7,
             aes(label = municipio, color = municipio),
             hjust = -0.05, size = 3, linewidth = 0, fill = "white", show.legend = FALSE) +
  scale_color_manual(values = paleta_11) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_x_continuous(breaks = anios_breaks, expand = expansion(mult = c(0.02, 0.18))) +
  labs(
    title    = "Evolución de la tasa de desempleo — Corredor Urabá, 2017-2023",
    subtitle = "Resaltados: Apartadó (mayor mejora), Necoclí y Carepa (mayor deterioro), Turbo (alta volatilidad).",
    x = "Año", y = "Tasa de desempleo (%)", color = "Municipio"
  ) +
  tema_base

guardar("g7_desempleo_evolucion_temporal.png")

# =============================================================================
# G8 — Brecha urbano-rural tasa de desempleo
# =============================================================================

# G8 — Small multiples: brecha urbano-rural tasa de desempleo por municipio
g8_data <- panel |>
  mutate(brecha_desemp = tasa_desempleo_urbano - tasa_desempleo_rural)

g8 <- ggplot(g8_data, aes(x = anio, y = brecha_desemp)) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.5, linetype = "dashed") +
  geom_line(aes(color = municipio), linewidth = 1, na.rm = TRUE) +
  geom_point(aes(color = municipio), size = 2, na.rm = TRUE) +
  scale_color_manual(values = paleta_11, guide = "none") +
  scale_x_continuous(breaks = anios_breaks) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  facet_wrap(~ municipio, ncol = 4, scales = "free_y") +
  labs(
    title    = "Brecha urbano-rural en tasa de desempleo — Corredor Urabá, 2017-2023",
    subtitle = "Un panel por municipio. Valores positivos: mayor desempleo urbano. Línea punteada: paridad urbano-rural.",
    x = "Año", y = "Desempleo urbano − rural (pp)"
  ) +
  tema_base +
  theme(
    strip.text       = element_text(face = "bold", size = 9),
    panel.border     = element_rect(color = "grey85", fill = NA, linewidth = 0.4),
    panel.spacing    = unit(0.8, "lines")
  )

guardar("g8_desempleo_brecha_small_multiples.png", ancho = 14, alto = 10)

# =============================================================================
# G9 — Calidad de vivienda total (manejo explícito de NAs)
# =============================================================================

# G9 — Small multiples calidad vivienda con escala legible
mun_incompletos <- c("Arboletes", "Murindó", "San Juan de Urabá", "Vigía del Fuerte")

g9_data <- panel |>
  mutate(sin_dato_2023 = municipio %in% mun_incompletos & anio == 2023)

g9 <- ggplot(g9_data, aes(x = anio, y = calidad_vivienda_total)) +
  geom_line(aes(color = municipio), linewidth = 1, na.rm = TRUE) +
  geom_point(aes(color = municipio), size = 2, na.rm = TRUE) +
  geom_point(data = filter(g9_data, sin_dato_2023),
             aes(x = anio, y = 0), shape = 4, size = 3,
             stroke = 1.2, color = "grey50", show.legend = FALSE) +
  scale_color_manual(values = paleta_11, guide = "none") +
  scale_x_continuous(breaks = anios_breaks) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  facet_wrap(~ municipio, ncol = 4, scales = "free_y") +
  labs(
    title    = "Evolución de la calidad de vivienda (cuantificador ACP) — Corredor Urabá, 2017-2023",
    subtitle = "Un panel por municipio. Cruces (✕) en y = 0 indican ausencia de dato en 2023 por CV > 15%.",
    x = "Año", y = "Cuantificador ACP"
  ) +
  tema_base +
  theme(
    strip.text    = element_text(face = "bold", size = 9),
    panel.border  = element_rect(color = "grey85", fill = NA, linewidth = 0.4),
    panel.spacing = unit(0.8, "lines")
  )

guardar("g9_calidad_vivienda_small_multiples.png", ancho = 14, alto = 10)

# =============================================================================
# G10 — Small multiples: cuantificador de estrato por municipio y año
# =============================================================================

g10_data <- panel |>
  mutate(anio_f = factor(anio))

g10 <- ggplot(g10_data, aes(x = anio_f, y = estrato_total, fill = municipio)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 1.0501, linetype = "dashed", color = "grey40",
             linewidth = 0.5) +
  annotate("text", x = 0.5, y = 1.0501, label = "Estrato 2",
           hjust = 0, vjust = -0.4, size = 2.8, color = "grey40",
           fontface = "italic") +
  scale_fill_manual(values = paleta_11, guide = "none") +
  scale_y_continuous(limits = c(0, 1.2),
                     breaks = c(0, 0.5, 1.0501),
                     labels = c("0", "0.50", "1.05\n(Estrato 2)")) +
  facet_wrap(~ municipio, ncol = 4) +
  labs(
    title    = "Cuantificador ACP de estrato de la vivienda — Corredor Urabá, 2017–2023",
    subtitle = "Un panel por municipio. Línea punteada: umbral de estrato 2 (cuantificador = 1.0501). Todos los municipios se ubican entre estrato 1 y estrato 2.",
    x = "Año", y = "Cuantificador ACP"
  ) +
  tema_base +
  theme(
    strip.text    = element_text(face = "bold", size = 9),
    panel.border  = element_rect(color = "grey85", fill = NA, linewidth = 0.4),
    panel.spacing = unit(0.8, "lines"),
    axis.text.x   = element_text(size = 8)
  )

guardar("g10_estrato_comparacion.png", ancho = 14, alto = 10)

# =============================================================================
# VERIFICACIÓN FINAL
# =============================================================================

archivos_generados <- list.files(ruta_graf)
message("\nArchivos generados en ", ruta_graf, ":")
print(archivos_generados)
# Debe mostrar g1_ a g10_ con extensión .png

list.files(ruta_graf, pattern = "g10")
# Debe mostrar g10_estrato_comparacion.png
