# Genera metodologia_graficos_ecv.docx con la librería officer

library(officer)
library(flextable)

ruta_doc <- "C:/Users/jimen/Documents/metodologia_graficos_ecv/metodologia_graficos_ecv.docx"
dir.create(dirname(ruta_doc), recursive = TRUE, showWarnings = FALSE)

# Función auxiliar para párrafo con estilo Normal
p <- function(doc, texto, bold = FALSE, italic = FALSE) {
  run <- fpar(
    ftext(texto, fp_text(bold = bold, italic = italic, font.size = 11,
                         font.family = "Calibri"))
  )
  body_add_fpar(doc, run, style = "Normal")
}

heading1 <- function(doc, texto) {
  body_add_par(doc, texto, style = "heading 1")
}

heading2 <- function(doc, texto) {
  body_add_par(doc, texto, style = "heading 2")
}

doc <- read_docx()

# Portada
body_add_fpar(doc,
  fpar(ftext("Notas metodológicas sobre las gráficas del panel ECV Urabá 2017-2023",
             fp_text(bold = TRUE, font.size = 16, font.family = "Calibri")),
       fp_p = fp_par(text.align = "center")))
body_add_par(doc, "")
body_add_fpar(doc,
  fpar(ftext("Encuesta de Calidad de Vida — Departamento Administrativo de Planeación de Antioquia",
             fp_text(italic = TRUE, font.size = 12, font.family = "Calibri")),
       fp_p = fp_par(text.align = "center")))
body_add_par(doc, "")

# ============================================================
# SECCIÓN 1 — Fuente de datos
# ============================================================
heading1(doc, "1. Fuente de datos")

p(doc, paste0(
  "Las gráficas se construyeron a partir de un panel longitudinal derivado de la ",
  "Encuesta de Calidad de Vida (ECV) del Departamento Administrativo de Planeación ",
  "de Antioquia. El panel cubre 11 municipios del corredor de Urabá (Apartadó, ",
  "Arboletes, Carepa, Chigorodó, Murindó, Mutatá, Necoclí, San Juan de Urabá, ",
  "San Pedro de Urabá, Turbo y Vigía del Fuerte) en cuatro cortes transversales: ",
  "2017, 2019, 2021 y 2023. La estructura resultante es un panel balanceado de ",
  "44 observaciones (11 municipios × 4 años) con 21 variables."
))
body_add_par(doc, "")

p(doc, paste0(
  "Los seis indicadores centrales son: Índice Multidimensional de Condiciones de ",
  "Vida (IMCV), estrato socioeconómico, calidad de vivienda, número de servicios ",
  "públicos instalados, número de servicios públicos suspendidos y tasa de ",
  "desempleo. Cada indicador cuenta con desagregación total, urbana y rural, ",
  "para un total de 18 variables de interés más tres variables de identificación ",
  "(código DANE, nombre de municipio y año)."
))
body_add_par(doc, "")

# ============================================================
# SECCIÓN 2 — Nota sobre cuantificadores ACP
# ============================================================
heading1(doc, "2. Nota sobre cuantificadores ACP")

p(doc, paste0(
  "Los indicadores de estrato, calidad de vivienda, número de servicios públicos ",
  "instalados y número de servicios públicos suspendidos no se expresan en sus ",
  "unidades originales —categorías discretas o conteos— sino como cuantificadores ",
  "óptimos derivados del Análisis de Componentes Principales (ACP) aplicado en la ",
  "construcción del IMCV. Este procedimiento asigna a cada categoría de cada ",
  "variable un valor numérico que maximiza la varianza explicada por la primera ",
  "componente principal, de modo que las distancias entre categorías reflejan su ",
  "peso relativo en la determinación de las condiciones de vida. Los cuantificadores ",
  "se recalibran en cada edición de la ECV, lo que implica que sus valores absolutos ",
  "no son directamente comparables entre años: un mismo valor numérico en 2017 y en ",
  "2023 no representa necesariamente la misma condición. Por esta razón, la ",
  "comparación temporal de estas variables debe interpretarse como indicativa de ",
  "tendencias y no como medición exacta de cambios en magnitud. Para el análisis de ",
  "evolución entre municipios dentro de un mismo año, los cuantificadores son ",
  "plenamente comparables dado que se derivan de un único ejercicio de estimación."
))
body_add_par(doc, "")

# ============================================================
# SECCIÓN 3 — Decisiones metodológicas
# ============================================================
heading1(doc, "3. Decisiones metodológicas")

heading2(doc, "3.1. Filtro de coeficiente de variación")
p(doc, paste0(
  "El filtro de CV ≤ 15% se aplica a todos los indicadores excepto tasa de desempleo. ",
  "La tasa de desempleo supera el umbral de CV en la mayoría de municipios de Urabá en ",
  "2021 y 2023 debido al tamaño muestral reducido. Sus valores se incluyen en las ",
  "gráficas sin filtro y deben interpretarse con precaución, especialmente en municipios ",
  "con muestras pequeñas (municipios rurales dispersos como Murindó y Vigía del Fuerte)."
))
body_add_par(doc, "")

heading2(doc, "3.2. Exclusión de estrato de las gráficas principales")
p(doc, paste0(
  "La variable estrato socioeconómico se excluye de todas las gráficas de evolución ",
  "temporal porque su cobertura es insuficiente en 2021 y 2023: el CV supera el 15% en ",
  "la mayoría de municipios en esos años. Se documenta únicamente como variable de ",
  "contexto en las estadísticas descriptivas internas del script."
))
body_add_par(doc, "")

heading2(doc, "3.3. Datos urbano/rural faltantes en 2017")
p(doc, paste0(
  "La desagregación urbano/rural no está disponible en 2017 para los indicadores de ",
  "calidad de vivienda, número de servicios públicos instalados y suspendidos. ",
  "La tasa de desempleo sí cuenta con desagregación en los cuatro años. Las gráficas ",
  "de brecha urbano-rural para servicios públicos (G6) se restringen a 2019-2023 por ",
  "esta razón."
))
body_add_par(doc, "")

heading2(doc, "3.4. Manejo de valores faltantes (NA) en gráficas de líneas")
p(doc, paste0(
  "En las gráficas de evolución temporal con muchos valores faltantes (calidad de ",
  "vivienda, servicios públicos), se utilizan segmentos de línea interrumpidos: los ",
  "puntos disponibles se muestran con geom_point() y las líneas solo conectan ",
  "observaciones consecutivas no faltantes. No se elimina ninguna fila completa del ",
  "panel por ausencia de datos en una sola variable."
))
body_add_par(doc, "")

heading2(doc, "3.5. Comparabilidad temporal de cuantificadores ACP")
p(doc, paste0(
  "Véase Sección 2. Esta limitación afecta directamente a las gráficas G5, G6 y G9 ",
  "(servicios públicos y calidad de vivienda) y en menor medida a G3. Las ",
  "interpretaciones de estas gráficas señalan tendencias y no cambios en magnitud exacta."
))
body_add_par(doc, "")

# ============================================================
# SECCIÓN 4 — Descripción de cada gráfica
# ============================================================
heading1(doc, "4. Descripción de las gráficas")

graficas <- list(
  list(
    id = "G1", archivo = "g1_imcv_evolucion_temporal.png",
    titulo = "Evolución del IMCV total por municipio (2017-2023)",
    vars = "IMCV_total, anio, municipio",
    tipo = "Gráfica de líneas",
    decisiones = paste0(
      "Se resaltan con línea más gruesa los municipios con mayor mejora (Murindó) y ",
      "mayor deterioro (Arboletes). Una línea punteada horizontal muestra el promedio ",
      "del corredor por año. Etiquetas al final de las líneas resaltadas. Expansión del ",
      "eje X para acomodar etiquetas sin solaparse."
    ),
    hallazgos = paste0(
      "El promedio del corredor subió de 26.76 en 2017 a 27.68 en 2021 y retrocedió a ",
      "27.46 en 2023. Murindó y Vigía del Fuerte muestran las trayectorias más positivas; ",
      "Arboletes y Turbo retroceden. El salto de Carepa y la caída de Apartadó en 2023 ",
      "son señales para indagación posterior."
    )
  ),
  list(
    id = "G2", archivo = "g2_imcv_distribucion_anual.png",
    titulo = "Distribución del IMCV entre municipios por año",
    vars = "IMCV_total, anio",
    tipo = "Boxplot + jitter con etiquetas",
    decisiones = paste0(
      "Un boxplot por año con los 11 municipios superpuestos como puntos. Se usan ",
      "etiquetas ggrepel para los extremos y municipios resaltados, evitando solapamiento."
    ),
    hallazgos = paste0(
      "La dispersión entre municipios se redujo (std de 2.60 en 2017 a 2.20 en 2023), ",
      "sugiriendo convergencia parcial. La mediana del corredor se mantiene relativamente ",
      "estable entre 2019 y 2023."
    )
  ),
  list(
    id = "G3", archivo = "g3_imcv_comparacion_municipios.png",
    titulo = "Comparación del IMCV entre municipios por año (barras facetadas)",
    vars = "IMCV_total, municipio, anio",
    tipo = "Barras horizontales facetadas por año",
    decisiones = paste0(
      "Barras coloreadas según posición respecto al promedio del corredor (cálido = sobre ",
      "el promedio, frío = bajo). Línea punteada vertical marca el promedio. Municipios ",
      "ordenados de mayor a menor IMCV dentro de cada panel."
    ),
    hallazgos = paste0(
      "Apartadó encabeza el corredor en todos los años. Murindó escala posiciones entre ",
      "2017 y 2021. La polarización entre municipios altos y bajos se mantiene a lo largo ",
      "del período."
    )
  ),
  list(
    id = "G4", archivo = "g4_imcv_brecha_urbano_rural.png",
    titulo = "Brecha urbano-rural del IMCV (2017-2023)",
    vars = "IMCV_urbano, IMCV_rural, anio, municipio",
    tipo = "Gráfica de líneas con referencia en y = 0",
    decisiones = paste0(
      "Eje Y = IMCV_urbano − IMCV_rural. Línea de referencia en cero. Se resaltan Mutatá ",
      "(único con brecha negativa en 2023) y Apartadó (brecha más alta sostenida)."
    ),
    hallazgos = paste0(
      "La brecha promedio se redujo de 4.60 pp en 2017 a 1.97 pp en 2023. El mecanismo ",
      "es convergencia por abajo: lo rural mejora mientras lo urbano se estanca. Mutatá es ",
      "el único municipio donde en 2023 la zona rural supera a la urbana (−1.92 pp)."
    )
  ),
  list(
    id = "G5", archivo = "g5_servicios_pub_evolucion_temporal.png",
    titulo = "Evolución del cuantificador ACP de servicios públicos instalados",
    vars = "num_servicios_pub_total, anio, municipio",
    tipo = "Gráfica de líneas",
    decisiones = paste0(
      "Estructura idéntica a G1. Se maneja na.rm = FALSE para no conectar puntos ",
      "donde faltan datos (3 NAs en total). Nota metodológica sobre naturaleza ACP ",
      "del indicador incluida en el pie de gráfica."
    ),
    hallazgos = paste0(
      "Caída generalizada del cuantificador en todos los municipios entre 2017 y 2023 ",
      "(media de 1.57 a 0.91, −42%). Es la señal más llamativa del panel. Debe ",
      "contrastarse con datos administrativos de EPM para determinar si refleja deterioro ",
      "real o recalibración metodológica del ACP."
    )
  ),
  list(
    id = "G6", archivo = "g6_servicios_pub_brecha_urbano_rural.png",
    titulo = "Brecha urbano-rural en servicios públicos instalados (2019-2023)",
    vars = "num_servicios_pub_urbano, num_servicios_pub_rural, anio, municipio",
    tipo = "Barras agrupadas horizontales",
    decisiones = paste0(
      "Solo años 2019-2023 por ausencia de desagregación urbano/rural en 2017. ",
      "Barras agrupadas por año con paleta Set2. Se filtran NAs en la diferencia calculada."
    ),
    hallazgos = paste0(
      "La brecha urbano-rural en servicios es positiva en la mayoría de municipios y años, ",
      "indicando mayor cobertura urbana. Algunos municipios muestran reducción de la brecha ",
      "entre 2019 y 2023."
    )
  ),
  list(
    id = "G7", archivo = "g7_desempleo_evolucion_temporal.png",
    titulo = "Evolución de la tasa de desempleo total (2017-2023)",
    vars = "tasa_desempleo_total, anio, municipio",
    tipo = "Gráfica de líneas con banda sombreada",
    decisiones = paste0(
      "Banda sombreada amarilla para el período COVID (2019-2021). Se resaltan Apartadó ",
      "(mayor mejora), Necoclí y Carepa (mayor deterioro) y Turbo (alta volatilidad). ",
      "Advertencia explícita en pie sobre ausencia de filtro CV."
    ),
    hallazgos = paste0(
      "El promedio del corredor subió de 8.15% en 2017 a 10.49% en 2023. Apartadó es ",
      "el único con reducción sostenida (−7.14 pp). Necoclí (+10.36 pp) y Carepa ",
      "(+9.10 pp) muestran el mayor deterioro. Alta heterogeneidad (std entre 3.37 y 7.30)."
    )
  ),
  list(
    id = "G8", archivo = "g8_desempleo_brecha_urbano_rural.png",
    titulo = "Brecha urbano-rural en tasa de desempleo (2017-2023)",
    vars = "tasa_desempleo_urbano, tasa_desempleo_rural, anio, municipio",
    tipo = "Gráfica de líneas con referencia en y = 0",
    decisiones = paste0(
      "Eje Y = desempleo_urbano − desempleo_rural. Línea de referencia en cero. Se resaltan ",
      "Turbo (inversión de brecha en 2021) y Chigorodó (brecha más alta en 2019)."
    ),
    hallazgos = paste0(
      "La brecha urbano-rural se redujo en 2021 (posible efecto COVID que niveló los ",
      "mercados laborales) y se reconstituyó parcialmente en 2023. Turbo muestra inversión ",
      "de la brecha en 2021: el desempleo rural superó al urbano ese año."
    )
  ),
  list(
    id = "G9", archivo = "g9_calidad_vivienda_evolucion.png",
    titulo = "Evolución de la calidad de vivienda (cuantificador ACP, 2017-2023)",
    vars = "calidad_vivienda_total, anio, municipio",
    tipo = "Gráfica de líneas con puntos y segmentos interrumpidos",
    decisiones = paste0(
      "Los NAs se manejan explícitamente: las líneas solo conectan puntos disponibles ",
      "consecutivos (na.rm = FALSE). Los municipios sin dato en 2023 se marcan con cruz (✕). ",
      "Nota en pie especifica cuáles municipios tienen cobertura insuficiente ese año."
    ),
    hallazgos = paste0(
      "Mejora leve pero con cobertura muy limitada en 2021 y 2023 por CV alto. Brecha ",
      "urbano-rural pronunciada y persistente. Murindó y Vigía del Fuerte mantienen valores ",
      "cercanos a 0.00 — prácticamente todos sus hogares en estrato 1 con materiales precarios."
    )
  )
)

for (g in graficas) {
  heading2(doc, paste0(g$id, ". ", g$titulo))
  p(doc, paste0("Variables: ", g$vars))
  p(doc, paste0("Tipo de gráfica: ", g$tipo))
  p(doc, paste0("Decisiones de visualización: ", g$decisiones))
  p(doc, paste0("Hallazgos principales: ", g$hallazgos))
  body_add_par(doc, "")
}

# ============================================================
# SECCIÓN 5 — Resumen integrado de hallazgos
# ============================================================
heading1(doc, "5. Resumen integrado de hallazgos")

hallazgos_resumen <- list(
  list(
    titulo = "5.1. IMCV",
    texto  = paste0(
      "El promedio del corredor subió levemente de 26.76 en 2017 a un máximo de 27.68 en ",
      "2021, pero retrocedió a 27.46 en 2023. No hay mejora sostenida. La dispersión entre ",
      "municipios se redujo (std de 2.60 a 2.20), sugiriendo convergencia parcial. Murindó ",
      "(+16.9%) y Vigía del Fuerte (+7.8%) muestran las trayectorias más positivas; Arboletes ",
      "(−5.9%) y Turbo (−3.3%) retroceden. El salto de Carepa en 2023 (de 26.49 a 31.26) y ",
      "la caída simultánea de Apartadó requieren indagación."
    )
  ),
  list(
    titulo = "5.2. Brecha urbano-rural IMCV",
    texto  = paste0(
      "Se redujo a la mitad entre 2017 (4.60 pp) y 2023 (1.97 pp). El mecanismo es convergencia ",
      "por abajo: lo rural mejora mientras lo urbano se estanca o cae. Mutatá es el único ",
      "municipio donde en 2023 la zona rural supera a la urbana (brecha negativa de −1.92 pp)."
    )
  ),
  list(
    titulo = "5.3. Servicios públicos instalados",
    texto  = paste0(
      "Caída generalizada del cuantificador en todos los municipios entre 2017 y 2023 (media ",
      "de 1.57 a 0.91, −42%). Esta es la señal más llamativa del panel y debe contrastarse con ",
      "datos administrativos de EPM para determinar si refleja deterioro real o recalibración ",
      "metodológica del ACP."
    )
  ),
  list(
    titulo = "5.4. Servicios públicos suspendidos",
    texto  = paste0(
      "Variable con efecto de saturación: casi todos los municipios se concentran cerca del ",
      "valor máximo (1.5031 = ningún servicio suspendido). Vigía del Fuerte es la excepción con ",
      "deterioro sostenido. Las suspensiones no son el problema central de acceso en Urabá; el ",
      "problema está en la instalación."
    )
  ),
  list(
    titulo = "5.5. Tasa de desempleo",
    texto  = paste0(
      "El promedio del corredor subió de 8.15% en 2017 a 10.49% en 2023. Alta heterogeneidad ",
      "entre municipios (std entre 3.37 y 7.30). Apartadó es el único con reducción sostenida ",
      "(−7.14 pp). Necoclí (+10.36 pp) y Carepa (+9.10 pp) muestran el mayor deterioro. Turbo ",
      "presenta volatilidad extrema en zona urbana. La brecha urbano-rural se redujo en 2021 ",
      "(posible efecto COVID) y se reconstituyó parcialmente en 2023."
    )
  ),
  list(
    titulo = "5.6. Calidad de vivienda",
    texto  = paste0(
      "Mejora leve pero con cobertura muy limitada en 2021 y 2023 por CV alto. Brecha ",
      "urbano-rural pronunciada y persistente. Murindó y Vigía del Fuerte mantienen valores ",
      "de 0.0000 — prácticamente todos sus hogares en estrato 1 con materiales precarios."
    )
  ),
  list(
    titulo = "5.7. Casos para indagación posterior",
    texto  = paste0(
      "(a) Salto de Carepa en IMCV y desempleo en 2023. (b) Caída simultánea de Apartadó en ",
      "IMCV en 2023. (c) Inversión de brecha urbano-rural en Mutatá 2023. (d) Volatilidad ",
      "extrema de Turbo en desempleo urbano. (e) Caída generalizada del cuantificador de ",
      "servicios públicos: ¿artefacto metodológico o deterioro real?"
    )
  )
)

for (h in hallazgos_resumen) {
  heading2(doc, h$titulo)
  p(doc, h$texto)
  body_add_par(doc, "")
}

# Guardar
print(doc, target = ruta_doc)
cat("Documento guardado:", ruta_doc, "\n")
