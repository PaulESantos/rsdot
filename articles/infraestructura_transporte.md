# Infraestructura de Transporte en el Perú

## Introducción

El paquete `rsdot` proporciona acceso a datos de infraestructura de
transporte del Perú a través del Visor SDOT (Sistema de Difusión de Obra
Territorial) del Ministerio de Transportes y Comunicaciones. Esta viñeta
muestra cómo acceder y analizar información sobre:

- **Red Vial Nacional**: Carreteras principales que conectan el país
- **Red Vial Departamental**: Vías que conectan provincias dentro de
  cada departamento
- **Red Vial Vecinal**: Carreteras rurales que conectan centros poblados
  menores
- **Aeródromos**: Aeropuertos, aeródromos y helipuertos

Todos los datos provienen de PROVÍAS NACIONAL y PROVÍAS DESCENTRALIZADO,
actualizados a julio 2022 según el Clasificador de Rutas (DS
011-2016-MTC).

``` r
library(rsdot)
library(sf)
library(dplyr)
library(ggplot2)
```

## Red Vial Nacional

La Red Vial Nacional comprende las principales carreteras que conectan
el territorio peruano, incluyendo las longitudinales de la costa, sierra
y selva, así como las carreteras transversales.

### Carga de datos

``` r
# Cargar red vial nacional de Cusco
vias_nac_cusco <- get_red_vial_nacional(departamento = "CUSCO")

# Ver estructura de los datos
head(vias_nac_cusco)
```

### Análisis básico

``` r
# Estadísticas generales
vias_nac_cusco |>
  st_drop_geometry() |>
  summarise(
    longitud_total_km = sum(longitud, na.rm = TRUE),
    n_rutas = n_distinct(codruta),
    n_tramos = n(),
    km_pavimentado = sum(longitud[superfic_l == "Pavimentado"], na.rm = TRUE),
    pct_pavimentado = (km_pavimentado / longitud_total_km) * 100
  )

# Análisis por eje de clasificación
vias_nac_cusco |>
  st_drop_geometry() |>
  group_by(ejeclas) |>
  summarise(
    longitud_km = sum(longitud, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(longitud_km))
```

### Visualización

``` r
# Mapa de red vial nacional por código de ruta
ggplot(vias_nac_cusco) +
  geom_sf(aes(color = codruta), linewidth = 1.2) +
  labs(
    title = "Red Vial Nacional - Departamento de Cusco",
    subtitle = "Códigos de Ruta",
    color = "Código",
    caption = "Fuente: PROVÍAS NACIONAL - MTC 2022"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11)
  )

# Mapa por estado de conservación
ggplot(vias_nac_cusco) +
  geom_sf(aes(color = estado_l, linewidth = estado_l)) +
  scale_color_manual(
    values = c(
      "Bueno" = "#2ecc71",
      "Regular" = "#f39c12",
      "Malo" = "#e74c3c"
    ),
    name = "Estado"
  ) +
  scale_linewidth_manual(
    values = c("Bueno" = 1.2, "Regular" = 0.9, "Malo" = 0.6),
    guide = "none"
  ) +
  labs(
    title = "Estado de Conservación - Red Vial Nacional",
    subtitle = "Cusco",
    caption = "Fuente: PROVÍAS NACIONAL - MTC 2022"
  ) +
  theme_minimal()
```

## Red Vial Departamental

Las carreteras departamentales conectan las capitales provinciales y
centros poblados importantes dentro de cada departamento.

### Carga y análisis

``` r
# Cargar red vial departamental
vias_dep_cusco <- get_red_vial_departamental(departamento = "CUSCO")

# Análisis por provincia
analisis_prov <- vias_dep_cusco |>
  st_drop_geometry() |>
  group_by(nombprov) |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    n_rutas = n_distinct(codruta),
    km_pavimentado = sum(longitud[superfic_l == "PAVIMENTADO"], na.rm = TRUE),
    pct_pavimentado = (km_pavimentado / longitud_total) * 100,
    .groups = "drop"
  ) |>
  arrange(desc(longitud_total))

print(analisis_prov)
```

### Visualización por tipo de superficie

``` r
# Mapa por tipo de superficie
ggplot(vias_dep_cusco) +
  geom_sf(aes(color = superfic_l), linewidth = 0.8) +
  scale_color_manual(
    values = c(
      "PAVIMENTADO" = "#3498db",
      "AFIRMADO" = "#27ae60",
      "SIN AFIRMAR" = "#e67e22",
      "TROCHA" = "#95a5a6"
    ),
    name = "Superficie"
  ) +
  labs(
    title = "Red Vial Departamental por Tipo de Superficie",
    subtitle = "Departamento de Cusco",
    caption = "Fuente: PROVÍAS NACIONAL - MTC 2022"
  ) +
  theme_minimal()
```

### Filtrado por provincia

``` r
# Cargar solo una provincia específica
vias_prov_cusco <- get_red_vial_departamental(
  departamento = "CUSCO",
  provincia = "CUSCO"
)

# Visualizar
ggplot(vias_prov_cusco) +
  geom_sf(aes(color = estado_l), linewidth = 1) +
  scale_color_manual(
    values = c("BUENO" = "darkgreen", "REGULAR" = "orange", "MALO" = "red")
  ) +
  labs(
    title = "Red Vial Departamental - Provincia de Cusco",
    color = "Estado"
  ) +
  theme_minimal()
```

## Red Vial Vecinal

La Red Vial Vecinal o Rural conecta centros poblados menores, caseríos y
zonas de producción. Es administrada por los gobiernos locales.

### Características de la red vecinal

``` r
# Cargar red vial vecinal
vias_vec_cusco <- get_red_vial_vecinal(departamento = "CUSCO")

# Análisis de superficie
vias_vec_cusco |>
  st_drop_geometry() |>
  group_by(superfic_l) |>
  summarise(
    longitud_km = sum(longitud, na.rm = TRUE),
    n_tramos = n(),
    porcentaje = (longitud_km / sum(vias_vec_cusco$longitud, na.rm = TRUE)) * 100,
    .groups = "drop"
  ) |>
  arrange(desc(longitud_km))

# Análisis de estado
vias_vec_cusco |>
  st_drop_geometry() |>
  group_by(estado_l) |>
  summarise(
    longitud_km = sum(longitud, na.rm = TRUE),
    porcentaje = (longitud_km / sum(vias_vec_cusco$longitud, na.rm = TRUE)) * 100,
    .groups = "drop"
  )
```

### Identificación de prioridades

``` r
# Identificar vías en mal estado por provincia
vias_mal_estado <- vias_vec_cusco |>
  filter(estado_l == "Malo") |>
  st_drop_geometry() |>
  group_by(nombprov) |>
  summarise(
    km_mal_estado = sum(longitud, na.rm = TRUE),
    n_vias = n(),
    .groups = "drop"
  ) |>
  arrange(desc(km_mal_estado))

print(vias_mal_estado)
```

### Visualización

``` r
# Mapa de red vecinal por estado
ggplot(vias_vec_cusco) +
  geom_sf(aes(color = estado_l), linewidth = 0.6, alpha = 0.7) +
  scale_color_manual(
    values = c(
      "Bueno" = "#27ae60",
      "Regular" = "#f39c12",
      "Malo" = "#c0392b"
    ),
    name = "Estado"
  ) +
  labs(
    title = "Red Vial Vecinal - Departamento de Cusco",
    subtitle = "Estado de Conservación",
    caption = "Fuente: PROVÍAS DESCENTRALIZADO - MTC 2022"
  ) +
  theme_minimal()
```

## Aeródromos

Los datos de aeródromos incluyen aeropuertos, aeródromos y helipuertos
en todo el Perú.

### Carga y exploración

``` r
# Cargar aeródromos de Cusco
aero_cusco <- get_aerodromos(departamento = "CUSCO")

# Ver tipos de instalaciones
table(aero_cusco$tipo)

# Análisis por tipo y titularidad
aero_cusco |>
  st_drop_geometry() |>
  group_by(tipo, titular) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(tipo, desc(n))
```

### Visualización

``` r
# Mapa de aeródromos
ggplot(aero_cusco) +
  geom_sf(aes(color = tipo, shape = tipo), size = 4) +
  scale_color_manual(
    values = c(
      "AEROPUERTO INTERNACIONAL" = "#e74c3c",
      "AERODROMO" = "#3498db",
      "HELIPUERTO" = "#2ecc71"
    ),
    name = "Tipo"
  ) +
  scale_shape_manual(
    values = c(
      "AEROPUERTO INTERNACIONAL" = 17,
      "AERODROMO" = 16,
      "HELIPUERTO" = 15
    ),
    name = "Tipo"
  ) +
  labs(
    title = "Aeródromos del Departamento de Cusco",
    subtitle = "Actualizado 2022 - MTC",
    caption = "Fuente: MTC | Visor SDOT"
  ) +
  theme_minimal()

# Mapa con etiquetas
ggplot(aero_cusco) +
  geom_sf(aes(color = tipo), size = 3) +
  geom_sf_text(
    aes(label = nombre),
    size = 2.5,
    nudge_y = 0.1,
    check_overlap = TRUE
  ) +
  scale_color_brewer(palette = "Set1", name = "Tipo") +
  labs(
    title = "Aeródromos de Cusco con Nombres",
    caption = "Fuente: MTC 2022"
  ) +
  theme_minimal()
```

## Análisis Integrado

### Comparación entre redes viales

``` r
# Cargar las tres redes
vias_nacional <- get_red_vial_nacional(departamento = "CUSCO")
vias_departamental <- get_red_vial_departamental(departamento = "CUSCO")
vias_vecinal <- get_red_vial_vecinal(departamento = "CUSCO")

# Crear resumen comparativo
comparacion <- data.frame(
  Red = c("Nacional", "Departamental", "Vecinal"),
  Longitud_km = c(
    sum(vias_nacional$longitud, na.rm = TRUE),
    sum(vias_departamental$longitud, na.rm = TRUE),
    sum(vias_vecinal$longitud, na.rm = TRUE)
  ),
  N_tramos = c(
    nrow(vias_nacional),
    nrow(vias_departamental),
    nrow(vias_vecinal)
  )
)

comparacion <- comparacion |>
  mutate(
    Porcentaje = (Longitud_km / sum(Longitud_km)) * 100,
    Km_promedio_tramo = Longitud_km / N_tramos
  )

print(comparacion)
```

### Visualización comparativa

``` r
# Gráfico de barras comparativo
ggplot(comparacion, aes(x = Red, y = Longitud_km, fill = Red)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(Longitud_km, 0), " km\n",
                       round(Porcentaje, 1), "%")),
    vjust = -0.5,
    size = 3.5
  ) +
  scale_fill_manual(
    values = c(
      "Nacional" = "#e74c3c",
      "Departamental" = "#3498db",
      "Vecinal" = "#2ecc71"
    )
  ) +
  labs(
    title = "Comparación de Redes Viales en Cusco",
    subtitle = "Longitud total por tipo de red",
    x = "Tipo de Red Vial",
    y = "Longitud Total (km)",
    caption = "Fuente: PROVÍAS NACIONAL/DESCENTRALIZADO - MTC 2022"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

### Mapa integrado de infraestructura

``` r
# Mapa con todas las redes y aeródromos
ggplot() +
  # Red Nacional (base)
  geom_sf(
    data = vias_nacional,
    aes(color = "Nacional"),
    linewidth = 1.2
  ) +
  # Red Departamental
  geom_sf(
    data = vias_departamental,
    aes(color = "Departamental"),
    linewidth = 0.8
  ) +
  # Red Vecinal
  geom_sf(
    data = vias_vecinal,
    aes(color = "Vecinal"),
    linewidth = 0.4,
    alpha = 0.5
  ) +
  # Aeródromos
  geom_sf(
    data = aero_cusco,
    aes(shape = tipo),
    size = 3,
    color = "black",
    fill = "yellow"
  ) +
  scale_color_manual(
    values = c(
      "Nacional" = "#e74c3c",
      "Departamental" = "#3498db",
      "Vecinal" = "#95a5a6"
    ),
    name = "Red Vial"
  ) +
  scale_shape_manual(
    values = c(
      "AEROPUERTO INTERNACIONAL" = 24,
      "AERODROMO" = 22,
      "HELIPUERTO" = 23
    ),
    name = "Aeródromo"
  ) +
  labs(
    title = "Infraestructura de Transporte - Departamento de Cusco",
    subtitle = "Redes Viales y Aeródromos",
    caption = "Fuente: PROVÍAS NACIONAL/DESCENTRALIZADO, MTC 2022"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )
```

## Análisis Multidepartamental

### Comparación entre departamentos del sur

``` r
# Cargar red nacional de varios departamentos
vias_sur <- get_red_vial_nacional(
  departamento = c("CUSCO", "PUNO", "AREQUIPA", "APURIMAC")
)

# Análisis por departamento
resumen_sur <- vias_sur |>
  st_drop_geometry() |>
  group_by(nombdep) |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    n_rutas = n_distinct(codruta),
    km_pavimentado = sum(longitud[superfic_l == "Pavimentado"], na.rm = TRUE),
    pct_pavimentado = (km_pavimentado / longitud_total) * 100,
    km_buen_estado = sum(longitud[estado_l == "Bueno"], na.rm = TRUE),
    pct_buen_estado = (km_buen_estado / longitud_total) * 100,
    .groups = "drop"
  ) |>
  arrange(desc(longitud_total))

print(resumen_sur)
```

### Visualización regional

``` r
# Mapa regional
ggplot(vias_sur) +
  geom_sf(aes(color = nombdep), linewidth = 1) +
  scale_color_brewer(palette = "Set1", name = "Departamento") +
  labs(
    title = "Red Vial Nacional - Región Sur del Perú",
    subtitle = "Cusco, Puno, Arequipa y Apurímac",
    caption = "Fuente: PROVÍAS NACIONAL - MTC 2022"
  ) +
  theme_minimal()

# Gráfico comparativo de estado
vias_sur |>
  st_drop_geometry() |>
  group_by(nombdep, estado_l) |>
  summarise(km = sum(longitud, na.rm = TRUE), .groups = "drop") |>
  ggplot(aes(x = nombdep, y = km, fill = estado_l)) +
  geom_col(position = "fill") +
  scale_fill_manual(
    values = c("Bueno" = "#2ecc71", "Regular" = "#f39c12", "Malo" = "#e74c3c"),
    name = "Estado"
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proporción de Estado de Vías por Departamento",
    subtitle = "Red Vial Nacional - Sur del Perú",
    x = "Departamento",
    y = "Porcentaje",
    caption = "Fuente: PROVÍAS NACIONAL"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

## Análisis de Accesibilidad

### Conectividad aérea

``` r
# Cargar aeródromos de múltiples departamentos
aero_sur <- get_aerodromos(
  departamento = c("CUSCO", "PUNO", "AREQUIPA", "APURIMAC")
)

# Resumen por departamento
aero_sur |>
  st_drop_geometry() |>
  group_by(nombdep, tipo) |>
  summarise(n = n(), .groups = "drop") |>
  tidyr::pivot_wider(names_from = tipo, values_from = n, values_fill = 0)

# Aeródromos operativos públicos
aero_publicos <- aero_sur |>
  filter(titular == "PUBLICA", estado == "OPERATIVO")

nrow(aero_publicos)
```

## Casos de Uso Prácticos

### Caso 1: Planificación de rutas

``` r
# Identificar la ruta principal entre Cusco y Lima
# PE-3S es la carretera principal

ruta_principal <- vias_nacional |>
  filter(codruta == "PE-3S")

# Análisis de la ruta
ruta_principal |>
  st_drop_geometry() |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    km_pavimentado = sum(longitud[superfic_l == "Pavimentado"], na.rm = TRUE),
    km_afirmado = sum(longitud[superfic_l == "Afirmado"], na.rm = TRUE)
  )
```

### Caso 2: Evaluación de necesidades de mantenimiento

``` r
# Identificar tramos que requieren intervención urgente
# (mal estado y alta importancia)

vias_prioritarias <- vias_departamental |>
  filter(
    estado_l == "MALO",
    superfic_l %in% c("PAVIMENTADO", "AFIRMADO")
  ) |>
  arrange(desc(longitud))

# Resumen por provincia
vias_prioritarias |>
  st_drop_geometry() |>
  group_by(nombprov) |>
  summarise(
    km_requiere_atencion = sum(longitud, na.rm = TRUE),
    n_tramos = n(),
    .groups = "drop"
  ) |>
  arrange(desc(km_requiere_atencion))
```

### Caso 3: Análisis de inversión rural

``` r
# Analizar el estado de la red vecinal
# para priorizar inversiones

analisis_rural <- vias_vecinal |>
  st_drop_geometry() |>
  group_by(nombprov) |>
  summarise(
    km_total = sum(longitud, na.rm = TRUE),
    km_trocha = sum(longitud[superfic_l == "Trocha"], na.rm = TRUE),
    pct_trocha = (km_trocha / km_total) * 100,
    km_malo = sum(longitud[estado_l == "Malo"], na.rm = TRUE),
    pct_malo = (km_malo / km_total) * 100,
    .groups = "drop"
  ) |>
  arrange(desc(pct_trocha))

# Provincias con mayor necesidad de mejoramiento
print(head(analisis_rural, 10))
```

## Exportación de Datos

### Guardar en formatos estándar

``` r
# Exportar a GeoPackage
st_write(
  vias_nacional,
  "red_vial_nacional_cusco.gpkg",
  delete_dsn = TRUE
)

# Exportar a Shapefile
st_write(
  aero_cusco,
  "aerodromos_cusco.shp",
  delete_dsn = TRUE
)

# Exportar datos tabulares a CSV
vias_nacional |>
  st_drop_geometry() |>
  write.csv("red_nacional_cusco_datos.csv", row.names = FALSE)
```

## Recursos Adicionales

### Fuentes de datos

- **Visor SDOT**: <https://geosdot.servicios.gob.pe/visor/>
- **PROVÍAS NACIONAL**: <https://www.gob.pe/proviasnac>
- **PROVÍAS DESCENTRALIZADO**: <https://www.gob.pe/proviasdes>
- **Repositorio OSF**: <https://osf.io/qy4j6/>

### Normativa relevante

- Decreto Supremo 011-2016-MTC: Clasificador de Rutas del SINAC
- Ley 27181: Ley General de Transporte y Tránsito Terrestre

### Funciones relacionadas

- [`get_departamentos()`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md):
  Límites departamentales
- [`get_provincias()`](https://paulesantos.github.io/rsdot/reference/get_provincias.md):
  Límites provinciales
- [`get_distritos()`](https://paulesantos.github.io/rsdot/reference/get_distritos.md):
  Límites distritales
- [`get_centros_poblados_crecimiento()`](https://paulesantos.github.io/rsdot/reference/get_centros_poblados_crecimiento.md):
  Datos demográficos de centros poblados

## Conclusión

El paquete `rsdot` facilita el acceso y análisis de datos de
infraestructura de transporte en el Perú. Las funciones presentadas
permiten:

1.  **Análisis espacial** de redes viales a diferentes niveles
    jerárquicos
2.  **Evaluación de estado** de infraestructura vial
3.  **Planificación** de inversiones en transporte
4.  **Estudios de conectividad** territorial
5.  **Análisis multidepartamental** para estudios regionales

Para más información sobre otras funcionalidades del paquete, consulte
las viñetas adicionales y la documentación de cada función.
