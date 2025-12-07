# Obtener Red Vial Nacional del Perú

Descarga y carga la información de la Red Vial Nacional actualizada por
PROVÍAS NACIONAL a julio 2022, según Clasificador de Rutas aprobado
mediante Decreto Supremo 011-2016-MTC y sus modificatorias. Los datos
incluyen geometría tipo MULTILINESTRING e información detallada sobre
trayectorias, clasificación, estado, superficie y concesiones de las
carreteras nacionales.

## Usage

``` r
get_red_vial_nacional(
  departamento = NULL,
  show_progress = TRUE,
  force_update = FALSE
)
```

## Arguments

- departamento:

  Character vector. Nombre(s) del/los departamento(s) a descargar.
  Opciones válidas: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA",
  "AYACUCHO", "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO",
  "ICA", "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE
  DE DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA",
  "TUMBES", "UCAYALI". No distingue entre mayúsculas y minúsculas. Si es
  `NULL`, muestra la lista de departamentos disponibles.

- show_progress:

  Logical. Si `TRUE` (por defecto), muestra mensajes informativos sobre
  el progreso de la descarga. Si `FALSE`, ejecuta de forma silenciosa.

- force_update:

  Logical. Si `TRUE`, fuerza una nueva descarga del archivo incluso si
  existe en caché. Por defecto `FALSE`.

## Value

Un objeto `sf` (simple feature) con geometría tipo MULTILINESTRING que
contiene información de la red vial nacional, incluyendo:

- Geometría MULTILINESTRING (trayectoria de la vía)

- `inicio`: Kilómetro de inicio del tramo

- `fin`: Kilómetro de fin del tramo

- `trayectori`: Descripción completa de la trayectoria de la ruta

- `nrocarril`: Número de carriles

- `ejeclas`: Eje de clasificación (Longitudinal de la Costa,
  Transversal, etc.)

- `iddpto`: Código del departamento

- `nombdep`: Nombre del departamento

- `codruta`: Código de la ruta (ej. PE-1S, PE-3N, PE-22A)

- `jerarq`: Jerarquía de la ruta (RN = Red Nacional)

- `superfic`: Código de tipo de superficie (1-11)

- `longitud`: Longitud del tramo en kilómetros

- `codconces`: Código de concesión (si aplica)

- `codclog`: Código de corredor logístico

- `superfic_l`: Tipo de superficie descriptivo (Pavimentado, Afirmado,
  etc.)

- `codclog_l`: Descripción del corredor logístico

- `estado`: Estado numérico de la vía (1-3)

- `estado_l`: Estado descriptivo (Bueno, Regular, Malo)

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

**Fuente de los datos:**

- Fuente: PROVÍAS NACIONAL - Ministerio de Transportes y Comunicaciones

- Actualización: Julio 2022

- Base legal: Decreto Supremo 011-2016-MTC y modificatorias

- Nivel: Red Vial Nacional (Sistema Nacional de Carreteras - SINAC)

- Aplicación: Planificación vial nacional y análisis de conectividad

**Clasificación de superficie:**

- 1 = Pavimentado

- 2 = Afirmado

- 11 = Asfaltado económico

- Otros códigos según especificaciones técnicas de PROVÍAS

**Clasificación de estado:**

- 1 = Bueno

- 2 = Regular

- 3 = Malo

**Ejes de clasificación (ejeclas):**

- Longitudinal de la Costa (PE-1N, PE-1S)

- Longitudinal de la Sierra

- Longitudinal de la Selva

- Transversal

El caché se almacena en: `tempdir()/DEMARCA_cache/red_vial_nacional/`

**NOTA:** Las geometrías son tipo MULTILINESTRING (multilíneas) y
representan las trayectorias de las carreteras nacionales. Algunos
tramos pueden estar bajo concesión (campo `codconces`).

## References

PROVÍAS NACIONAL - MTC. Base Cartográfica de la Red Vial Nacional 2022.

Decreto Supremo 011-2016-MTC - Clasificador de Rutas del Sistema
Nacional de Carreteras (SINAC).

Repositorio DEMARCA en OSF: <https://osf.io/qy4j6/>

## See also

[`get_red_vial_departamental`](https://paulesantos.github.io/rsdot/reference/get_red_vial_departamental.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ver departamentos disponibles
get_red_vial_nacional()

# Cargar red vial nacional de un departamento
vias_cusco <- get_red_vial_nacional(departamento = "CUSCO")

# Cargar múltiples departamentos
vias_sur <- get_red_vial_nacional(
  departamento = c("CUSCO", "PUNO", "AREQUIPA")
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Mapa de red vial nacional por tipo de superficie
ggplot(vias_cusco) +
  geom_sf(aes(color = superfic_l), linewidth = 1) +
  scale_color_manual(
    values = c(
      "Pavimentado" = "darkblue",
      "Afirmado" = "darkgreen",
      "Asfaltado económico" = "purple"
    ),
    name = "Superficie"
  ) +
  labs(
    title = "Red Vial Nacional de Cusco",
    subtitle = "Por Tipo de Superficie - PROVÍAS 2022",
    caption = "Fuente: PROVÍAS NACIONAL | Visor - SDOT"
  ) +
  theme_minimal()

# Mapa por estado de conservación
ggplot(vias_cusco) +
  geom_sf(aes(color = estado_l, linewidth = estado_l)) +
  scale_color_manual(
    values = c(
      "Bueno" = "darkgreen",
      "Regular" = "orange",
      "Malo" = "red"
    ),
    name = "Estado"
  ) +
  scale_linewidth_manual(
    values = c("Bueno" = 1.2, "Regular" = 0.8, "Malo" = 0.6),
    name = "Estado"
  ) +
  labs(
    title = "Estado de Conservación - Red Vial Nacional",
    subtitle = "Departamento de Cusco",
    caption = "Fuente: PROVÍAS NACIONAL | Visor - SDOT"
  ) +
  theme_minimal()

# Mapa por código de ruta
ggplot(vias_cusco) +
  geom_sf(aes(color = codruta), linewidth = 1.2) +
  labs(
    title = "Red Vial Nacional - Códigos de Ruta",
    subtitle = "Cusco",
    color = "Código de Ruta",
    caption = "Fuente: PROVÍAS NACIONAL"
  ) +
  theme_minimal()

# Análisis de longitud por tipo de superficie
vias_cusco |>
  st_drop_geometry() |>
  group_by(superfic_l) |>
  summarise(
    total_km = sum(longitud, na.rm = TRUE),
    n_tramos = n(),
    km_promedio = mean(longitud, na.rm = TRUE)
  ) |>
  arrange(desc(total_km))

# Análisis por eje de clasificación
vias_cusco |>
  st_drop_geometry() |>
  group_by(ejeclas, estado_l) |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(ejeclas, desc(longitud_total))

# Filtrar vías concesionadas
vias_concesion <- vias_cusco |>
  filter(!is.na(codconces))

# Análisis de corredores logísticos
vias_cusco |>
  st_drop_geometry() |>
  filter(!is.na(codclog_l)) |>
  group_by(codclog_l) |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    n_tramos = n(),
    .groups = "drop"
  ) |>
  arrange(desc(longitud_total))

# Análisis por número de carriles
vias_cusco |>
  st_drop_geometry() |>
  group_by(nrocarril) |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    pct = (longitud_total / sum(vias_cusco$longitud, na.rm = TRUE)) * 100,
    .groups = "drop"
  )

# Principales rutas nacionales
vias_cusco |>
  st_drop_geometry() |>
  group_by(codruta) |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    km_inicio = min(inicio, na.rm = TRUE),
    km_fin = max(fin, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(longitud_total))

# Visualizar solo carreteras longitudinales
vias_long <- vias_cusco |>
  filter(grepl("Longitudinal", ejeclas))

ggplot(vias_long) +
  geom_sf(aes(color = ejeclas), linewidth = 1.5) +
  labs(
    title = "Carreteras Longitudinales",
    subtitle = "Red Vial Nacional - Cusco",
    color = "Eje de Clasificación"
  ) +
  theme_minimal()

# Estadísticas generales
vias_cusco |>
  st_drop_geometry() |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    n_rutas = n_distinct(codruta),
    km_pavimentado = sum(longitud[superfic_l == "Pavimentado"], na.rm = TRUE),
    km_afirmado = sum(longitud[superfic_l == "Afirmado"], na.rm = TRUE),
    pct_pavimentado = (km_pavimentado / longitud_total) * 100,
    km_buen_estado = sum(longitud[estado_l == "Bueno"], na.rm = TRUE),
    pct_buen_estado = (km_buen_estado / longitud_total) * 100
  )
} # }
```
