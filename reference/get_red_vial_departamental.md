# Obtener Red Vial Departamental del Perú

Descarga y carga la información de la Red Vial Departamental actualizada
por PROVÍAS NACIONAL a julio 2022, según Clasificador de Rutas aprobado
mediante Decreto Supremo 011-2016-MTC y sus modificatorias. Los datos
incluyen geometría tipo MULTILINESTRING e información sobre
trayectorias, estado y superficie de las carreteras departamentales.
Permite filtrado escalonado por departamento y provincia.

## Usage

``` r
get_red_vial_departamental(
  departamento = NULL,
  provincia = NULL,
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

- provincia:

  Character vector. Nombre(s) de la(s) provincia(s) para filtrar dentro
  del/los departamento(s) especificado(s). Opcional.

- show_progress:

  Logical. Si `TRUE` (por defecto), muestra mensajes informativos sobre
  el progreso de la descarga. Si `FALSE`, ejecuta de forma silenciosa.

- force_update:

  Logical. Si `TRUE`, fuerza una nueva descarga del archivo incluso si
  existe en caché. Por defecto `FALSE`.

## Value

Un objeto `sf` (simple feature) con geometría tipo MULTILINESTRING que
contiene información de la red vial departamental, incluyendo:

- Geometría MULTILINESTRING (trayectoria de la vía)

- `nombdep`: Nombre del departamento

- `nombprov`: Nombre de la provincia

- `trayectori`: Descripción de la trayectoria de la ruta

- `iddpto`: Código del departamento

- `jerarq`: Jerarquía de la ruta (RD = Red Departamental)

- `estado`: Estado numérico de la vía (1, 2, 3, 4)

- `codruta`: Código de la ruta (ej. PI-102, CU-105)

- `superfic`: Código de tipo de superficie (1-4)

- `longitud`: Longitud del tramo en kilómetros

- `estado_l`: Estado descriptivo (BUENO, REGULAR, MALO)

- `superfic_l`: Tipo de superficie (PAVIMENTADO, AFIRMADO, SIN AFIRMAR,
  TROCHA)

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

**Fuente de los datos:**

- Fuente: PROVÍAS NACIONAL - Ministerio de Transportes y Comunicaciones

- Actualización: Julio 2022

- Base legal: Decreto Supremo 011-2016-MTC y modificatorias

- Nivel: Red Vial Departamental

- Aplicación: Planificación vial y análisis de conectividad territorial

**Clasificación de superficie:**

- 1 = PAVIMENTADO

- 2 = AFIRMADO

- 3 = SIN AFIRMAR

- 4 = TROCHA

**Clasificación de estado:**

- 1 = BUENO

- 2 = REGULAR

- 3 = MALO

- 4 = MUY MALO

**Filtrado jerárquico:** Los filtros se aplican en cascada:

1.  Primero se cargan los departamentos especificados

2.  Luego se filtran las provincias (si se especifican)

El caché se almacena en:
`tempdir()/DEMARCA_cache/red_vial_departamental/`

**NOTA:** Las geometrías son tipo MULTILINESTRING (multilíneas) y
representan las trayectorias de las carreteras departamentales.

## References

PROVÍAS NACIONAL - MTC. Base Cartográfica de la Red Vial Nacional 2022.

Decreto Supremo 011-2016-MTC - Clasificador de Rutas del Sistema
Nacional de Carreteras (SINAC).

Repositorio DEMARCA en OSF: <https://osf.io/qy4j6/>

## See also

[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`get_provincias`](https://paulesantos.github.io/rsdot/reference/get_provincias.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ver departamentos disponibles
get_red_vial_departamental()

# Cargar red vial de un departamento completo
vias_cusco <- get_red_vial_departamental(departamento = "CUSCO")

# Filtrar por provincia específica
vias_cusco_prov <- get_red_vial_departamental(
  departamento = "CUSCO",
  provincia = "CUSCO"
)

# Cargar múltiples departamentos
vias_sur <- get_red_vial_departamental(
  departamento = c("CUSCO", "PUNO", "AREQUIPA")
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Mapa de red vial por tipo de superficie
ggplot(vias_cusco) +
  geom_sf(aes(color = superfic_l), linewidth = 0.8) +
  scale_color_manual(
    values = c(
      "PAVIMENTADO" = "darkblue",
      "AFIRMADO" = "darkgreen",
      "SIN AFIRMAR" = "orange",
      "TROCHA" = "brown"
    ),
    name = "Tipo de Superficie"
  ) +
  labs(
    title = "Red Vial Departamental de Cusco",
    subtitle = "Por Tipo de Superficie - PROVÍAS 2022",
    caption = "Fuente: PROVÍAS NACIONAL | Visor - SDOT"
  ) +
  theme_minimal()

# Mapa por estado de conservación
ggplot(vias_cusco) +
  geom_sf(aes(color = estado_l), linewidth = 1) +
  scale_color_manual(
    values = c(
      "BUENO" = "darkgreen",
      "REGULAR" = "orange",
      "MALO" = "red"
    ),
    name = "Estado"
  ) +
  labs(
    title = "Estado de Conservación de la Red Vial",
    subtitle = "Departamento de Cusco",
    caption = "Fuente: PROVÍAS NACIONAL | Visor - SDOT"
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

# Análisis por estado y superficie
vias_cusco |>
  st_drop_geometry() |>
  group_by(estado_l, superfic_l) |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(longitud_total))

# Filtrar vías pavimentadas en buen estado
vias_buenas <- vias_cusco |>
  filter(superfic_l == "PAVIMENTADO", estado_l == "BUENO")

# Análisis por provincia
vias_cusco |>
  st_drop_geometry() |>
  group_by(nombprov) |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    n_rutas = n_distinct(codruta),
    .groups = "drop"
  ) |>
  arrange(desc(longitud_total))

# Visualizar una provincia específica
vias_prov <- vias_cusco |>
  filter(nombprov == "CUSCO")

ggplot(vias_prov) +
  geom_sf(aes(color = codruta), linewidth = 1.2) +
  labs(
    title = "Red Vial de la Provincia de Cusco",
    subtitle = "Códigos de Ruta",
    caption = "Fuente: PROVÍAS NACIONAL"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

# Estadísticas generales
vias_cusco |>
  st_drop_geometry() |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    n_rutas = n_distinct(codruta),
    n_provincias = n_distinct(nombprov),
    km_pavimentado = sum(longitud[superfic_l == "PAVIMENTADO"], na.rm = TRUE),
    pct_pavimentado = (km_pavimentado / longitud_total) * 100
  )
} # }
```
