# Obtener Red Vial Vecinal del Perú

Descarga y carga la información de la Red Vial Vecinal actualizada por
PROVÍAS DESCENTRALIZADO a julio 2022, según Clasificador de Rutas
aprobado mediante Decreto Supremo 011-2016-MTC y sus modificatorias. Los
datos incluyen geometría tipo MULTILINESTRING e información sobre
trayectorias, estado y superficie de las carreteras vecinales o rurales.
Permite filtrado escalonado por departamento y provincia.

## Usage

``` r
get_red_vial_vecinal(
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
  "AYACUCHO", "CAJAMARCA", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA",
  "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE
  DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA",
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
contiene información de la red vial vecinal, incluyendo:

- Geometría MULTILINESTRING (trayectoria de la vía)

- `nombdep`: Nombre del departamento

- `nombprov`: Nombre de la provincia

- `iddpto`: Código del departamento

- `jerarq`: Jerarquía de la ruta (RV = Red Vecinal)

- `trayectori`: Descripción de la trayectoria de la ruta

- `estado`: Estado numérico de la vía (1, 2, 3)

- `codruta`: Código de la ruta vecinal (ej. LO-517, CU-123)

- `superfic`: Código de tipo de superficie (1-4)

- `longitud`: Longitud del tramo en kilómetros

- `estado_l`: Estado descriptivo (Bueno, Regular, Malo)

- `superfic_l`: Tipo de superficie (Pavimentado, Afirmado, Sin afirmar,
  Trocha)

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

**Fuente de los datos:**

- Fuente: PROVÍAS DESCENTRALIZADO - Ministerio de Transportes y
  Comunicaciones

- Actualización: Julio 2022

- Base legal: Decreto Supremo 011-2016-MTC y modificatorias

- Nivel: Red Vial Vecinal o Rural (Sistema Nacional de Carreteras -
  SINAC)

- Aplicación: Planificación vial local y análisis de conectividad rural

**Clasificación de superficie:**

- 1 = Pavimentado

- 2 = Afirmado

- 3 = Sin afirmar

- 4 = Trocha

**Clasificación de estado:**

- 1 = Bueno

- 2 = Regular

- 3 = Malo

**Características de la Red Vecinal:** Las carreteras vecinales o
rurales conectan centros poblados menores, caseríos y áreas de
producción con la red vial departamental y nacional. Son de competencia
de los gobiernos locales (municipalidades).

**Filtrado jerárquico:** Los filtros se aplican en cascada:

1.  Primero se cargan los departamentos especificados

2.  Luego se filtran las provincias (si se especifican)

El caché se almacena en: `tempdir()/DEMARCA_cache/red_vial_vecinal/`

**NOTA:** Las geometrías son tipo MULTILINESTRING (multilíneas) y
representan las trayectorias de las carreteras vecinales.

## References

PROVÍAS DESCENTRALIZADO - MTC. Base Cartográfica de la Red Vial Vecinal
2022.

Decreto Supremo 011-2016-MTC - Clasificador de Rutas del Sistema
Nacional de Carreteras (SINAC).

Repositorio DEMARCA en OSF: <https://osf.io/qy4j6/>

## See also

[`get_red_vial_departamental`](https://paulesantos.github.io/rsdot/reference/get_red_vial_departamental.md),
[`get_red_vial_nacional`](https://paulesantos.github.io/rsdot/reference/get_red_vial_nacional.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`get_provincias`](https://paulesantos.github.io/rsdot/reference/get_provincias.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ver departamentos disponibles
get_red_vial_vecinal()

# Cargar red vial vecinal de un departamento completo
vias_cusco <- get_red_vial_vecinal(departamento = "CUSCO")

# Filtrar por provincia específica
vias_cusco_prov <- get_red_vial_vecinal(
  departamento = "CUSCO",
  provincia = "CUSCO"
)

# Cargar múltiples departamentos
vias_sur <- get_red_vial_vecinal(
  departamento = c("CUSCO", "PUNO", "AREQUIPA")
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Mapa de red vial vecinal por tipo de superficie
ggplot(vias_cusco) +
  geom_sf(aes(color = superfic_l), linewidth = 0.6) +
  scale_color_manual(
    values = c(
      "Pavimentado" = "darkblue",
      "Afirmado" = "darkgreen",
      "Sin afirmar" = "orange",
      "Trocha" = "brown"
    ),
    name = "Tipo de Superficie"
  ) +
  labs(
    title = "Red Vial Vecinal de Cusco",
    subtitle = "Por Tipo de Superficie - PROVÍAS 2022",
    caption = "Fuente: PROVÍAS DESCENTRALIZADO | Visor - SDOT"
  ) +
  theme_minimal()

# Mapa por estado de conservación
ggplot(vias_cusco) +
  geom_sf(aes(color = estado_l), linewidth = 0.8) +
  scale_color_manual(
    values = c(
      "Bueno" = "darkgreen",
      "Regular" = "orange",
      "Malo" = "red"
    ),
    name = "Estado"
  ) +
  labs(
    title = "Estado de Conservación de la Red Vial Vecinal",
    subtitle = "Departamento de Cusco",
    caption = "Fuente: PROVÍAS DESCENTRALIZADO | Visor - SDOT"
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

# Filtrar vías en mal estado que necesitan intervención
vias_mal_estado <- vias_cusco |>
  filter(estado_l == "Malo")

# Análisis por provincia
vias_cusco |>
  st_drop_geometry() |>
  group_by(nombprov) |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    n_rutas = n_distinct(codruta),
    km_trocha = sum(longitud[superfic_l == "Trocha"], na.rm = TRUE),
    pct_trocha = (km_trocha / longitud_total) * 100,
    .groups = "drop"
  ) |>
  arrange(desc(longitud_total))

# Gráfico de barras: km por provincia
vias_prov_summary <- vias_cusco |>
  st_drop_geometry() |>
  group_by(nombprov) |>
  summarise(total_km = sum(longitud, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(total_km)) |>
  head(10)

ggplot(vias_prov_summary, aes(x = reorder(nombprov, total_km), y = total_km)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Provincias con Mayor Red Vial Vecinal",
    subtitle = "Departamento de Cusco",
    x = "Provincia",
    y = "Longitud Total (km)",
    caption = "Fuente: PROVÍAS DESCENTRALIZADO"
  ) +
  theme_minimal()

# Análisis de superficie por provincia
vias_cusco |>
  st_drop_geometry() |>
  group_by(nombprov, superfic_l) |>
  summarise(km = sum(longitud, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = superfic_l, values_from = km, values_fill = 0) |>
  arrange(desc(Trocha))

# Filtrar solo vías pavimentadas o afirmadas
vias_mejoradas <- vias_cusco |>
  filter(superfic_l %in% c("Pavimentado", "Afirmado"))

# Estadísticas generales
vias_cusco |>
  st_drop_geometry() |>
  summarise(
    longitud_total = sum(longitud, na.rm = TRUE),
    n_rutas = n_distinct(codruta),
    n_provincias = n_distinct(nombprov),
    km_trocha = sum(longitud[superfic_l == "Trocha"], na.rm = TRUE),
    pct_trocha = (km_trocha / longitud_total) * 100,
    km_afirmado = sum(longitud[superfic_l == "Afirmado"], na.rm = TRUE),
    pct_afirmado = (km_afirmado / longitud_total) * 100,
    km_mal_estado = sum(longitud[estado_l == "Malo"], na.rm = TRUE),
    pct_mal_estado = (km_mal_estado / longitud_total) * 100
  )

# Comparación entre provincias
vias_cusco |>
  st_drop_geometry() |>
  group_by(nombprov) |>
  summarise(
    total_km = sum(longitud, na.rm = TRUE),
    pct_bueno = sum(longitud[estado_l == "Bueno"], na.rm = TRUE) / total_km * 100,
    pct_regular = sum(longitud[estado_l == "Regular"], na.rm = TRUE) / total_km * 100,
    pct_malo = sum(longitud[estado_l == "Malo"], na.rm = TRUE) / total_km * 100,
    .groups = "drop"
  ) |>
  arrange(desc(total_km))

# Visualizar una provincia específica
vias_prov <- vias_cusco |>
  filter(nombprov == "CUSCO")

ggplot(vias_prov) +
  geom_sf(aes(color = estado_l), linewidth = 1) +
  scale_color_manual(
    values = c("Bueno" = "green", "Regular" = "orange", "Malo" = "red")
  ) +
  labs(
    title = "Red Vial Vecinal - Provincia de Cusco",
    subtitle = "Estado de Conservación",
    color = "Estado"
  ) +
  theme_minimal()
} # }
```
