# Obtener Centros Poblados con Tasa de Crecimiento Poblacional

Descarga y carga la información de centros poblados con su tasa media de
crecimiento anual poblacional intercensal (2007-2017) desde el
repositorio OSF de DEMARCA. Los datos incluyen geometría tipo POINT y
permiten identificar los cambios de volumen poblacional de cada centro
poblado. Permite filtrado escalonado por departamento, provincia y
distrito.

## Usage

``` r
get_centros_poblados_crecimiento(
  departamento = NULL,
  provincia = NULL,
  distrito = NULL,
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

- distrito:

  Character vector. Nombre(s) del/los distrito(s) para filtrar dentro de
  la(s) provincia(s) especificada(s). Opcional.

- show_progress:

  Logical. Si `TRUE` (por defecto), muestra mensajes informativos sobre
  el progreso de la descarga. Si `FALSE`, ejecuta de forma silenciosa.

- force_update:

  Logical. Si `TRUE`, fuerza una nueva descarga del archivo incluso si
  existe en caché. Por defecto `FALSE`.

## Value

Un objeto `sf` (simple feature) con geometría tipo POINT que contiene
información de los centros poblados y su tasa de crecimiento,
incluyendo:

- Geometría POINT (coordenadas del centro poblado)

- `nro`: Número de identificación

- `codccpp`: Código de centro poblado

- `ubigeo`: Código UBIGEO del distrito

- `dep`: Nombre del departamento

- `prov`: Nombre de la provincia

- `distrito`: Nombre del distrito

- `centro_pob`: Nombre del centro poblado

- `pob_2007`: Población en el censo 2007

- `pob_2017`: Población en el censo 2017

- `tasa`: Tasa media de crecimiento anual (%)

- `capital`: Tipo de capital (Departamental, Provincial, Distrital, No
  es capital)

- `region`: Región natural (Costa, Sierra, Selva)

- `urb_rural`: Clasificación urbano/rural

- `tc_catg`: Categoría de tasa de crecimiento (POSITIVO, NEGATIVO)

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

**Metodología de los datos:**

- Fuente: Censos de Población INEI 2007 y 2017

- Nivel: Centro poblado

- Variable: Tasa media de crecimiento anual intercensal

- Aplicación: Análisis territorial para creaciones distritales

**Filtrado jerárquico:** Los filtros se aplican en cascada:

1.  Primero se cargan los departamentos especificados

2.  Luego se filtran las provincias (si se especifican)

3.  Finalmente se filtran los distritos (si se especifican)

El caché se almacena en: `tempdir()/DEMARCA_cache/centros_poblados/`

**NOTA:** Las geometrías son tipo POINT (puntos) y representan la
ubicación de cada centro poblado.

## References

INEI. Censos Nacionales de Población y Vivienda 2007 y 2017.

Repositorio DEMARCA en OSF: <https://osf.io/qy4j6/>

## See also

[`get_densidad_poblacional`](https://paulesantos.github.io/rsdot/reference/get_densidad_poblacional.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`get_provincias`](https://paulesantos.github.io/rsdot/reference/get_provincias.md),
[`get_distritos`](https://paulesantos.github.io/rsdot/reference/get_distritos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ver departamentos disponibles
get_centros_poblados_crecimiento()

# Cargar centros poblados de un departamento completo
ccpp_cusco <- get_centros_poblados_crecimiento(departamento = "CUSCO")

# Filtrar por provincia específica
ccpp_prov_cusco <- get_centros_poblados_crecimiento(
  departamento = "CUSCO",
  provincia = "CUSCO"
)

# Filtrar por distrito específico
ccpp_wanchaq <- get_centros_poblados_crecimiento(
  departamento = "CUSCO",
  provincia = "CUSCO",
  distrito = "WANCHAQ"
)

# Cargar múltiples departamentos
ccpp_sur <- get_centros_poblados_crecimiento(
  departamento = c("CUSCO", "PUNO", "AREQUIPA")
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Mapa de centros poblados por categoría de crecimiento
ggplot(ccpp_cusco) +
  geom_sf(aes(color = tc_catg, size = pob_2017), alpha = 0.6) +
  scale_color_manual(
    values = c("POSITIVO" = "darkgreen", "NEGATIVO" = "darkred"),
    name = "Crecimiento"
  ) +
  scale_size_continuous(
    name = "Población 2017",
    range = c(0.5, 5)
  ) +
  labs(
    title = "Centros Poblados del Departamento de Cusco",
    subtitle = "Tasa de Crecimiento Poblacional 2007-2017",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  theme_minimal()

# Filtrar centros poblados por características
ccpp_urbanos <- ccpp_cusco |>
  filter(urb_rural == "URBANA")

ccpp_capitales <- ccpp_cusco |>
  filter(capital != "No es capital")

# Centros poblados con mayor crecimiento
top_crecimiento <- ccpp_cusco |>
  arrange(desc(tasa)) |>
  head(10)

ggplot(top_crecimiento) +
  geom_sf(aes(size = tasa, color = tasa)) +
  scale_color_gradient(low = "yellow", high = "red", name = "Tasa (%)") +
  scale_size_continuous(range = c(3, 10), name = "Tasa (%)") +
  geom_sf_text(
    aes(label = centro_pob),
    size = 2.5,
    nudge_y = 0.05
  ) +
  labs(
    title = "Top 10 Centros Poblados con Mayor Crecimiento",
    subtitle = "Departamento de Cusco (2007-2017)",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  theme_minimal()

# Análisis por región natural
ccpp_cusco |>
  group_by(region, tc_catg) |>
  summarise(
    n_centros = n(),
    pob_total_2017 = sum(pob_2017, na.rm = TRUE),
    tasa_promedio = mean(tasa, na.rm = TRUE),
    .groups = "drop"
  )

# Comparación urbano vs rural
ggplot(ccpp_cusco) +
  geom_boxplot(aes(x = urb_rural, y = tasa, fill = urb_rural)) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribución de Tasas de Crecimiento",
    subtitle = "Por área urbana y rural - Cusco",
    x = "Clasificación",
    y = "Tasa de Crecimiento (%)",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
} # }
```
