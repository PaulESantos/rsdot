# Obtener Capitales Distritales del Perú

Descarga y carga la información de capitales distritales identificadas
en el Censo de Población y Vivienda 2017 del Instituto Nacional de
Estadística e Informática (INEI). Los datos incluyen información
demográfica y geográfica de las capitales a nivel departamental,
provincial y distrital, con geometría tipo POINT.

## Usage

``` r
get_capitales(
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

  Character vector. Nombre(s) de provincia(s) para filtrar los
  resultados (opcional). Se aplica después de cargar los datos.

- distrito:

  Character vector. Nombre(s) de distrito(s) para filtrar los resultados
  (opcional). Se aplica después de cargar los datos.

- show_progress:

  Logical. Si `TRUE` (por defecto), muestra mensajes informativos sobre
  el progreso de la descarga. Si `FALSE`, ejecuta de forma silenciosa.

- force_update:

  Logical. Si `TRUE`, fuerza una nueva descarga del archivo incluso si
  existe en caché. Por defecto `FALSE`.

## Value

Un objeto `sf` (simple feature) con geometría tipo POINT que contiene
información de capitales distritales, incluyendo:

- Geometría POINT (ubicación de la capital)

- `gid`: Identificador único del registro

- `ubigeo`: Código de ubicación geográfica (6 dígitos)

- `nombdep`: Nombre del departamento

- `nombprov`: Nombre de la provincia

- `nombdist`: Nombre del distrito

- `codccpp`: Código del centro poblado (10 dígitos)

- `cen_pob`: Nombre del centro poblado

- `pob`: Población total

- `capital`: Tipo de capital (1=departamental, 2=provincial,
  3=distrital)

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

El caché se almacena en: `tempdir()/DEMARCA_cache/capitales/`

**NOTA:** Las geometrías son tipo POINT y representan la ubicación
geográfica aproximada de las capitales según el Censo 2017. Los datos de
población corresponden al año censal.

## References

INEI. Censo de Población y Vivienda 2017. Instituto Nacional de
Estadística e Informática.

Visor SDOT: <https://geosdot.servicios.gob.pe/visor/>

## See also

[`get_centros_poblados`](https://paulesantos.github.io/rsdot/reference/get_centros_poblados.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ver departamentos disponibles
get_capitales()

# Cargar capitales de un departamento
capitales_cusco <- get_capitales(departamento = "CUSCO")

# Cargar múltiples departamentos
capitales_sur <- get_capitales(
  departamento = c("CUSCO", "PUNO", "AREQUIPA")
)

# Filtrar por provincia
capitales_lima_prov <- get_capitales(
  departamento = "LIMA",
  provincia = "LIMA"
)

# Filtrar por distrito
capital_cusco_dist <- get_capitales(
  departamento = "CUSCO",
  provincia = "CUSCO",
  distrito = "CUSCO"
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Mapa de capitales por jerarquía
ggplot(capitales_cusco) +
  geom_sf(aes(color = factor(capital), size = pob), alpha = 0.7) +
  scale_color_manual(
    values = c("1" = "red", "2" = "blue", "3" = "green"),
    labels = c("Departamental", "Provincial", "Distrital"),
    name = "Tipo de Capital"
  ) +
  scale_size_continuous(range = c(1, 10), name = "Población") +
  labs(
    title = "Capitales de Cusco por Jerarquía",
    subtitle = "Censo de Población y Vivienda 2017",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  theme_minimal()

# Análisis de población por tipo de capital
capitales_cusco |>
  st_drop_geometry() |>
  group_by(capital) |>
  summarise(
    n = n(),
    pob_total = sum(pob, na.rm = TRUE),
    pob_promedio = mean(pob, na.rm = TRUE),
    pob_mediana = median(pob, na.rm = TRUE)
  ) |>
  arrange(capital)

# Capitales más pobladas
capitales_cusco |>
  st_drop_geometry() |>
  arrange(desc(pob)) |>
  select(nombdist, cen_pob, pob, capital) |>
  head(10)

# Filtrar solo capitales provinciales
caps_prov <- capitales_cusco |>
  filter(capital == 2)

# Análisis por provincia
capitales_cusco |>
  st_drop_geometry() |>
  group_by(nombprov) |>
  summarise(
    n_capitales = n(),
    pob_total = sum(pob, na.rm = TRUE)
  ) |>
  arrange(desc(pob_total))
} # }
```
