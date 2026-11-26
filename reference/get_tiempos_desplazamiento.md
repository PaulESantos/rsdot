# Get Travel Times to the Nearest Capital

Downloads and loads information on travel times to the nearest
departmental capital from the DEMARCA OSF repository. The data
represents accessibility layers from the SDOT-PCM Spatial Data Viewer.
Supports spatial clipping via an area of interest, or filtering by
department, province, and district.

## Usage

``` r
get_tiempos_desplazamiento(
  departamento = NULL,
  provincia = NULL,
  distrito = NULL,
  area_interes = NULL,
  show_progress = TRUE,
  force_update = FALSE
)
```

## Arguments

- departamento:

  Character vector. Name(s) of the department(s) to download. Valid
  options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
  "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA",
  "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE
  DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA",
  "TUMBES", "UCAYALI". Case-insensitive. If `NULL` and `area_interes` is
  also `NULL`, shows the list of available departments.

- provincia:

  Character vector. Name(s) of the province(s) to filter. Optional.
  Since the raw dataset lacks administrative columns, filtering is
  performed spatially using boundary geometries.

- distrito:

  Character vector. Name(s) of the district(s) to filter. Optional.
  Performed spatially using boundary geometries.

- area_interes:

  An `sf` or `sfc` object. Bounding boxes are used to dynamically
  identify and download the corresponding departments, and the resulting
  layer is cropped to the exact geometry of the area of interest.

- show_progress:

  Logical. If `TRUE` (default), shows informative messages about
  download progress. If `FALSE`, executes silently.

- force_update:

  Logical. If `TRUE`, forces a new download of the files even if they
  exist in cache. Default `FALSE`.

## Value

An `sf` (simple feature) object with MULTIPOLYGON geometry containing
accessibility attributes:

- `dn`: Numeric classification of time values.

- `cat_tiempo`: Time interval category (e.g. "\<= 30 min", etc.).

- `fid`: Feature identifier.

- `capa`: Source layer name.

## Details

The function downloads data from OSF (Open Science Framework) and stores
it in persistent cache:
`tools::R_user_dir("rsdot", which = "cache")/tiempo_desplazamiento/`.

## See also

[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`get_provincias`](https://paulesantos.github.io/rsdot/reference/get_provincias.md),
[`get_distritos`](https://paulesantos.github.io/rsdot/reference/get_distritos.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# See available departments
get_tiempos_desplazamiento()

# Load times for a specific department
tiempos_cusco <- get_tiempos_desplazamiento(departamento = "CUSCO")
} # }
```
