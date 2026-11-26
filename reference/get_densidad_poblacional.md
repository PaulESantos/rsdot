# Get Intercensal Population Density and Growth of Peru

Downloads and loads information on intercensal population density and
growth (2007-2017) of Peru, prepared by INEI and distributed through the
SDOT Viewer. Data includes MULTIPOLYGON geometry at the block or census
area level with indicators of density, population change, and growth
categories. Allows stepwise filtering by department, province, and
district.

## Usage

``` r
get_densidad_poblacional(
  departamento = NULL,
  provincia = NULL,
  distrito = NULL,
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
  "TUMBES", "UCAYALI". Case-insensitive. If `NULL`, shows the list of
  available departments.

- provincia:

  Character vector. Name(s) of the province(s) to filter within the
  specified department(s). Optional.

- distrito:

  Character vector. Name(s) of the district(s) to filter within the
  specified province(s). Optional.

- show_progress:

  Logical. If `TRUE` (default), shows informative messages about
  download progress. If `FALSE`, executes silently.

- force_update:

  Logical. If `TRUE`, forces a new download of the file even if it
  exists in cache. Default `FALSE`.

## Value

An `sf` (simple feature) object with MULTIPOLYGON geometry containing
population density and growth attributes.

## Details

The function downloads data from OSF (Open Science Framework) and stores
it in cache:
`tools::R_user_dir("rsdot", which = "cache")/densidad_poblacional/`.

## References

INEI. National Population and Housing Censuses 2007 and 2017. DEMARCA
OSF Repository: <https://osf.io/qy4j6/>

## See also

[`get_centros_poblados_crecimiento`](https://paulesantos.github.io/rsdot/reference/get_centros_poblados_crecimiento.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# See available departments
get_densidad_poblacional()

# Load population density for Cusco
dens_cusco <- get_densidad_poblacional(departamento = "CUSCO")
} # }
```
