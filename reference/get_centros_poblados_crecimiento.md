# Get Population Centers with Population Growth Rate

Downloads and loads information on population centers with their average
annual intercensal population growth rate (2007-2017) from the DEMARCA
OSF repository. Data includes POINT type geometry and allows identifying
changes in population volume for each population center. Allows stepwise
filtering by department, province, and district.

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

An `sf` (simple feature) object with POINT type geometry containing
information on population centers and their growth rate.

## Details

The function downloads data from OSF (Open Science Framework) and stores
it in cache:
`tools::R_user_dir("rsdot", which = "cache")/densidad_crecimiento_pob_ccpp/`.

## References

INEI. National Population and Housing Censuses 2007 and 2017. DEMARCA
OSF Repository: <https://osf.io/qy4j6/>

## See also

[`get_centros_poblados`](https://paulesantos.github.io/rsdot/reference/get_centros_poblados.md),
[`get_densidad_poblacional`](https://paulesantos.github.io/rsdot/reference/get_densidad_poblacional.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# See available departments
get_centros_poblados_crecimiento()

# Load population growth rate for Cusco
ccpp_cusco <- get_centros_poblados_crecimiento(departamento = "CUSCO")
} # }
```
