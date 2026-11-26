# Get Educational Services of Peru

Downloads and loads information on educational services in Peru,
prepared by the Ministry of Education (MINEDU). Data includes POINT type
geometry and detailed programmatic, level, and enrollment attributes for
each service. Updated to 2024. Allows stepwise filtering by department
and province.

## Usage

``` r
get_servicios_educativos(
  departamento = NULL,
  provincia = NULL,
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

- show_progress:

  Logical. If `TRUE` (default), shows informative messages about
  download progress. If `FALSE`, executes silently.

- force_update:

  Logical. If `TRUE`, forces a new download of the file even if it
  exists in cache. Default `FALSE`.

## Value

An `sf` (simple feature) object with POINT geometry containing
educational services information.

## Details

The function downloads data from OSF (Open Science Framework) and stores
it in cache:
`tools::R_user_dir("rsdot", which = "cache")/servicios_educativos/`.

## References

MINEDU - ESCALE. Padrón de Instituciones Educativas 2024. DEMARCA OSF
Repository: <https://osf.io/qy4j6/>

## See also

[`get_locales_educativos`](https://paulesantos.github.io/rsdot/reference/get_locales_educativos.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# See available departments
get_servicios_educativos()

# Load educational services for Cusco
serv_cusco <- get_servicios_educativos(departamento = "CUSCO")
} # }
```
