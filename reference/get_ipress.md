# Get Health Service Provider Institutions (IPRESS) of Peru

Downloads and loads information on Health Service Provider Institutions
(IPRESS) of Peru, prepared by the National Health Superintendence
(SUSALUD). Data includes POINT type geometry and information on health
establishments, supporting medical services, categorization, operating
hours, and health networks. Updated to 2024. Allows stepwise filtering
by department, province, and district.

## Usage

``` r
get_ipress(
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

An `sf` (simple feature) object with POINT geometry containing IPRESS
information.

## Details

The function downloads data from OSF (Open Science Framework) and stores
it in cache: `tools::R_user_dir("rsdot", which = "cache")/ipress/`.

## References

National Health Superintendence (SUSALUD). National Registry of Health
Service Provider Institutions (RENIPRESS) 2024. DEMARCA OSF Repository:
<https://osf.io/qy4j6/>

## See also

[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`get_provincias`](https://paulesantos.github.io/rsdot/reference/get_provincias.md),
[`get_distritos`](https://paulesantos.github.io/rsdot/reference/get_distritos.md),
[`get_centros_poblados`](https://paulesantos.github.io/rsdot/reference/get_centros_poblados.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# See available departments
get_ipress()

# Load IPRESS for Cusco
ipress_cusco <- get_ipress(departamento = "CUSCO")
} # }
```
