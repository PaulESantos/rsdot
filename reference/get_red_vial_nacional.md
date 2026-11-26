# Get National Road Network of Peru

Downloads and loads information on the National Road Network (Red Vial
Nacional - RVN) of Peru updated to 2022, prepared by PROVIAS NACIONAL
and the Ministry of Transport and Communications (MTC). Data includes
MULTILINESTRING geometry and detailed attributes for each road section.

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

  Character vector. Name(s) of the department(s) to download. Valid
  options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
  "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA",
  "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE
  DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA",
  "TUMBES", "UCAYALI". Case-insensitive. If `NULL`, shows the list of
  available departments.

- show_progress:

  Logical. If `TRUE` (default), shows informative messages about
  download progress. If `FALSE`, executes silently.

- force_update:

  Logical. If `TRUE`, forces a new download of the file even if it
  exists in cache. Default `FALSE`.

## Value

An `sf` (simple feature) object with MULTILINESTRING geometry containing
national road network information.

## Details

The function downloads data from OSF (Open Science Framework) and stores
it in cache:
`tools::R_user_dir("rsdot", which = "cache")/red_vial_nacional/`.

## References

PROVIAS NACIONAL - MTC. Base Cartográfica de la Red Vial Nacional 2022.
DEMARCA OSF Repository: <https://osf.io/qy4j6/>

## See also

[`get_red_vial_departamental`](https://paulesantos.github.io/rsdot/reference/get_red_vial_departamental.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# See available departments
get_red_vial_nacional()

# Load road network for Cusco
vias_cusco <- get_red_vial_nacional(departamento = "CUSCO")
} # }
```
