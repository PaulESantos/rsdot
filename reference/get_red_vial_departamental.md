# Get Departmental Road Network of Peru

Downloads and loads information on the Departmental Road Network updated
by PROVIAS NACIONAL as of July 2022, according to the Route Classifier
approved by Supreme Decree 011-2016-MTC and its amendments. Data
includes MULTILINESTRING type geometry and information on trajectories,
status, and surface of departmental roads. Allows stepwise filtering by
department and province.

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

An `sf` (simple feature) object with MULTILINESTRING geometry containing
departmental road network information.

## Details

The function downloads data from OSF (Open Science Framework) and stores
it in cache:
`tools::R_user_dir("rsdot", which = "cache")/red_vial_departamental/`.

## References

PROVIAS NACIONAL - MTC. Base Cartográfica de la Red Vial Departamental
2022. DEMARCA OSF Repository: <https://osf.io/qy4j6/>

## See also

[`get_red_vial_nacional`](https://paulesantos.github.io/rsdot/reference/get_red_vial_nacional.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# See available departments
get_red_vial_departamental()

# Load road network for Cusco
vias_cusco <- get_red_vial_departamental(departamento = "CUSCO")
} # }
```
