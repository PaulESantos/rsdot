# Get Disaster Risk Spatial Data of Peru

Downloads and loads spatial data of disaster risk assessments and
non-mitigable risk zones prepared by SDOT-PCM and CENEPRED.

## Usage

``` r
get_riesgos(
  tipo = NULL,
  departamento = NULL,
  provincia = NULL,
  distrito = NULL,
  show_progress = TRUE,
  force_update = FALSE,
  ...
)

get_evaluacion_riesgos(...)

get_zonas_riesgos_no_mitigable(...)
```

## Arguments

- tipo:

  Character. The type of risk layer to retrieve. Valid options:

  - `"evaluacion_riesgos"` (or alias `"evaluacion"`)

  - `"zonas_riesgos_no_mitigable"` (or alias `"no_mitigable"`)

  Case-insensitive. If `NULL`, lists all available types.

- departamento:

  Character vector. Name(s) of the department(s) to download.
  Case-insensitive. If `NULL`, lists available departments for the
  selected type.

- provincia:

  Character vector. Name(s) of the province(s) to filter. Optional.

- distrito:

  Character vector. Name(s) of the district(s) to filter. Optional.

- show_progress:

  Logical. If `TRUE` (default), shows informative messages.

- force_update:

  Logical. If `TRUE`, forces a new download of files. Default `FALSE`.

- ...:

  Additional arguments passed to internal functions or wrapper methods.

## Value

An `sf` (simple feature) object representing the requested risk layer.

## Details

Cache is stored under subdirectories of
`tools::R_user_dir("rsdot", which = "cache")/`.

## Examples

``` r
if (FALSE) { # \dontrun{
# List available types
get_riesgos()

# Load non-mitigable risk zones for Cusco
r_cusco <- get_riesgos(tipo = "no_mitigable", departamento = "CUSCO")
} # }
```
