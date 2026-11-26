# Get Rural Cadastre Spatial Data of Peru

Downloads and loads spatial cadastral parcels and property polygons from
the official Rural Cadastre (Catastro Rural) of the Ministry of Agrarian
Development and Irrigation (MIDAGRI), integrated into the SDOT platform.

## Usage

``` r
get_catastro_rural(
  departamento = NULL,
  provincia = NULL,
  distrito = NULL,
  show_progress = TRUE,
  force_update = FALSE
)
```

## Arguments

- departamento:

  Character vector. Name(s) of department(s) to filter. Optional.

- provincia:

  Character vector. Name(s) of province(s) to filter. Optional.

- distrito:

  Character vector. Name(s) of district(s) to filter. Optional.

- show_progress:

  Logical. If `TRUE` (default), shows informative messages.

- force_update:

  Logical. If `TRUE`, forces a new download of files. Default `FALSE`.

## Value

An `sf` (simple feature) object containing cadastral parcels.

## Details

Cache is stored under
`tools::R_user_dir("rsdot", which = "cache")/catastro_rural/`.

## References

MIDAGRI. Sistema Catastral Rural de Predios Individuales y Comunales.
DEMARCA OSF Repository: <https://osf.io/qy4j6/>

## See also

[`get_distritos`](https://paulesantos.github.io/rsdot/reference/get_distritos.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load rural cadastre for a department
catastro_cusco <- get_catastro_rural(departamento = "CUSCO")
} # }
```
