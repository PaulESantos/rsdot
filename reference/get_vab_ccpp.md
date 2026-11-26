# Get Gross Added Value (VAB) of Population Centers

Downloads and loads the socioeconomic model of Gross Added Value (VAB -
Valor Agregado Bruto) estimated at a 150-meter buffer around population
centers (CCPP) in Peru, prepared by SDOT-PCM. Available at national
coverage or focused metropolitan domains (Lima, Callao).

## Usage

``` r
get_vab_ccpp(
  cobertura = "total",
  departamento = NULL,
  provincia = NULL,
  distrito = NULL,
  show_progress = TRUE,
  force_update = FALSE
)
```

## Arguments

- cobertura:

  Character. Geographic scope to retrieve:

  - `"total"` (or `"nacional"`): Complete national model.

  - `"lima"`: Metropolitan Lima model.

  - `"callao"`: Callao region model.

  Default is `"total"`. Case-insensitive.

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

An `sf` (simple feature) object containing population centers with
estimated VAB attributes.

## Details

Cache is stored under
`tools::R_user_dir("rsdot", which = "cache")/vab_ccpp/`.

## See also

[`get_centros_poblados`](https://paulesantos.github.io/rsdot/reference/get_centros_poblados.md),
[`get_densidad_actividades`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load national VAB model
vab_nac <- get_vab_ccpp()

# Load VAB model for Lima
vab_lima <- get_vab_ccpp(cobertura = "lima")
} # }
```
