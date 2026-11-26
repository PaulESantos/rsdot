# Get Economic Activity and Housing Density Models of Peru

Downloads and loads spatial density models of commercial, market,
financial, real estate, and housing activities developed for territorial
modeling by SDOT-PCM. Data includes polygon/grid geometry with
standardized density indicators.

## Usage

``` r
get_densidad_actividades(
  tipo = NULL,
  departamento = NULL,
  provincia = NULL,
  distrito = NULL,
  show_progress = TRUE,
  force_update = FALSE,
  ...
)

get_densidad_comercio(...)

get_densidad_mercados(...)

get_densidad_financiero(...)

get_densidad_inmobiliario(...)

get_densidad_viviendas(...)
```

## Arguments

- tipo:

  Character. Activity density model to retrieve. Valid options:

  - `"comercio"`: Density of retail and wholesale commerce.

  - `"mercado"` (or `"mercados"`): Density of municipal/neighborhood
    supply markets.

  - `"financiero"`: Density of banking and financial services.

  - `"inmobiliario"`: Density of real estate activity and developments.

  - `"viviendas"`: Density of residential housing units.

  Case-insensitive. If `NULL`, lists available options.

- departamento:

  Character vector. Name(s) of the department(s) to filter. Optional.

- provincia:

  Character vector. Name(s) of the province(s) to filter. Optional.

- distrito:

  Character vector. Name(s) of the district(s) to filter. Optional.

- show_progress:

  Logical. If `TRUE` (default), shows informative messages.

- force_update:

  Logical. If `TRUE`, forces a new download of files. Default `FALSE`.

- ...:

  Additional arguments passed to internal filtering functions.

## Value

An `sf` (simple feature) object containing the requested density model.

## Details

Cache is stored under
`tools::R_user_dir("rsdot", which = "cache")/densidad_actividades/`.

## See also

[`get_densidad_poblacional`](https://paulesantos.github.io/rsdot/reference/get_densidad_poblacional.md),
[`get_distritos`](https://paulesantos.github.io/rsdot/reference/get_distritos.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# List available types
get_densidad_actividades()

# Load commerce density for Cusco
dens_com <- get_densidad_comercio(departamento = "CUSCO")

# Load financial services density
dens_fin <- get_densidad_financiero(departamento = "LIMA")
} # }
```
