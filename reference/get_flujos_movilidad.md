# Get Territorial Mobility and Commuting Flows of Peru

Downloads and loads spatial flow lines of inter-district commuting and
mobility patterns for students and workers in Peru, modeled by SDOT-PCM.
Available across multiple flow volume brackets (e.g. 100-1000,
1000-3500, etc.).

## Usage

``` r
get_flujos_movilidad(
  tipo = NULL,
  volumen = NULL,
  departamento = NULL,
  show_progress = TRUE,
  force_update = FALSE,
  ...
)

get_flujos_estudiantes(volumen = "100_1000", ...)

get_flujos_trabajadores(volumen = "100_1500", ...)
```

## Arguments

- tipo:

  Character. Type of mobility flow:

  - `"estudiantes"` (or `"est"`): Student commuting flows.

  - `"trabajadores"` (or `"trb"`): Worker commuting flows.

  Case-insensitive. If `NULL`, lists available types and volume
  intervals.

- volumen:

  Character or numeric. Volume interval bracket:

  - For students: `"100_1000"` (default), `"1000_3500"`, `"3500_7000"`,
    `"7000_13000"`, `"13000"`.

  - For workers: `"100_1500"` (default), `"1500_5000"`, `"5000_10000"`,
    `"10000_25000"`, `"25000"`.

- departamento:

  Character vector. Name(s) of department(s) to filter. Optional.

- show_progress:

  Logical. If `TRUE` (default), shows informative messages.

- force_update:

  Logical. If `TRUE`, forces a new download of files. Default `FALSE`.

- ...:

  Additional arguments passed to internal filtering functions.

## Value

An `sf` (simple feature) object with LINESTRING geometries representing
mobility vectors.

## Details

Cache is stored under
`tools::R_user_dir("rsdot", which = "cache")/flujos_movilidad/`.

## See also

[`get_red_vial_nacional`](https://paulesantos.github.io/rsdot/reference/get_red_vial_nacional.md),
[`get_tiempos_desplazamiento`](https://paulesantos.github.io/rsdot/reference/get_tiempos_desplazamiento.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# List available options
get_flujos_movilidad()

# Load student flows between 100 and 1,000 trips
flujos_est <- get_flujos_estudiantes(volumen = "100_1000")

# Load worker flows
flujos_trb <- get_flujos_trabajadores(volumen = "100_1500")
} # }
```
