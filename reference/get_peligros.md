# Get Natural Hazards Spatial Data of Peru

Downloads and loads spatial data of natural hazards (floods, mass
movements, etc.) prepared by SDOT-PCM. Data includes polygons, lines, or
points representing areas of exposure, critical points, and tramos.

## Usage

``` r
get_peligros(
  tipo = NULL,
  departamento = NULL,
  provincia = NULL,
  distrito = NULL,
  show_progress = TRUE,
  force_update = FALSE,
  ...
)

get_puntos_criticos(...)

get_tramos_criticos(...)

get_zonas_criticas(...)

get_areas_inundacion(...)

get_areas_movimientos_masa(...)

get_inventario_inundaciones(...)
```

## Arguments

- tipo:

  Character. The type of hazard layer to retrieve. Valid options:

  - `"areas_exposicion_inundacion"` (or alias `"exp_inundacion"`)

  - `"areas_exposicion_movimientos_masa"` (or alias `"exp_mov_masa"`)

  - `"inventario_inundaciones"`

  - `"puntos_criticos_inundaciones"` (or alias `"puntos_criticos"`)

  - `"tramos_criticos_inundaciones"` (or alias `"tramos_criticos"`)

  - `"zonas_criticas_movimientos_masa"` (or alias `"zonas_criticas"`)

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

An `sf` (simple feature) object representing the requested hazard layer.

## Details

Cache is stored under subdirectories of
`tools::R_user_dir("rsdot", which = "cache")/`.

## Examples

``` r
if (FALSE) { # \dontrun{
# List available types
get_peligros()

# Load critical flood points for Cusco
pc_cusco <- get_peligros(tipo = "puntos_criticos", departamento = "CUSCO")
} # }
```
