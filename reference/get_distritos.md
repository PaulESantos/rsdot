# Get District Census Limits of Peru

Downloads and loads the official district limits from INEI (2023 or 2025
update) from the DEMARCA OSF repository. Implements persistent caching
to optimize repeated downloads and keeps the console clean during
execution.

## Usage

``` r
get_distritos(
  distrito = NULL,
  provincia = NULL,
  departamento = NULL,
  ano = 2023,
  show_progress = TRUE,
  force_update = FALSE,
  ...
)

get_distritos_2025(...)
```

## Arguments

- distrito:

  Character vector. Name(s) of the district(s) to filter. Can be a
  single district (e.g., "WANCHAQ") or multiple districts (e.g.,
  c("CUSCO", "WANCHAQ", "SANTIAGO")). Case-insensitive. If `NULL`, no
  filtering is performed by district.

- provincia:

  Character vector. Name of the province to filter all its districts
  (e.g., "CUSCO"). If specified, it returns all the districts of the
  indicated province. Case-insensitive.

- departamento:

  Character vector. Name of the department to filter all its districts
  (e.g., "CUSCO"). If specified, it returns all the districts of the
  indicated department. Case-insensitive.

- ano:

  Numeric or character. Year of the district limits: `2023` (default) or
  `2025` (updated boundary layer).

- show_progress:

  Logical. If `TRUE` (default), shows informative messages about
  download progress. If `FALSE`, executes silently.

- force_update:

  Logical. If `TRUE`, forces a new download of the file even if it
  exists in cache. Default `FALSE` to leverage cached data.

- ...:

  Additional arguments passed to internal functions.

## Value

An `sf` (simple feature) object with the geometry of the requested
districts, including their geographic and administrative attributes.

## Details

The function downloads data directly from OSF (Open Science Framework)
and stores it in the user's cache directory
(`tools::R_user_dir("rsdot", "cache")`). The data corresponds to the
official INEI census limits (2023 or 2025 update).

The function automatically handles special character encoding (accents,
ñ) to ensure compatibility across different operating systems.

Filters can be combined hierarchically:

- Department only: returns all districts in the department

- Department + province: returns all districts in that province

- Department + province + district: returns specific districts

- District only: returns that district from anywhere in Peru

Data source: DEMARCA Repository on OSF (<https://osf.io/j9ax8/>)

## References

INEI (2023/2025). District Census Limits of Peru. SDOT DEMARCA PERU
VISOR Repository: <https://osf.io/qy4j6/overview>

## See also

[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html),
[`st_geometry`](https://r-spatial.github.io/sf/reference/st_geometry.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all districts of Peru (2023)
distritos_peru <- get_distritos()

# Load 2025 districts update
distritos_2025 <- get_distritos_2025()

# Load a specific district
wanchaq <- get_distritos(distrito = "Wanchaq")

# Load all districts of a province
distritos_cusco <- get_distritos(provincia = "Cusco")
} # }
```
