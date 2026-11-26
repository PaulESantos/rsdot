# Get Departmental Census Limits of Peru

Downloads and loads the official departmental limits from INEI (2023)
from the DEMARCA OSF repository. Implements persistent caching to
optimize repeated downloads and keeps the console clean during
execution.

## Usage

``` r
get_departamentos(
  departamento = NULL,
  show_progress = TRUE,
  force_update = FALSE
)
```

## Arguments

- departamento:

  Character vector. Name(s) of the department(s) to filter. Can be a
  single department (e.g., "CUSCO") or multiple departments (e.g.,
  c("CUSCO", "PUNO")). Case-insensitive. If `NULL` (default), returns
  all departments of Peru.

- show_progress:

  Logical. If `TRUE` (default), shows informative messages about
  download progress. If `FALSE`, executes silently.

- force_update:

  Logical. If `TRUE`, forces a new download of the file even if it
  exists in cache. Default `FALSE` to leverage cached data.

## Value

An `sf` (simple feature) object with the geometry of the requested
departments, including their geographic and administrative attributes.

## Details

The function downloads data directly from OSF (Open Science Framework)
and stores it in the user's cache directory
(`tools::R_user_dir("rsdot", "cache")`). The data corresponds to the
official INEI 2023 census limits.

Data source: DEMARCA Repository on OSF (<https://osf.io/grhe3/>)

## References

INEI (2023). Departmental Census Limits of Peru. SDOT DEMARCA PERU VISOR
Repository: <https://osf.io/qy4j6/overview>

## See also

[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html),
[`st_geometry`](https://r-spatial.github.io/sf/reference/st_geometry.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all departments of Peru
peru <- get_departamentos()

# Load a specific department (case-insensitive)
cusco <- get_departamentos(departamento = "Cusco")

# Load multiple departments (southern region)
sur <- get_departamentos(
  departamento = c("PUNO", "TACNA", "MOQUEGUA", "AREQUIPA")
)

# Force data update
peru_updated <- get_departamentos(force_update = TRUE)

# Silent execution
lima <- get_departamentos(departamento = "LIMA", show_progress = FALSE)

# Visualization with ggplot2
library(ggplot2)

ggplot(peru) +
  geom_sf(fill = "#ff9999", color = "#e60000", linewidth = 1) +
  labs(
    title = "Departmental Census Limits of Peru",
    subtitle = "INEI 2023 Census Limits",
    caption = "Source: INEI | Visor - SDOT"
  ) +
  theme_minimal()
} # }
```
