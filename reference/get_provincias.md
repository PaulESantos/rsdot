# Get Provincial Census Limits of Peru

Downloads and loads the official provincial limits from INEI (2023) from
the DEMARCA OSF repository. Implements persistent caching to optimize
repeated downloads and keeps the console clean during execution.

## Usage

``` r
get_provincias(
  provincia = NULL,
  departamento = NULL,
  show_progress = TRUE,
  force_update = FALSE
)
```

## Arguments

- provincia:

  Character vector. Name(s) of the province(s) to filter. Can be a
  single province (e.g., "URUBAMBA") or multiple provinces (e.g.,
  c("CAÑETE", "CHINCHA")). Case-insensitive. If `NULL`, no filtering is
  performed by province.

- departamento:

  Character vector. Name of the department to filter all its provinces
  (e.g., "CUSCO"). If specified, it returns all the provinces of the
  indicated department. Case-insensitive.

- show_progress:

  Logical. If `TRUE` (default), shows informative messages about
  download progress. If `FALSE`, executes silently.

- force_update:

  Logical. If `TRUE`, forces a new download of the file even if it
  exists in cache. Default `FALSE` to leverage cached data.

## Value

An `sf` (simple feature) object with the geometry of the requested
provinces, including their geographic and administrative attributes.

## Details

The function downloads data directly from OSF (Open Science Framework)
and stores it in the user's cache directory
(`tools::R_user_dir("rsdot", "cache")`). The data corresponds to the
official INEI 2023 census limits.

The function automatically corrects character encoding issues
(especially Ñ and accents) in the original data.

Data source: DEMARCA Repository on OSF (<https://osf.io/t36aj/>)

## References

INEI (2023). Provincial Census Limits of Peru. DEMARCA Repository:
<https://osf.io/t36aj/>

## See also

[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html),
[`st_geometry`](https://r-spatial.github.io/sf/reference/st_geometry.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all provinces of Peru
provincias_peru <- get_provincias()

# Load a specific province
urubamba <- get_provincias(provincia = "Urubamba")

# Load all provinces of a department
provincias_cusco <- get_provincias(departamento = "Cusco")

# Load multiple specific provinces
sur_chico <- get_provincias(
  provincia = c("CAÑETE", "CHINCHA", "PISCO")
)

# Combine filters: specific provinces of a department
costa_lima <- get_provincias(
  provincia = c("CAÑETE", "YAUYOS"),
  departamento = "LIMA"
)

# Force data update
provincias_updated <- get_provincias(force_update = TRUE)

# Silent execution
pasco <- get_provincias(departamento = "PASCO", show_progress = FALSE)

# Visualization with ggplot2
library(ggplot2)

# Map of all provinces in Cusco
ggplot(provincias_cusco) +
  geom_sf(aes(fill = nombprov), color = "white", linewidth = 0.3) +
  labs(
    title = "Provinces of the Department of Cusco",
    subtitle = "INEI 2023 Census Limits",
    fill = "Province",
    caption = "Source: INEI | Visor - SDOT"
  ) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  theme(legend.position = "right")
} # }
```
