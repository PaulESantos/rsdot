# List Available Layers in GeoSDOT and rsdot

Retrieves the complete catalog of spatial layers supported by the
`rsdot` package, detailing the layer key, thematic domain, source layer
in GeoServer, geographic partitioning level, and the recommended R
access function.

## Usage

``` r
list_available_layers(domain = NULL, show_details = FALSE)
```

## Arguments

- domain:

  Character. Optional filter by thematic domain: `"limits"`,
  `"transport"`, `"services"`, `"demography"`, `"telecom"`, `"hazards"`,
  `"mobility"`, `"cadastre"`, or `NULL` (default, returns all).

- show_details:

  Logical. If `TRUE`, returns additional technical metadata such as
  GeoServer layer names and partition columns. Default is `FALSE`.

## Value

A `data.frame` containing the catalog of layers.

## Examples

``` r
if (FALSE) { # \dontrun{
# List all supported layers
list_available_layers()

# Filter only transportation layers
list_available_layers(domain = "transport")
} # }
```
