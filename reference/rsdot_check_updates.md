# Check for Updates in Local rsdot Cache

Inspects the local persistent cache directory used by `rsdot`, analyzes
which spatial layers have been downloaded, and compares their status and
file modification dates to assist users in knowing if local data is
current or if newer versions are available.

## Usage

``` r
rsdot_check_updates(show_all = FALSE)
```

## Arguments

- show_all:

  Logical. If `TRUE`, displays all cataloged layers including those not
  yet downloaded. If `FALSE` (default), displays only layers currently
  present in the local cache.

## Value

A `data.frame` with the update and cache status of each layer (returned
invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
# Check status of downloaded datasets in local cache
rsdot_check_updates()

# Check all layers in the catalog
rsdot_check_updates(show_all = TRUE)
} # }
```
