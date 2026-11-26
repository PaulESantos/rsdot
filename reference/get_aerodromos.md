# Get Airfields of Peru

Downloads and loads information on airfields in Peru updated to the year
2022, prepared by the Ministry of Transport and Communications (MTC).
Data includes POINT type geometry and information on airports,
airfields, and heliports. Allows stepwise filtering by department,
province, and district.

## Usage

``` r
get_aerodromos(
  departamento = NULL,
  provincia = NULL,
  distrito = NULL,
  show_progress = TRUE,
  force_update = FALSE
)
```

## Arguments

- departamento:

  Character vector. Name(s) of the department(s) to download. Valid
  options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
  "CAJAMARCA", "CUSCO", "HUANUCO", "ICA", "JUNIN", "LA LIBERTAD",
  "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA", "PASCO",
  "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
  Case-insensitive. If `NULL`, shows the list of available departments.

- provincia:

  Character vector. Name(s) of the province(s) to filter within the
  specified department(s). Optional.

- distrito:

  Character vector. Name(s) of the district(s) to filter within the
  specified province(s). Optional.

- show_progress:

  Logical. If `TRUE` (default), shows informative messages about
  download progress. If `FALSE`, executes silently.

- force_update:

  Logical. If `TRUE`, forces a new download of the file even if it
  exists in cache. Default `FALSE`.

## Value

An `sf` (simple feature) object with POINT type geometry containing
airfield information, including:

- POINT geometry (airfield coordinates)

- `nombre`: Airfield name

- `label`: Full descriptive label

- `tipo`: Type of facility (INTERNATIONAL AIRPORT, AIRFIELD, HELIPORT)

- `codidep`: Department code

- `nombdep`: Department name

- `nombprov`: Province name

- `nombdist`: District name

- `escala`: Airfield scale

- `lat`: Latitude

- `lon`: Longitude

- `estado`: Operating status (OPERATIONAL, etc.)

- `administ`: Managing entity

- `jerarquia`: Hierarchy (NATIONAL, etc.)

- `titular`: Ownership type (PUBLIC, PRIVATE)

If multiple departments are requested, it returns a combined sf object.

## Details

The function downloads data from OSF (Open Science Framework) and stores
it in cache during the R session. Data is in GeoPackage (.gpkg) format.

Cache is stored in:
`tools::R_user_dir("rsdot", which = "cache")/aerodromos/`

**NOTE:** Geometries are POINT type and represent the location of each
airfield.

## References

Ministry of Transport and Communications (MTC). Airfields of Peru 2022.
DEMARCA OSF Repository: <https://osf.io/qy4j6/>

## See also

[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`get_provincias`](https://paulesantos.github.io/rsdot/reference/get_provincias.md),
[`get_distritos`](https://paulesantos.github.io/rsdot/reference/get_distritos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# See available departments
get_aerodromos()

# Load airfields of a full department
aero_cusco <- get_aerodromos(departamento = "CUSCO")

# Filter by specific province
aero_prov_cusco <- get_aerodromos(
  departamento = "CUSCO",
  provincia = "CUSCO"
)
} # }
```
