# Get Mobile Coverage by Population Center

Downloads and loads information on mobile coverage by population center
from the DEMARCA OSF repository. Data includes binary information
(YES/NO) on the existence of 2G, 3G, 4G, and 5G coverage for each
population center according to the SDOT Viewer.

## Usage

``` r
get_cobertura_movil_c(
  departamento = NULL,
  provincia = NULL,
  distrito = NULL,
  show_progress = TRUE,
  force_update = FALSE
)

get_cobertura_movil(
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
  "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA",
  "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE
  DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA",
  "TUMBES", "UCAYALI".

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

An `sf` (simple feature) object with POINT geometry containing mobile
coverage information.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché:
`tools::R_user_dir("rsdot", which = "cache")/cobertura_movil/`. Los
datos están en formato GeoPackage (.gpkg).

## References

Ministerio de Transportes y Comunicaciones (MTC) / OSIPTEL. DEMARCA OSF
Repository: <https://osf.io/qy4j6/>

## See also

[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`get_provincias`](https://paulesantos.github.io/rsdot/reference/get_provincias.md),
[`get_distritos`](https://paulesantos.github.io/rsdot/reference/get_distritos.md),
[`get_centros_poblados`](https://paulesantos.github.io/rsdot/reference/get_centros_poblados.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ver departamentos disponibles
get_cobertura_movil()

# Cargar cobertura móvil de Cusco
cob_cusco <- get_cobertura_movil(departamento = "CUSCO")
} # }
```
