# Get Population Centers of Peru

Downloads and loads information on population centers of Peru from the
2017 Population and Housing Census of the National Institute of
Statistics and Informatics (INEI). Data includes demographic and
geographic information at the departmental, provincial, and district
levels, with POINT type geometry.

## Usage

``` r
get_centros_poblados(
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
  "TUMBES", "UCAYALI". Case-insensitive. If `NULL`, shows the list of
  available departments.

- provincia:

  Character vector. Name(s) of province(s) to filter the results
  (optional). It is applied after loading the data.

- distrito:

  Character vector. Name(s) of district(s) to filter the results
  (optional). It is applied after loading the data.

- show_progress:

  Logical. If `TRUE` (default), shows informative messages about
  download progress. If `FALSE`, executes silently.

- force_update:

  Logical. If `TRUE`, forces a new download of the file even if it
  exists in cache. Default `FALSE`.

## Value

An `sf` (simple feature) object with POINT type geometry containing
population center information.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

Cache is stored in:
`tools::R_user_dir("rsdot", which = "cache")/centros_poblados/`

## References

INEI. Censo de Población y Vivienda 2017. Instituto Nacional de
Estadística e Informática. Visor SDOT:
<https://geosdot.servicios.gob.pe/visor/>

## See also

[`get_capitales`](https://paulesantos.github.io/rsdot/reference/get_capitales.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ver departamentos disponibles
get_centros_poblados()

# Cargar centros poblados de un departamento
cp_tacna <- get_centros_poblados(departamento = "TACNA")
} # }
```
