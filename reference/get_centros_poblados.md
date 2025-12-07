# Obtener Centros Poblados y Asentamientos Dispersos del Perú

Descarga y carga la información de centros poblados y asentamientos
dispersos recogidos en el Censo de Población y Vivienda 2017 del
Instituto Nacional de Estadística e Informática (INEI). Comprende
viviendas particulares con ocupantes presentes. Los datos incluyen
geometría tipo POINT e información detallada sobre población, vivienda y
ubicación geográfica.

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

  Character vector. Nombre(s) del/los departamento(s) a descargar.
  Opciones válidas: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA",
  "AYACUCHO", "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO",
  "ICA", "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE
  DE DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA",
  "TUMBES", "UCAYALI". No distingue entre mayúsculas y minúsculas. Si es
  `NULL`, muestra la lista de departamentos disponibles.

- provincia:

  Character vector. Nombre(s) de provincia(s) para filtrar los
  resultados (opcional). Se aplica después de cargar los datos.

- distrito:

  Character vector. Nombre(s) de distrito(s) para filtrar los resultados
  (opcional). Se aplica después de cargar los datos.

- show_progress:

  Logical. Si `TRUE` (por defecto), muestra mensajes informativos sobre
  el progreso de la descarga. Si `FALSE`, ejecuta de forma silenciosa.

- force_update:

  Logical. Si `TRUE`, fuerza una nueva descarga del archivo incluso si
  existe en caché. Por defecto `FALSE`.

## Value

Un objeto `sf` (simple feature) con geometría tipo POINT que contiene
información de centros poblados, incluyendo:

- Geometría POINT (ubicación del centro poblado)

- `ubigeo`: Código de ubicación geográfica (6 dígitos)

- `codccpp`: Código del centro poblado (10 dígitos)

- `nombdep`: Nombre del departamento

- `nombprov`: Nombre de la provincia

- `nombdist`: Nombre del distrito

- `cen_pob`: Nombre del centro poblado

- `pob`: Población total

- `viv`: Total de viviendas

- `viv_part`: Viviendas particulares

- `viv_part_o`: Viviendas particulares ocupadas

- `viv_part_1`: Viviendas particulares con 1 o más ocupantes

- `pob_viv_pa`: Población en viviendas particulares

- `y`: Latitud (grados decimales)

- `x`: Longitud (grados decimales)

- `fuente`: Fuente de la información

- `revision`: Información de revisión

- `cap`: Indicador de capital

- `capital`: Tipo de capital si aplica

- `iddpto`: ID del departamento

- `idprov`: ID de la provincia

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

**IMPORTANTE - Ubicación Referencial:** La ubicación de los centros
poblados es referencial. Las coordenadas pueden diferir de las
registradas en otras bases de datos oficiales o levantamientos de campo
específicos. Se recomienda validar las ubicaciones para estudios que
requieran precisión espacial.

El caché se almacena en: `tempdir()/DEMARCA_cache/centros_poblados/`

**NOTA:** Las geometrías son tipo POINT y representan la ubicación
aproximada de los centros poblados según el Censo 2017. El sistema de
coordenadas es WGS 84 (EPSG:4326).

## References

INEI. Censo de Población y Vivienda 2017. Instituto Nacional de
Estadística e Informática.

Visor SDOT: <https://geosdot.servicios.gob.pe/visor/>

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

# Cargar múltiples departamentos
cp_sur <- get_centros_poblados(
  departamento = c("TACNA", "MOQUEGUA")
)

# Filtrar por provincia
cp_cusco_prov <- get_centros_poblados(
  departamento = "CUSCO",
  provincia = "CUSCO"
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Clasificar por tamaño poblacional
cp_tacna <- cp_tacna |>
  mutate(
    categoria = case_when(
      pob >= 5000 ~ "Grande (5000+)",
      pob >= 1000 ~ "Mediano (1000-4999)",
      pob >= 500 ~ "Pequeño (500-999)",
      TRUE ~ "Muy pequeño (<500)"
    )
  )

# Mapa de centros poblados por tamaño
ggplot(cp_tacna) +
  geom_sf(aes(color = categoria, size = pob), alpha = 0.6) +
  scale_size_continuous(range = c(0.5, 5)) +
  scale_color_brewer(palette = "Spectral", direction = -1) +
  labs(
    title = "Centros Poblados de Tacna",
    subtitle = "Clasificados por Tamaño - Censo 2017",
    color = "Categoría",
    size = "Población",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  theme_minimal()

# Análisis de vivienda
cp_tacna |>
  st_drop_geometry() |>
  filter(viv > 0) |>
  summarise(
    centros = n(),
    pob_total = sum(pob, na.rm = TRUE),
    viv_total = sum(viv, na.rm = TRUE),
    pob_por_vivienda = pob_total / viv_total,
    tasa_ocupacion = sum(viv_part_o) / sum(viv_part) * 100
  )

# Distribución por distrito
cp_tacna |>
  st_drop_geometry() |>
  group_by(nombdist) |>
  summarise(
    n_centros = n(),
    poblacion = sum(pob, na.rm = TRUE),
    viviendas = sum(viv, na.rm = TRUE)
  ) |>
  arrange(desc(poblacion))

# Análisis de densidad de vivienda
cp_tacna |>
  st_drop_geometry() |>
  filter(viv > 0) |>
  mutate(pob_por_viv = pob / viv) |>
  summarise(
    min = min(pob_por_viv),
    q1 = quantile(pob_por_viv, 0.25),
    mediana = median(pob_por_viv),
    promedio = mean(pob_por_viv),
    q3 = quantile(pob_por_viv, 0.75),
    max = max(pob_por_viv)
  )
} # }
```
