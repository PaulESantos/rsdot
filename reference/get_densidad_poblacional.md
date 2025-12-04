# Obtener Densidad y Crecimiento Poblacional por Departamento

Descarga y carga la información de densidad y crecimiento poblacional
intercensal por departamento desde el repositorio OSF de DEMARCA. Los
datos provienen de los censos de población 2007 y 2017 del INEI,
procesados mediante interpolación espacial. Permite filtrado escalonado
por departamento, provincia y distrito.

## Usage

``` r
get_densidad_poblacional(
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

  Character vector. Nombre(s) de la(s) provincia(s) para filtrar dentro
  del/los departamento(s) especificado(s). Opcional.

- distrito:

  Character vector. Nombre(s) del/los distrito(s) para filtrar dentro de
  la(s) provincia(s) especificada(s). Opcional.

- show_progress:

  Logical. Si `TRUE` (por defecto), muestra mensajes informativos sobre
  el progreso de la descarga. Si `FALSE`, ejecuta de forma silenciosa.

- force_update:

  Logical. Si `TRUE`, fuerza una nueva descarga del archivo incluso si
  existe en caché. Por defecto `FALSE`.

## Value

Un objeto `sf` (simple feature) con información de densidad y
crecimiento poblacional, incluyendo:

- Geometría de los polígonos interpolados (con geometrías validadas)

- `ubigeo`: Código UBIGEO del distrito

- `nombdep`: Nombre del departamento

- `nombprov`: Nombre de la provincia

- `nombdist`: Nombre del distrito

- `nivel`: Nivel de crecimiento poblacional

- `rango`: Rango de tasa de crecimiento

- `descrip`: Descripción de la categoría

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

**Metodología de los datos:**

- Fuente: Censos de Población INEI 2007 y 2017

- Cobertura: Centros poblados con población \> 150 habitantes

- Técnica: Interpolación espacial para representación continua

- Variable: Tasa de crecimiento poblacional intercensal anual

**Filtrado jerárquico:** Los filtros se aplican en cascada:

1.  Primero se cargan los departamentos especificados

2.  Luego se filtran las provincias (si se especifican)

3.  Finalmente se filtran los distritos (si se especifican)

El caché se almacena en: `tempdir()/DEMARCA_cache/densidad_poblacional/`

**NOTA:** Las geometrías se validan automáticamente con
[`st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
para prevenir errores en operaciones espaciales posteriores.

## References

INEI. Censos Nacionales de Población y Vivienda 2007 y 2017.

Repositorio DEMARCA en OSF: <https://osf.io/qy4j6/>

## See also

[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`get_provincias`](https://paulesantos.github.io/rsdot/reference/get_provincias.md),
[`get_distritos`](https://paulesantos.github.io/rsdot/reference/get_distritos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html),
[`st_make_valid`](https://r-spatial.github.io/sf/reference/valid.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ver departamentos disponibles
get_densidad_poblacional()

# Cargar un departamento completo
densidad_cusco <- get_densidad_poblacional(departamento = "CUSCO")

# Cargar una provincia específica
densidad_prov_cusco <- get_densidad_poblacional(
  departamento = "CUSCO",
  provincia = "CUSCO"
)

# Cargar un distrito específico
densidad_wanchaq <- get_densidad_poblacional(
  departamento = "CUSCO",
  provincia = "CUSCO",
  distrito = "WANCHAQ"
)

# Cargar múltiples departamentos
densidad_sur <- get_densidad_poblacional(
  departamento = c("CUSCO", "PUNO", "AREQUIPA")
)

# Filtrar múltiples provincias
densidad_valle <- get_densidad_poblacional(
  departamento = "CUSCO",
  provincia = c("CUSCO", "URUBAMBA", "CALCA")
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Mapa de nivel de crecimiento
ggplot(densidad_cusco) +
  geom_sf(aes(fill = nivel), color = NA) +
  scale_fill_brewer(
    palette = "RdYlGn",
    name = "Nivel de\nCrecimiento"
  ) +
  labs(
    title = "Crecimiento Poblacional por Nivel",
    subtitle = "Departamento de Cusco (2007-2017)",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  theme_minimal()

# Capa agregada por distrito (un polígono por distrito)
distritos_anta <- prov_anta |>
 group_by(ubigeo, nombdist) |>
 summarise(.groups = "drop")
distritos_anta

# Puntos para poner las etiquetas dentro de cada distrito
distritos_centroides <- st_point_on_surface(distritos_anta)
distritos_centroides

ggplot() +
 # 1. Celdas / polígonos con el nivel de crecimiento
 geom_sf(
   data = prov_anta,
   aes(fill = nivel),
   color = NA
 ) +
 # 2. Contorno de distritos (para “hacerlos notar”)
 geom_sf(
   data = distritos_anta,
   fill  = NA,
   color = "grey20",
   linewidth = 0.4
 ) +
 # 3. Nombre de los distritos
 geom_sf_text(
   data = distritos_centroides,
   aes(label = nombdist),
   size = 3
 ) +
 scale_fill_brewer(
   palette = "RdYlGn",
   name    = "Nivel de\ncrecimiento"
 ) +
 labs(
   title    = "Crecimiento poblacional",
   subtitle = "Provincia de Anta (2007–2017)",
   caption  = "Fuente: INEI | Visor SDOT"
 ) +
 theme_minimal() +
 theme(
   legend.position = "right",
   panel.grid.major = element_line(linewidth = 0.2, colour = "grey90")
 )
} # }
```
