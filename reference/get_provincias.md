# Obtener Límites Censales Provinciales del Perú

Descarga y carga los límites provinciales oficiales del INEI (2023)
desde el repositorio OSF de DEMARCA. Implementa caché de sesión para
optimizar descargas repetidas y mantiene la consola limpia durante la
ejecución.

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

  Character vector. Nombre(s) de la(s) provincia(s) a filtrar. Puede ser
  una única provincia (ej: "URUBAMBA") o múltiples provincias (ej:
  c("CAÑETE", "CHINCHA")). No distingue entre mayúsculas y minúsculas.
  Si es `NULL`, no filtra por provincia.

- departamento:

  Character vector. Nombre del departamento para filtrar todas sus
  provincias (ej: "CUSCO"). Si se especifica, retorna todas las
  provincias del departamento indicado. No distingue entre mayúsculas y
  minúsculas.

- show_progress:

  Logical. Si `TRUE` (por defecto), muestra mensajes informativos sobre
  el progreso de la descarga. Si `FALSE`, ejecuta de forma silenciosa.

- force_update:

  Logical. Si `TRUE`, fuerza una nueva descarga del archivo incluso si
  existe en caché. Por defecto `FALSE` para aprovechar datos en caché.

## Value

Un objeto `sf` (simple feature) con la geometría de las provincias
solicitadas, incluyendo sus atributos geográficos y administrativos.

## Details

La función descarga los datos directamente desde OSF (Open Science
Framework) y los almacena en un directorio temporal de caché durante la
sesión de R. Los datos corresponden a los límites censales oficiales del
INEI 2023.

El caché se almacena en: `tempdir()/DEMARCA_cache/`

La función corrige automáticamente problemas de codificación de
caracteres (especialmente Ñ y tildes) en los datos originales para
asegurar compatibilidad entre diferentes sistemas operativos.

Fuente de datos: Repositorio DEMARCA en OSF (<https://osf.io/t36aj/>)

## References

INEI (2023). Límites Censales Provinciales del Perú. Repositorio
DEMARCA: <https://osf.io/t36aj/>

## See also

[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html),
[`st_geometry`](https://r-spatial.github.io/sf/reference/st_geometry.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Cargar todas las provincias del Perú
provincias_peru <- get_provincias()

# Cargar una provincia específica
urubamba <- get_provincias(provincia = "Urubamba")

# Cargar todas las provincias de un departamento
provincias_cusco <- get_provincias(departamento = "Cusco")

# Cargar múltiples provincias específicas
sur_chico <- get_provincias(
  provincia = c("CAÑETE", "CHINCHA", "PISCO")
)

# Combinar filtros: provincias específicas de un departamento
costa_lima <- get_provincias(
  provincia = c("CAÑETE", "YAUYOS"),
  departamento = "LIMA"
)

# Forzar actualización de datos
provincias_actualizadas <- get_provincias(force_update = TRUE)

# Ejecución silenciosa
pasco <- get_provincias(departamento = "PASCO", show_progress = FALSE)

# Visualización con ggplot2
library(ggplot2)

# Mapa de todas las provincias de Cusco
ggplot(provincias_cusco) +
  geom_sf(aes(fill = nombprov), color = "white", linewidth = 0.3) +
  labs(
    title = "Provincias del Departamento de Cusco",
    subtitle = "Límites Censales INEI 2023",
    fill = "Provincia",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal() +
  theme(legend.position = "right")

# Mapa de una provincia específica
ggplot(urubamba) +
  geom_sf(fill = "lightblue", color = "darkblue", linewidth = 1) +
  labs(
    title = "Provincia de Urubamba",
    subtitle = "Departamento de Cusco - INEI 2023",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  theme_minimal()

# Mapa de provincias de la costa sur
ggplot(sur_chico) +
  geom_sf(aes(fill = nombprov), color = "white", linewidth = 0.5) +
  labs(
    title = "Provincias de la Costa Sur",
    fill = "Provincia",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  scale_fill_manual(values = c("CHINCHA" = "#FF6B6B",
                                 "PISCO" = "#4ECDC4",
                                 "CAÑETE" = "#45B7D1")) +
  theme_minimal() +
  theme(legend.position = "bottom")
} # }
```
