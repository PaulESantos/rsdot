# Obtener Límites Censales Departamentales del Perú

Descarga y carga los límites departamentales oficiales del INEI (2023)
desde el repositorio OSF de DEMARCA. Implementa caché de sesión para
optimizar descargas repetidas y mantiene la consola limpia durante la
ejecución.

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

  Character vector. Nombre(s) del/los departamento(s) a filtrar. Puede
  ser un único departamento (ej: "CUSCO") o múltiples departamentos (ej:
  c("CUSCO", "PUNO")). No distingue entre mayúsculas y minúsculas. Si es
  `NULL` (por defecto), retorna todos los departamentos del Perú.

- show_progress:

  Logical. Si `TRUE` (por defecto), muestra mensajes informativos sobre
  el progreso de la descarga. Si `FALSE`, ejecuta de forma silenciosa.

- force_update:

  Logical. Si `TRUE`, fuerza una nueva descarga del archivo incluso si
  existe en caché. Por defecto `FALSE` para aprovechar datos en caché.

## Value

Un objeto `sf` (simple feature) con la geometría de los departamentos
solicitados, incluyendo sus atributos geográficos y administrativos.

## Details

La función descarga los datos directamente desde OSF (Open Science
Framework) y los almacena en un directorio temporal de caché durante la
sesión de R. Los datos corresponden a los límites censales oficiales del
INEI 2023.

El caché se almacena en: `tempdir()/DEMARCA_cache/`

Fuente de datos: Repositorio DEMARCA en OSF (<https://osf.io/grhe3/>)

## References

INEI (2023). Límites Censales Departamentales del Perú. Repositorio
VISOR SDOT DEMARCA PERU: <https://osf.io/qy4j6/overview>

## See also

[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html),
[`st_geometry`](https://r-spatial.github.io/sf/reference/st_geometry.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Cargar todos los departamentos del Perú
peru <- get_departamentos()

# Cargar un departamento específico (no distingue mayúsculas/minúsculas)
cusco <- get_departamentos(departamento = "Cusco")

# Cargar múltiples departamentos (región sur)
sur <- get_departamentos(
  departamento = c("PUNO", "TACNA", "MOQUEGUA", "AREQUIPA")
)

# Forzar actualización de datos
peru_actualizado <- get_departamentos(force_update = TRUE)

# Ejecución silenciosa
lima <- get_departamentos(departamento = "LIMA", show_progress = FALSE)

# Visualización con ggplot2
library(ggplot2)

ggplot(peru) +
  geom_sf(fill = "#ff9999", color = "#e60000", linewidth = 1) +
  labs(
    title = "Límites Censales Departamentales del Perú",
    subtitle = "Límites Censales INEI 2023",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  theme_minimal()

ggplot(cusco) +
  geom_sf(fill = "lightgreen", color = "darkgreen", linewidth = 1) +
  labs(
    title = "Departamento de Cusco",
    subtitle = "Límites Censales INEI 2023",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  theme_minimal()

ggplot(sur) +
  geom_sf(aes(fill = nombdep), color = "white", linewidth = 0.5) +
  labs(
    title = "Macro Región Sur del Perú",
    fill = "Departamento",
    caption = "Fuente: INEI | Visor - SDOT"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(legend.position = "bottom")
} # }
```
