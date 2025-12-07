# Obtener Aeródromos del Perú

Descarga y carga la información de aeródromos del Perú actualizado al
año 2022, elaborado por el Ministerio de Transportes y Comunicaciones.
Los datos incluyen geometría tipo POINT e información sobre aeropuertos,
aeródromos y helipuertos. Permite filtrado escalonado por departamento,
provincia y distrito.

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

  Character vector. Nombre(s) del/los departamento(s) a descargar.
  Opciones válidas: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA",
  "AYACUCHO", "CAJAMARCA", "CUSCO", "HUANUCO", "ICA", "JUNIN", "LA
  LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS",
  "MOQUEGUA", "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES",
  "UCAYALI". No distingue entre mayúsculas y minúsculas. Si es `NULL`,
  muestra la lista de departamentos disponibles.

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

Un objeto `sf` (simple feature) con geometría tipo POINT que contiene
información de los aeródromos, incluyendo:

- Geometría POINT (coordenadas del aeródromo)

- `nombre`: Nombre del aeródromo

- `label`: Etiqueta descriptiva completa

- `tipo`: Tipo de instalación (AEROPUERTO INTERNACIONAL, AERODROMO,
  HELIPUERTO)

- `codidep`: Código del departamento

- `nombdep`: Nombre del departamento

- `nombprov`: Nombre de la provincia

- `nombdist`: Nombre del distrito

- `escala`: Escala del aeródromo

- `lat`: Latitud

- `lon`: Longitud

- `estado`: Estado operativo (OPERATIVO, etc.)

- `administ`: Entidad administradora

- `jerarquia`: Jerarquía (NACIONAL, etc.)

- `titular`: Tipo de titularidad (PUBLICA, PRIVADA)

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

**Fuente de los datos:**

- Fuente: Ministerio de Transportes y Comunicaciones (MTC)

- Año: 2022

- Nivel: Aeródromo/Aeropuerto/Helipuerto

- Aplicación: Planificación territorial y análisis de conectividad

**Filtrado jerárquico:** Los filtros se aplican en cascada:

1.  Primero se cargan los departamentos especificados

2.  Luego se filtran las provincias (si se especifican)

3.  Finalmente se filtran los distritos (si se especifican)

El caché se almacena en: `tempdir()/DEMARCA_cache/aerodromos/`

**NOTA:** Las geometrías son tipo POINT (puntos) y representan la
ubicación de cada aeródromo.

## References

Ministerio de Transportes y Comunicaciones (MTC). Aeródromos del Perú
2022.

Repositorio DEMARCA en OSF: <https://osf.io/qy4j6/>

## See also

[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`get_provincias`](https://paulesantos.github.io/rsdot/reference/get_provincias.md),
[`get_distritos`](https://paulesantos.github.io/rsdot/reference/get_distritos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ver departamentos disponibles
get_aerodromos()

# Cargar aeródromos de un departamento completo
aero_cusco <- get_aerodromos(departamento = "CUSCO")

# Filtrar por provincia específica
aero_prov_cusco <- get_aerodromos(
  departamento = "CUSCO",
  provincia = "CUSCO"
)

# Filtrar por distrito específico
aero_san_sebastian <- get_aerodromos(
  departamento = "CUSCO",
  provincia = "CUSCO",
  distrito = "SAN SEBASTIAN"
)

# Cargar múltiples departamentos
aero_sur <- get_aerodromos(
  departamento = c("CUSCO", "PUNO", "AREQUIPA")
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Mapa de aeródromos por tipo
ggplot(aero_cusco) +
  geom_sf(aes(color = tipo, shape = tipo), size = 3) +
  scale_color_manual(
    values = c(
      "AEROPUERTO INTERNACIONAL" = "darkred",
      "AERODROMO" = "darkblue",
      "HELIPUERTO" = "darkgreen"
    ),
    name = "Tipo"
  ) +
  labs(
    title = "Aeródromos del Departamento de Cusco",
    subtitle = "Actualizado 2022 - MTC",
    caption = "Fuente: MTC | Visor - SDOT"
  ) +
  theme_minimal()

# Aeródromos por tipo de titularidad
aero_cusco |>
  group_by(tipo, titular) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(desc(n))

# Filtrar aeródromos operativos públicos
aero_publicos <- aero_cusco |>
  filter(titular == "PUBLICA", estado == "OPERATIVO")

# Visualizar aeródromos por administrador
aero_cusco |>
  filter(!is.na(administ)) |>
  group_by(administ) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(desc(n))

# Mapa de aeródromos con etiquetas
ggplot(aero_cusco) +
  geom_sf(aes(color = tipo), size = 4) +
  geom_sf_text(
    aes(label = nombre),
    size = 2.5,
    nudge_y = 0.1,
    check_overlap = TRUE
  ) +
  scale_color_brewer(palette = "Set1", name = "Tipo") +
  labs(
    title = "Aeródromos de Cusco",
    subtitle = "MTC 2022",
    caption = "Fuente: MTC | Visor - SDOT"
  ) +
  theme_minimal()
} # }
```
