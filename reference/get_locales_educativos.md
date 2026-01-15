# Obtener Locales Educativos del Perú

Descarga y carga la información de locales educativos del Perú,
elaborado por el Ministerio de Educación (MINEDU). Un local educativo es
un inmueble en el cual funciona uno o más establecimientos educativos
(servicios educativos). Los datos incluyen geometría tipo POINT e
información sobre los servicios educativos que operan en cada local,
ubicación y área censal. Permite filtrado escalonado por departamento,
provincia y distrito.

## Usage

``` r
get_locales_educativos(
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

Un objeto `sf` (simple feature) con geometría tipo POINT que contiene
información de los locales educativos, incluyendo:

**Identificación del local:**

- Geometría POINT (coordenadas del local educativo)

- `codlocal`: Código único del local educativo asignado por MINEDU

- `servicios`: Lista de servicios educativos (instituciones educativas)
  que funcionan en el local, incluyendo nombre, nivel y modalidad. Un
  mismo local puede albergar múltiples servicios educativos (ej:
  inicial, primaria y secundaria)

- `dir_cen`: Dirección del local educativo

**Ubicación geográfica:**

- `localidad`: Nombre de la localidad donde se ubica el local

- `codcp_inei`: Código de centro poblado según INEI

- `codccpp`: Código de centro poblado (alternativo)

- `centro_pob`: Nombre del centro poblado

- `departamen`: Nombre del departamento

- `provincia`: Nombre de la provincia

- `distrito`: Nombre del distrito

**Características del local:**

- `area_censo`: Área censal donde se ubica el local:

  - URBANA: Zona urbana

  - RURAL: Zona rural

**Coordenadas:**

- `nlat_ie`: Latitud del local educativo (coordenada Y)

- `nlong_ie`: Longitud del local educativo (coordenada X)

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

**Fuente de los datos:**

- Fuente: Ministerio de Educación (MINEDU)

- Registro: Padrón de Instituciones Educativas

- Nivel: Local educativo

- Aplicación: Análisis de cobertura educativa, planificación de
  infraestructura escolar y estudios de accesibilidad

**Conceptos clave:**

**Local educativo vs Institución educativa:**

- **Local educativo**: Infraestructura física (inmueble) donde funcionan
  los servicios educativos. Un local puede albergar múltiples
  instituciones educativas.

- **Servicio/Institución educativa**: Unidad operativa del sistema
  educativo que brinda un tipo específico de educación (inicial,
  primaria, secundaria, CETPRO, etc.)

**Niveles y modalidades educativas en Perú:**

- **Educación Básica Regular (EBR)**:

  - Inicial (0-5 años): Cuna, Jardín

  - Primaria (6-11 años)

  - Secundaria (12-16 años)

- **Educación Básica Alternativa (EBA)**: Para personas que no
  accedieron oportunamente a EBR

- **Educación Básica Especial (EBE)**: Para personas con discapacidad o
  talento superdotado

- **Educación Técnico-Productiva (CETPRO)**: Formación técnica y
  capacitación laboral

- **Educación Superior**: Institutos y escuelas de educación superior
  pedagógica, tecnológica y artística

**Interpretación del campo servicios:**

El campo `servicios` contiene una cadena de texto con todos los
servicios educativos que operan en el local, separados por comas. Cada
servicio incluye el nombre de la institución y su nivel/modalidad entre
paréntesis.

Ejemplo: "501330 (PRIMARIA), 501330 (SECUNDARIA)" indica que en ese
local funcionan los niveles de primaria y secundaria de la IE 501330.

**Filtrado jerárquico:** Los filtros se aplican en cascada:

1.  Primero se cargan los departamentos especificados

2.  Luego se filtran las provincias (si se especifican)

3.  Finalmente se filtran los distritos (si se especifican)

El caché se almacena en: `tempdir()/DEMARCA_cache/locales_educativos/`

**NOTA:** Las geometrías son tipo POINT (puntos) y representan la
ubicación de cada local educativo.

## References

Ministerio de Educación del Perú (MINEDU). Padrón de Instituciones
Educativas y Programas Educativos.

ESCALE - Estadística de la Calidad Educativa:
<https://escale.minedu.gob.pe/>

Repositorio DEMARCA en OSF: <https://osf.io/qy4j6/>

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
get_locales_educativos()

# Cargar locales educativos de un departamento completo
locales_cusco <- get_locales_educativos(departamento = "CUSCO")

# Filtrar por provincia específica
locales_prov_cusco <- get_locales_educativos(
  departamento = "CUSCO",
  provincia = "CUSCO"
)

# Filtrar por distrito específico
locales_wanchaq <- get_locales_educativos(
  departamento = "CUSCO",
  provincia = "CUSCO",
  distrito = "WANCHAQ"
)

# Cargar múltiples departamentos
locales_sur <- get_locales_educativos(
  departamento = c("CUSCO", "PUNO", "AREQUIPA")
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Mapa de locales educativos por área censal
ggplot(locales_cusco) +
  geom_sf(aes(color = area_censo), size = 1, alpha = 0.6) +
  scale_color_manual(
    values = c("URBANA" = "darkblue", "RURAL" = "darkgreen"),
    name = "Área"
  ) +
  labs(
    title = "Locales Educativos del Departamento de Cusco",
    subtitle = "Ministerio de Educación",
    caption = "Fuente: MINEDU - Padrón de Instituciones Educativas"
  ) +
  theme_minimal()

# Resumen por área censal
locales_cusco |>
  sf::st_drop_geometry() |>
  count(area_censo)

# Distribución por distrito
locales_cusco |>
  sf::st_drop_geometry() |>
  count(distrito, sort = TRUE) |>
  head(10)

# Filtrar locales con nivel inicial
locales_inicial <- locales_cusco |>
  filter(grepl("INICIAL", servicios, ignore.case = TRUE))

# Filtrar locales con secundaria
locales_secundaria <- locales_cusco |>
  filter(grepl("SECUNDARIA", servicios, ignore.case = TRUE))

# Locales con múltiples niveles (inicial + primaria + secundaria)
locales_completos <- locales_cusco |>
  filter(
    grepl("INICIAL", servicios) &
    grepl("PRIMARIA", servicios) &
    grepl("SECUNDARIA", servicios)
  )

# Análisis de cobertura por provincia
cobertura_provincia <- locales_cusco |>
  sf::st_drop_geometry() |>
  group_by(provincia) |>
  summarise(
    total_locales = n(),
    locales_urbanos = sum(area_censo == "URBANA"),
    locales_rurales = sum(area_censo == "RURAL"),
    pct_rural = round(sum(area_censo == "RURAL") / n() * 100, 1),
    .groups = "drop"
  ) |>
  arrange(desc(total_locales))

# Mapa de locales rurales
locales_cusco |>
  filter(area_censo == "RURAL") |>
  ggplot() +
  geom_sf(color = "darkgreen", size = 0.8, alpha = 0.5) +
  labs(
    title = "Locales Educativos Rurales - Cusco",
    subtitle = "MINEDU",
    caption = "Fuente: MINEDU - Padrón de Instituciones Educativas"
  ) +
  theme_minimal()

# Contar servicios educativos por local
locales_cusco |>
  sf::st_drop_geometry() |>
  mutate(
    n_servicios = stringr::str_count(servicios, "\\(")
  ) |>
  count(n_servicios, name = "n_locales")

# Buscar locales por nombre de institución
locales_emblematicos <- locales_cusco |>
  filter(grepl("INCA GARCILASO|CLORINDA MATTO", servicios, ignore.case = TRUE))
} # }
```
