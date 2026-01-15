# Obtener Servicios Educativos del Perú

Descarga y carga la información de servicios educativos del Perú,
elaborado por el Ministerio de Educación (MINEDU). Un servicio educativo
es el conjunto de actividades educativas diseñadas y organizadas para
lograr un objetivo de aprendizaje. Los datos incluyen geometría tipo
POINT e información sobre nivel educativo, modalidad, gestión, turno y
características del servicio. Permite filtrado escalonado por
departamento, provincia y distrito.

## Usage

``` r
get_servicios_educativos(
  departamento = NULL,
  provincia = NULL,
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

- show_progress:

  Logical. Si `TRUE` (por defecto), muestra mensajes informativos sobre
  el progreso de la descarga. Si `FALSE`, ejecuta de forma silenciosa.

- force_update:

  Logical. Si `TRUE`, fuerza una nueva descarga del archivo incluso si
  existe en caché. Por defecto `FALSE`.

- distrito:

  Character vector. Nombre(s) del/los distrito(s) para filtrar dentro de
  la(s) provincia(s) especificada(s). Opcional.

## Value

Un objeto `sf` (simple feature) con geometría tipo POINT que contiene
información de los servicios educativos, incluyendo:

**Identificación del servicio:**

- Geometría POINT (coordenadas del servicio educativo)

- `cod_mod`: Código modular del servicio educativo (identificador único
  asignado por MINEDU)

- `anexo`: Número de anexo del servicio educativo (0 si es sede
  principal)

- `cen_edu`: Nombre del centro educativo o institución

- `codinst`: Código de la institución (RUC o código interno)

**Nivel y modalidad educativa:**

- `d_niv_mod`: Nivel y modalidad del servicio educativo

- `d_forma`: Forma de atención:

  - ESCOLARIZADO: Educación presencial en aula

  - NO ESCOLARIZADO: Programas no escolarizados (PRONOEI, etc.)

  - NO APLICA:

**Gestión y dependencia:**

- `d_gestion`: Tipo de gestión:

  - PÚBLICA DE GESTIÓN DIRECTA: Administrada directamente por el Estado

  - PÚBLICA DE GESTIÓN PRIVADA: Pública pero administrada por privados
    (convenios)

  - PRIVADA: Administración particular

- `d_ges_dep`: Dependencia específica de la gestión

- `d_dreugel`: DRE o UGEL de dependencia administrativa

**Características del servicio:**

- `d_cod_car`: Característica del docente según número de grados a
  cargo:

  - POLIDOCENTE COMPLETO: Un docente por grado/sección

  - POLIDOCENTE MULTIGRADO: Docentes atienden varios grados

  - UNIDOCENTE: Un solo docente para todos los grados

  - NO APLICA: No corresponde al nivel educativo

  - NO DISPONIBLE:

- `d_tipssexo`: Tipo de servicio según sexo de estudiantes:

  - MIXTO: Atiende a ambos sexos

  - VARONES: Solo varones

  - MUJERES: Solo mujeres

  - NO APLICA

- `d_tipoprog`: Tipo de programa (para servicios no escolarizados)

- `d_cod_tur`: Turno de atención

**Ubicación geográfica:**

- `departamen`: Nombre del departamento

- `provincia`: Nombre de la provincia

- `nlat_ie`: Latitud del servicio educativo (coordenada Y)

- `nlong_ie`: Longitud del servicio educativo (coordenada X)

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

**Fuente de los datos:**

- Fuente: Ministerio de Educación (MINEDU)

- Registro: Padrón de Instituciones Educativas

- Nivel: Servicio educativo

- Aplicación: Análisis de oferta educativa, planificación de política
  educativa y estudios de cobertura por nivel y modalidad

**Diferencia entre local educativo y servicio educativo:**

- **Local educativo**: Infraestructura física (inmueble) donde funcionan
  los servicios. Ver
  [`get_locales_educativos`](https://paulesantos.github.io/rsdot/reference/get_locales_educativos.md).

- **Servicio educativo**: Unidad de gestión pedagógica que brinda un
  nivel o modalidad específica de educación. Un local puede albergar
  múltiples servicios educativos.

**Sistema educativo peruano - Niveles y modalidades:**

**Educación Básica Regular (EBR):**

- **Inicial**: Cuna (0-2 años), Jardín (3-5 años)

- **Primaria**: 6 grados (6-11 años)

- **Secundaria**: 5 grados (12-16 años)

**Clasificación por característica docente (d_cod_car):**

- **Polidocente completo**: Cada sección tiene su propio docente. Típico
  en zonas urbanas.

- **Polidocente multigrado**: Varios docentes que atienden más de un
  grado cada uno.

- **Unidocente**: Un solo docente atiende todos los grados. Frecuente en
  zonas rurales con poca matrícula.

**Filtrado jerárquico:** Los filtros se aplican en cascada:

1.  Primero se cargan los departamentos especificados

2.  Luego se filtran las provincias (si se especifican)

El caché se almacena en: `tempdir()/DEMARCA_cache/servicios_educativos/`

**NOTA:** Las geometrías son tipo POINT (puntos) y representan la
ubicación de cada servicio educativo.

## References

Ministerio de Educación del Perú (MINEDU). Padrón de Instituciones
Educativas y Programas Educativos.

ESCALE - Estadística de la Calidad Educativa:
<https://escale.minedu.gob.pe/>

Repositorio DEMARCA en OSF: <https://osf.io/qy4j6/>

## See also

[`get_locales_educativos`](https://paulesantos.github.io/rsdot/reference/get_locales_educativos.md),
[`get_departamentos`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
[`get_provincias`](https://paulesantos.github.io/rsdot/reference/get_provincias.md),
[`get_distritos`](https://paulesantos.github.io/rsdot/reference/get_distritos.md),
[`read_sf`](https://r-spatial.github.io/sf/reference/st_read.html)

## Examples

``` r
if (FALSE) { # \dontrun{
# Ver departamentos disponibles
get_servicios_educativos()

# Cargar servicios educativos de un departamento completo
serv_cusco <- get_servicios_educativos(departamento = "CUSCO")

# Filtrar por provincia específica
serv_prov_cusco <- get_servicios_educativos(
  departamento = "CUSCO",
  provincia = "CUSCO"
)

# Filtrar por distrito específico
serv_prov_urubamba <- get_servicios_educativos(
  departamento = "CUSCO",
  provincia = "URUBAMBA"
)

# Cargar múltiples departamentos
serv_sur <- get_servicios_educativos(
  departamento = c("CUSCO", "PUNO", "AREQUIPA")
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Mapa de servicios educativos por nivel
serv_cusco |>
  filter(d_niv_mod %in% c("PRIMARIA", "SECUNDARIA", "INICIAL - JARDÍN")) |>
  ggplot() +
  geom_sf(aes(color = d_niv_mod), size = 1, alpha = 0.6) +
  scale_color_brewer(palette = "Set1", name = "Nivel") +
  labs(
    title = "Servicios Educativos por Nivel - Cusco",
    subtitle = "Educación Básica Regular",
    caption = "Fuente: MINEDU - Padrón de Instituciones Educativas"
  ) +
  theme_minimal()

# Resumen por nivel y modalidad
serv_cusco |>
  sf::st_drop_geometry() |>
  count(d_niv_mod, sort = TRUE)

# Distribución por tipo de gestión
serv_cusco |>
  sf::st_drop_geometry() |>
  count(d_gestion, sort = TRUE)

# Servicios públicos vs privados
serv_cusco |>
  sf::st_drop_geometry() |>
  count(d_ges_dep, sort = TRUE)

# Filtrar solo educación inicial
serv_inicial <- serv_cusco |>
  filter(grepl("INICIAL", d_niv_mod))

# Filtrar servicios de secundaria pública
serv_secund_publica <- serv_cusco |>
  filter(
    d_niv_mod == "SECUNDARIA",
    d_gestion == "PÚBLICA DE GESTIÓN DIRECTA"
  )

# Análisis de escuelas unidocentes (rurales)
serv_unidocente <- serv_cusco |>
  filter(d_cod_car == "UNIDOCENTE")

# Mapa de escuelas unidocentes
ggplot(serv_unidocente) +
  geom_sf(color = "darkred", size = 1.5, alpha = 0.7) +
  labs(
    title = "Servicios Educativos Unidocentes - Cusco",
    subtitle = "Un docente para todos los grados",
    caption = "Fuente: MINEDU"
  ) +
  theme_minimal()

# Análisis por UGEL
serv_cusco |>
  sf::st_drop_geometry() |>
  count(d_dreugel, sort = TRUE)

# Distribución por turno de atención
serv_cusco |>
  sf::st_drop_geometry() |>
  count(d_cod_tur, sort = TRUE)

# Servicios no escolarizados (PRONOEI, etc.)
serv_no_escolarizado <- serv_cusco |>
  filter(d_forma == "NO ESCOLARIZADO")

# Análisis de cobertura por provincia y nivel
cobertura <- serv_cusco |>
  sf::st_drop_geometry() |>
  filter(d_niv_mod %in% c("PRIMARIA", "SECUNDARIA")) |>
  group_by(provincia, d_niv_mod) |>
  summarise(
    total_servicios = n(),
    pct_publica = round(
      sum(d_gestion == "PÚBLICA DE GESTIÓN DIRECTA") / n() * 100, 1
    ),
    .groups = "drop"
  ) |>
  tidyr::pivot_wider(
    names_from = d_niv_mod,
    values_from = c(total_servicios, pct_publica)
  )

# Mapa comparativo público vs privado
serv_cusco |>
  filter(d_niv_mod == "SECUNDARIA") |>
  mutate(
    tipo_gestion = ifelse(
      grepl("PÚBLICA", d_gestion), "Pública", "Privada"
    )
  ) |>
  ggplot() +
  geom_sf(aes(color = tipo_gestion), size = 2, alpha = 0.6) +
  scale_color_manual(
    values = c("Pública" = "darkblue", "Privada" = "darkred"),
    name = "Gestión"
  ) +
  labs(
    title = "Servicios de Secundaria por Tipo de Gestión - Cusco",
    caption = "Fuente: MINEDU"
  ) +
  theme_minimal()

# CETPRO - Educación técnico productiva
cetpro <- serv_cusco |>
  filter(grepl("TÉCNICO PRODUCTIVA", d_niv_mod))

# Institutos superiores
superiores <- serv_cusco |>
  filter(grepl("SUPERIOR", d_niv_mod))
} # }
```
