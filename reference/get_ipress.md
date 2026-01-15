# Obtener Instituciones Prestadoras de Servicios de Salud (IPRESS) del Perú

Descarga y carga la información de Instituciones Prestadoras de
Servicios de Salud (IPRESS) del Perú, elaborado por la Superintendencia
Nacional de Salud (SUSALUD). Los datos incluyen geometría tipo POINT e
información sobre establecimientos de salud, servicios médicos de apoyo,
categorización, horarios de atención y redes de salud. Actualizado al
año 2024. Permite filtrado escalonado por departamento, provincia y
distrito.

## Usage

``` r
get_ipress(
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
información de las IPRESS, incluyendo:

**Identificación del establecimiento:**

- Geometría POINT (coordenadas del establecimiento)

- `cod_unic_e`: Código único del establecimiento (RENIPRESS)

- `nom_esta`: Nombre del establecimiento de salud

- `dir_esta`: Dirección completa del establecimiento

- `num_tele`: Número de teléfono de contacto

**Tipo y clasificación:**

- `cod_tipo_e`: Código del tipo de establecimiento

- `des_tipo_e`: Descripción del tipo de establecimiento:

  - 1 = Establecimiento de salud sin internamiento

  - 2 = Establecimiento de salud con internamiento

  - 3 = Servicio médico de apoyo

  - 4 = Oferta flexible

  - 5 = Comunidades terapeuticas

- `cod_clas_e`: Código de clasificación del establecimiento

- `des_clas_e`: Descripción de la clasificación (ej: Puestos de salud,
  Centros de salud, Hospitales, Clínicas, Consultorios, etc.)

**Institución administradora:**

- `cod_inst_d`: Código de la institución administradora

- `des_inst_d`: Descripción de la institución administradora

**Ubicación geográfica:**

- `cod_ubig_e`: Código de ubigeo del establecimiento

- `nombdep`: Nombre del departamento

- `nombprov`: Nombre de la provincia

- `nombdist`: Nombre del distrito

- `val_lati`: Latitud del establecimiento

- `val_long`: Longitud del establecimiento

**Categorización y operación:**

- `cod_cate`: Código de categoría del establecimiento (ej: I-1, I-2,
  I-3, I-4, II-1, II-2, II-E, III-1, III-2, III-E)

- `doc_cate`: Documento de resolución de categorización

- `fec_inic_o`: Fecha de inicio de operaciones

- `des_hora_e`: Horario de atención del establecimiento

- `cod_cond_e`: Condición del establecimiento (ej: ACTIVO, INACTIVO)

**Red de salud:**

- `cod_auto_s`: Código de autoridad sanitaria

- `des_auto_s`: Descripción de autoridad sanitaria (región)

- `cod_reds`: Código de red de salud

- `des_reds`: Nombre de la red de salud

- `cod_micr_r`: Código de micro red

- `des_micr_r`: Nombre de la micro red

**Datos del representante:**

- `cod_nruc`: Número de RUC de la institución

- `nom_repr_l`: Nombre del representante legal

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

**Fuente de los datos:**

- Fuente: Superintendencia Nacional de Salud (SUSALUD)

- Registro: RENIPRESS (Registro Nacional de IPRESS)

- Año de actualización: 2024

- Nivel: Establecimiento de salud

- Aplicación: Análisis de acceso a servicios de salud, planificación
  sanitaria y estudios epidemiológicos

**Sistema de categorización de establecimientos de salud en Perú:**

La categorización determina la capacidad resolutiva del establecimiento:

- **Primer nivel (I-1 a I-4)**: Atención básica y preventiva

  - I-1: Puesto de salud (técnico de enfermería)

  - I-2: Puesto de salud (profesional de salud)

  - I-3: Centro de salud sin internamiento

  - I-4: Centro de salud con internamiento

- **Segundo nivel (II-1, II-2, II-E)**: Atención especializada

  - II-1: Hospital I

  - II-2: Hospital II

  - II-E: Hospital especializado

- **Tercer nivel (III-1, III-2, III-E)**: Alta especialización

  - III-1: Hospital III

  - III-2: Instituto especializado

  - III-E: Instituto especializado de alta complejidad

**Tipos de IPRESS:**

1.  **Establecimientos sin internamiento**: Consultorios, puestos de
    salud, centros médicos, policlínicos

2.  **Establecimientos con internamiento**: Hospitales, clínicas,
    centros de salud con camas

3.  **Servicios médicos de apoyo**: Laboratorios, centros de diagnóstico
    por imágenes, bancos de sangre, patología clínica

**Filtrado jerárquico:** Los filtros se aplican en cascada:

1.  Primero se cargan los departamentos especificados

2.  Luego se filtran las provincias (si se especifican)

3.  Finalmente se filtran los distritos (si se especifican)

El caché se almacena en: `tempdir()/DEMARCA_cache/ipress/`

**NOTA:** Las geometrías son tipo POINT (puntos) y representan la
ubicación de cada establecimiento de salud.

## References

Superintendencia Nacional de Salud (SUSALUD). Registro Nacional de
Instituciones Prestadoras de Servicios de Salud (RENIPRESS) 2024.

Ministerio de Salud del Perú. Norma Técnica de Categorización de
Establecimientos de Salud.

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
get_ipress()

# Cargar IPRESS de un departamento completo
ipress_cusco <- get_ipress(departamento = "CUSCO")

# Filtrar por provincia específica
ipress_prov_cusco <- get_ipress(
  departamento = "CUSCO",
  provincia = "CUSCO"
)

# Filtrar por distrito específico
ipress_wanchaq <- get_ipress(
  departamento = "CUSCO",
  provincia = "CUSCO",
  distrito = "WANCHAQ"
)

# Cargar múltiples departamentos
ipress_sur <- get_ipress(
  departamento = c("CUSCO", "PUNO", "AREQUIPA")
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Mapa de IPRESS por tipo de establecimiento
ggplot(ipress_cusco) +
  geom_sf(aes(color = des_tipo_e), size = 1.5, alpha = 0.7) +
  scale_color_brewer(palette = "Set1", name = "Tipo") +
  labs(
    title = "Instituciones Prestadoras de Servicios de Salud - Cusco",
    subtitle = "SUSALUD 2024",
    caption = "Fuente: SUSALUD - RENIPRESS"
  ) +
  theme_minimal()

# Resumen por tipo de establecimiento
ipress_cusco |>
  sf::st_drop_geometry() |>
  count(des_tipo_e, sort = TRUE)

# Resumen por institución administradora
ipress_cusco |>
  sf::st_drop_geometry() |>
  count(des_inst_d, sort = TRUE)

# Filtrar establecimientos públicos activos
ipress_publicos <- ipress_cusco |>
  filter(
    des_inst_d == "GOBIERNO REGIONAL",
    cod_cond_e == "ACTIVO"
  )

# Análisis por categoría
ipress_cusco |>
  sf::st_drop_geometry() |>
  count(cod_cate, sort = TRUE) |>
  head(10)

# Mapa de hospitales y clínicas (establecimientos con internamiento)
ipress_cusco |>
  filter(cod_tipo_e == "2") |>
  ggplot() +
  geom_sf(aes(color = des_inst_d), size = 3) +
  scale_color_brewer(palette = "Dark2", name = "Administrador") +
  labs(
    title = "Establecimientos de Salud con Internamiento - Cusco",
    subtitle = "Hospitales y Clínicas",
    caption = "Fuente: SUSALUD - RENIPRESS 2024"
  ) +
  theme_minimal()

# Análisis de cobertura por distrito
cobertura_distrito <- ipress_cusco |>
  sf::st_drop_geometry() |>
  group_by(nombdist) |>
  summarise(
    total_ipress = n(),
    con_internamiento = sum(cod_tipo_e == "2"),
    sin_internamiento = sum(cod_tipo_e == "1"),
    servicios_apoyo = sum(cod_tipo_e == "3"),
    .groups = "drop"
  ) |>
  arrange(desc(total_ipress))

# Distribución por red de salud
ipress_cusco |>
  sf::st_drop_geometry() |>
  filter(!is.na(des_reds)) |>
  count(des_reds, sort = TRUE)

# Mapa de establecimientos por categoría (primer nivel)
ipress_cusco |>
  filter(grepl("^I-", cod_cate)) |>
  ggplot() +
  geom_sf(aes(color = cod_cate), size = 2, alpha = 0.7) +
  scale_color_viridis_d(name = "Categoría") +
  labs(
    title = "Establecimientos de Primer Nivel - Cusco",
    subtitle = "Categorías I-1 a I-4",
    caption = "Fuente: SUSALUD - RENIPRESS 2024"
  ) +
  theme_minimal()

# Buscar establecimientos por nombre
hospitales_regionales <- ipress_cusco |>
  filter(grepl("REGIONAL|HOSPITAL", nom_esta, ignore.case = TRUE))
} # }
```
