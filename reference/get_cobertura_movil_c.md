# Obtener Cobertura de Servicio Móvil por Centros Poblados del Perú

Descarga y carga la información de cobertura de servicio móvil por
centros poblados del Perú, elaborado por el Ministerio de Transportes y
Comunicaciones (MTC) y el Organismo Supervisor de Inversión Privada en
Telecomunicaciones (OSIPTEL). Los datos incluyen geometría tipo POINT e
información sobre disponibilidad de tecnologías 2G, 3G, 4G y 5G, así
como servicios de voz, SMS y datos. Permite filtrado escalonado por
departamento, provincia y distrito.

## Usage

``` r
get_cobertura_movil_c(
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
información de cobertura móvil por centro poblado, incluyendo:

**Identificación geográfica:**

- Geometría POINT (coordenadas del centro poblado)

- `n`: Número de registro

- `ubigeo_ccp`: Código de ubicación geográfica del centro poblado

- `departamen`: Nombre del departamento

- `provincia`: Nombre de la provincia

- `distrito`: Nombre del distrito

- `centro_pob`: Nombre del centro poblado

- `y_latitud`: Latitud del centro poblado

- `x_longitud`: Longitud del centro poblado

- `emoperador`: Indicador de presencia de operador

- `layer`: Capa de origen

**Tecnologías de red móvil** (valores: 1 = disponible, 0 = no
disponible):

- `2g`: Tecnología 2G declarada por la empresa operadora a nivel de
  centro poblado

- `3g`: Tecnología 3G declarada por la empresa operadora a nivel de
  centro poblado

- `4g`: Tecnología 4G declarada por la empresa operadora a nivel de
  centro poblado

- `5g`: Tecnología 5G declarada por la empresa operadora a nivel de
  centro poblado

**Servicios de comunicación** (valores: 1 = disponible, 0 = no
disponible):

- `voz`: Servicio de voz brindado por la empresa operadora a nivel de
  centro poblado

- `sms`: Servicio de mensaje de texto corto (SMS) brindado por la
  empresa operadora a nivel de centro poblado

- `mms`: Servicio de mensaje multimedia (MMS) brindado por la empresa
  operadora a nivel de centro poblado

**Velocidad de internet móvil** (valores: 1 = disponible, 0 = no
disponible):

- `hasta1mbps`: Servicio de acceso a internet móvil con velocidad de
  hasta 1 Mbps brindado por la empresa operadora

- `masde1mbps`: Servicio de acceso a internet móvil con velocidad de más
  de 1 Mbps brindado por la empresa operadora

**Infraestructura - Estaciones base:**

- `canteb2g`: Cantidad de estaciones base que pueden brindar servicio de
  cobertura móvil con tecnología 2G al centro poblado

- `canteb3g`: Cantidad de estaciones base que pueden brindar servicio de
  cobertura móvil con tecnología 3G al centro poblado

- `canteb4g`: Cantidad de estaciones base que pueden brindar servicio de
  cobertura móvil con tecnología 4G al centro poblado

- `canteb5g`: Cantidad de estaciones base que pueden brindar servicio de
  cobertura móvil con tecnología 5G al centro poblado

Si se solicitan múltiples departamentos, retorna un objeto sf combinado.

## Details

La función descarga datos desde OSF (Open Science Framework) y los
almacena en caché durante la sesión de R. Los datos están en formato
GeoPackage (.gpkg).

**Fuente de los datos:**

- Fuente: Ministerio de Transportes y Comunicaciones (MTC) / OSIPTEL

- Nivel: Centro poblado

- Aplicación: Análisis de brecha digital y planificación de
  telecomunicaciones

**Interpretación de variables binarias:**

Las variables de tecnología (2G, 3G, 4G, 5G), servicios (VOZ, SMS, MMS)
y velocidad (hasta1mbps, masde1mbps) son declaradas por las empresas
operadoras y toman únicamente valores de 0 o 1:

- **Valor 1**: El centro poblado cuenta con la tecnología/servicio

- **Valor 0**: El centro poblado NO cuenta con la tecnología/servicio

**Tecnologías de red móvil:**

- **2G (GSM)**: Segunda generación - servicios básicos de voz y SMS

- **3G (UMTS/HSPA)**: Tercera generación - datos móviles básicos,
  navegación web limitada

- **4G (LTE)**: Cuarta generación - banda ancha móvil, streaming de
  video, aplicaciones en tiempo real

- **5G**: Quinta generación - ultra banda ancha, baja latencia, IoT
  masivo

**Estaciones base:**

Las variables canteb2g, canteb3g, canteb4g y canteb5g indican la
cantidad de estaciones base (antenas) que pueden brindar servicio de
cobertura móvil con cada tecnología al centro poblado. Un mayor número
de estaciones base generalmente indica mejor calidad de señal y
capacidad de red.

**Filtrado jerárquico:** Los filtros se aplican en cascada:

1.  Primero se cargan los departamentos especificados

2.  Luego se filtran las provincias (si se especifican)

3.  Finalmente se filtran los distritos (si se especifican)

El caché se almacena en: `tempdir()/DEMARCA_cache/cobertura_movil/`

**NOTA:** Las geometrías son tipo POINT (puntos) y representan la
ubicación de cada centro poblado con información de cobertura.

## References

Ministerio de Transportes y Comunicaciones (MTC). Cobertura de Servicio
Móvil por Centros Poblados.

OSIPTEL - Organismo Supervisor de Inversión Privada en
Telecomunicaciones.

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
get_cobertura_movil_c()

# Cargar cobertura de un departamento completo
cob_cusco <- get_cobertura_movil_c(departamento = "CUSCO")

# Filtrar por provincia específica
cob_prov_cusco <- get_cobertura_movil_c(
  departamento = "CUSCO",
  provincia = "CUSCO"
)

# Filtrar por distrito específico
cob_san_sebastian <- get_cobertura_movil_c(
  departamento = "CUSCO",
  provincia = "CUSCO",
  distrito = "SAN SEBASTIAN"
)

# Cargar múltiples departamentos
cob_sur <- get_cobertura_movil_c(
  departamento = c("CUSCO", "PUNO", "AREQUIPA")
)

# Visualización con ggplot2
library(ggplot2)
library(dplyr)

# Mapa de cobertura 4G
ggplot(cob_cusco) +
  geom_sf(aes(color = factor(`4g`)), size = 1, alpha = 0.6) +
  scale_color_manual(
    values = c("0" = "red", "1" = "darkgreen"),
    labels = c("0" = "Sin cobertura", "1" = "Con cobertura"),
    name = "Cobertura 4G"
  ) +
  labs(
    title = "Cobertura 4G en Centros Poblados de Cusco",
    subtitle = "MTC / OSIPTEL",
    caption = "Fuente: MTC | Visor - SDOT"
  ) +
  theme_minimal()

# Resumen de cobertura por tecnología
cob_cusco |>
  sf::st_drop_geometry() |>
  summarise(
    total_ccpp = n(),
    con_2g = sum(`2g`),
    con_3g = sum(`3g`),
    con_4g = sum(`4g`),
    con_5g = sum(`5g`),
    pct_4g = round(sum(`4g`) / n() * 100, 1)
  )
} # }
```
