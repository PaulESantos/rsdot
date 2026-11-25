---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# rsdot <a href='https://github.com/PaulESantos/rsdot'><img src='man/figures/rsdot_logo.png' align="right" height="280" width="260" /></a>

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/rsdot)](https://CRAN.R-project.org/package=rsdot)
[![R-CMD-check](https://github.com/PaulESantos/rsdot/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PaulESantos/rsdot/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/PaulESantos/rsdot/graph/badge.svg)](https://app.codecov.io/gh/PaulESantos/rsdot)
<!-- badges: end -->

`rsdot` provides programmatic access to geographic information produced and collected by the Secretariat of Demarcation and Territorial Organization (SDOT) of the Presidency of the Council of Ministers (PCM) of Peru.

The package facilitates seamless download, caching, filtering, and manipulation of **35 official vector layers (226 partitioned GeoPackages hosted on OSF)** in R, including:

- **Official Census Limits**: Departments, provinces, and districts (INEI 2023 & updated 2025 boundaries)
- **Population Centers & Demography**: Capitals, rural settlements, intercensal growth, and population density grids
- **Transportation Networks**: National (SINAC), departmental, and rural/local roads, plus airports and airfields
- **Public & Social Infrastructure**: Health facilities (IPRESS - MINSA) and educational institutions (MINEDU)
- **Economic & Housing Density Models**: Spatial concentration grids for commerce, markets, financial services, real estate, and housing
- **Gross Value Added (VAB)**: CCPP-level gross value added models for national and metropolitan areas
- **Territorial Mobility & Commuting Flows**: Inter-district origin-destination commuting flows for workers and students
- **Rural Cadastre**: Cadastral property parcels and formalization polygons (MIDAGRI)
- **Telecommunications**: Mobile coverage (2G, 3G, 4G, 5G) by population center and transmission towers
- **Hazards & Disaster Risk Management**: Flooding points, mass movements, non-mitigable risk zones, and hazard assessments

### ⚠️ Important Legal Notice

The INEI census limits and SDOT modeling data provided by this package are **referential** and do not constitute official territorial boundaries nor have legal demarcatory effect.

---

## ✨ Features

- ✅ **Comprehensive Coverage**: 30 distinct spatial layers across 8 thematic domains.
- ✅ **Automated Remote Access**: Transparent downloading from high-availability OSF cloud storage with persistent local caching (`tools::R_user_dir`).
- ✅ **Offline / Staging Fallback**: Detects local staging datasets in `sdot_data/` for rapid offline analysis and continuous integration.
- ✅ **Standardized Administrative Filtering**: Case-insensitive filtering by department, province, and district across heterogeneous schemas (`nombdep`, `nombprov`, `nombdist`).
- ✅ **Robust Geometry Handling**: Automatic multi-threading verification with `st_make_valid()` and CRS validation (`EPSG:4326`).
- ✅ **Character Encoding Resilience**: Automatic detection and repair of Mojibake and Latin-1/UTF-8 encoding anomalies.
- ✅ **Tidyverse & Modern GIS Friendly**: Native integration with `sf`, `terra`, `dplyr`, and `ggplot2`.

---

## 📦 Available Functions by Domain

### 1. 🏛️ Census Limits and Territorial Organization
- `get_departamentos()` - Departmental census boundaries (INEI 2023).
- `get_provincias(departamento)` - Provincial census boundaries.
- `get_distritos(departamento, provincia, ano)` - District census boundaries (2023 or 2025).
- `get_distritos_2025(departamento, provincia)` - Updated 2025 district census boundaries.
- `get_capitales()` - Departmental, provincial, and district capital coordinates.
- `get_centros_poblados(departamento, provincia, distrito)` - Dispersed population centers and settlements.

### 2. 🛣️ Transportation Networks and Aerodromes
- `get_red_vial_nacional()` - National road network (SINAC / MTC).
- `get_red_vial_departamental(departamento, provincia)` - Departmental road network.
- `get_red_vial_vecinal(departamento, provincia)` - Local and rural road network.
- `get_aerodromos(departamento)` - Airports, aerodromes, and landing strips across Peru.

### 3. 🏥 Social Infrastructure and Public Services
- `get_ipress(departamento, provincia, distrito)` - Health establishments (IPRESS - MINSA/SDOT).
- `get_locales_educativos(departamento, provincia, distrito)` - Educational campuses and schools (MINEDU).
- `get_servicios_educativos(departamento, provincia, distrito)` - Educational services by level and modality.

### 4. 📈 Demographics and Territorial Accessibility
- `get_densidad_poblacional(departamento, provincia, distrito)` - Intercensal population density grid (2007-2017).
- `get_centros_poblados_crecimiento(departamento)` - Intercensal population growth rate by settlement.
- `get_ccpp_tasa_crecimiento(departamento)` - Alias for intercensal growth rate of population centers.
- `get_tiempos_desplazamiento(departamento, area_interes)` - Travel time isochrones and accessibility to nearest capitals.

### 5. 💼 Economic and Housing Densities
- `get_densidad_actividades(tipo, departamento)` - Spatial density grids of economic activities and housing.
- `get_densidad_comercio(departamento)` - Commercial activity density.
- `get_densidad_mercados(departamento)` - Abastecimiento markets and retail cluster density.
- `get_densidad_financiero(departamento)` - Financial and banking service density.
- `get_densidad_inmobiliario(departamento)` - Real estate development density.
- `get_densidad_viviendas(departamento)` - Housing unit density.
- `get_vab_ccpp(cobertura, departamento)` - Modeled Gross Value Added (VAB) for population centers (`total`, `lima`, `callao`).

### 6. 🚶 Territorial Mobility and Commuting Flows
- `get_flujos_movilidad(tipo, volumen)` - Inter-district daily mobility flows between residence and activity centroids.
- `get_flujos_estudiantes(volumen)` - Commuting flows for students (`todos`, `mayores_50`, `mayores_100`).
- `get_flujos_trabajadores(volumen)` - Commuting flows for workers (`todos`, `mayores_50`, `mayores_100`).

### 7. 🚜 Rural Cadastre and Property Boundaries
- `get_catastro_rural(departamento)` - Rural property parcels and titling polygons (MIDAGRI/SDOT).

### 8. 📶 Telecommunications
- `get_cobertura_movil(departamento, provincia, distrito)` - Mobile phone and internet coverage (2G, 3G, 4G, 5G).
- `get_cobertura_movil_c(departamento, provincia, distrito)` - Mobile coverage details and base station locations.

### 9. 🌋 Hazards and Disaster Risk Management
- `get_peligros(tipo, departamento)` - General hazard retriever (floods, mass movements, critical points/tramos).
- `get_puntos_criticos(departamento)` - Critical flood points (ANA / SDOT).
- `get_tramos_criticos(departamento)` - Critical river reaches exposed to flooding.
- `get_zonas_criticas(departamento)` - Critical zones by mass movements and geohazards (INGEMMET).
- `get_areas_inundacion(departamento)` - Flood exposure polygons.
- `get_areas_movimientos_masa(departamento)` - Mass movement hazard exposure polygons.
- `get_inventario_inundaciones(departamento)` - Historical flood inventory records.
- `get_riesgos(tipo, departamento)` - General disaster risk retriever.
- `get_evaluacion_riesgos(departamento)` - Disaster risk assessments (EVAR / CENEPRED).
- `get_zonas_riesgos_no_mitigable(departamento)` - Non-mitigable disaster risk zones.

### 10. ⚙️ Catalog Discovery and Maintenance Utilities
- `list_available_layers(domain)` - Inspect the full catalog of available layers (35 layers supported), descriptions, and formats.
- `rsdot_check_updates()` - Audit local cache status, file sizes, and download timestamps.
- `clear_rsdot_cache(layer)` - Purge specific or all cached layers to free disk space.

---

## 🗺️ Layer Coverage Matrix (GeoSDOT vs rsdot)

| Domain | GeoSDOT Source Layer | rsdot Function | Granularity / Partition |
| :--- | :--- | :--- | :--- |
| **Limits** | `geoportal:v_departamentos_2023` | `get_departamentos()` | National |
| **Limits** | `geoportal:v_provincias_2023` | `get_provincias()` | National / Department |
| **Limits** | `geoportal:v_distritos_2023` | `get_distritos(ano = 2023)` | National / Department / Province |
| **Limits** | `geoportal:v_distritos_2025` | `get_distritos_2025()` | National / Department / Province |
| **Limits** | `geoportal:v_capitales` | `get_capitales()` | National / Department |
| **Limits** | `geoportal:ccpp_asentamientos_dispersos` | `get_centros_poblados()` | Department / Province / District |
| **Transport** | `geoportal:v_red_vial_nacional` | `get_red_vial_nacional()` | National |
| **Transport** | `geoportal:v_red_vial_departamental` | `get_red_vial_departamental()` | Department / Province |
| **Transport** | `geoportal:v_red_vial_vecinal` | `get_red_vial_vecinal()` | Department / Province |
| **Transport** | `geoportal:v_aerodromos` | `get_aerodromos()` | Department |
| **Services** | `geoportal:v_ipress` | `get_ipress()` | Department / Province / District |
| **Services** | `geoportal:v_locales_educativos` | `get_locales_educativos()` | Department / Province / District |
| **Services** | `geoportal:v_servicios_educativos` | `get_servicios_educativos()` | Department / Province / District |
| **Demographics** | `geoportal:v_densidad_poblacional` | `get_densidad_poblacional()` | Department / Province / District |
| **Demographics** | `geoportal:v_ccpp_crecimiento` | `get_centros_poblados_crecimiento()` | Department |
| **Demographics** | `geoportal:v_tiempo_desplazamiento` | `get_tiempos_desplazamiento()` | Department / AOI Polygon |
| **Economic** | `geoportal:v_densidad_comercio` | `get_densidad_comercio()` | National / Department |
| **Economic** | `geoportal:v_densidad_mercados` | `get_densidad_mercados()` | National / Department |
| **Economic** | `geoportal:v_densidad_financiero` | `get_densidad_financiero()` | National / Department |
| **Economic** | `geoportal:v_densidad_inmobiliario` | `get_densidad_inmobiliario()` | National / Department |
| **Economic** | `geoportal:v_densidad_viviendas` | `get_densidad_viviendas()` | National / Department |
| **Economic** | `geoportal:v_vab_ccpp_*` | `get_vab_ccpp()` | National / Lima / Callao |
| **Mobility** | `geoportal:v_flujos_estudiantes_*` | `get_flujos_estudiantes()` | National (Thresholds) |
| **Mobility** | `geoportal:v_flujos_trabajadores_*` | `get_flujos_trabajadores()` | National (Thresholds) |
| **Cadastre** | `geoportal:v_catastro_rural` | `get_catastro_rural()` | National / Department |
| **Telecom** | `geoportal:cobertura_movil_oper` | `get_cobertura_movil()` | Department / Province / District |
| **Telecom** | `geoportal:cobertura_movil_c` | `get_cobertura_movil_c()` | Department / Province / District |
| **Hazards** | `geoportal:v_peligros_inundacion` | `get_areas_inundacion()` | Department (25) |
| **Hazards** | `geoportal:v_puntos_criticos_inundaciones` | `get_puntos_criticos()` | Department (25) |
| **Hazards** | `geoportal:v_tramos_criticos_inundaciones` | `get_tramos_criticos()` | Department (25) |
| **Hazards** | `geoportal:v_inventario_inundaciones` | `get_inventario_inundaciones()` | Department (25) |
| **Hazards** | `geoportal:v_peligros_movimientos_masa` | `get_areas_movimientos_masa()` | Department (25) |
| **Hazards** | `geoportal:v_zonas_criticas_movimientos_masa` | `get_zonas_criticas()` | Department (25) |
| **Risks** | `geoportal:v_evaluacion_riesgos` | `get_evaluacion_riesgos()` | Department (25) |
| **Risks** | `geoportal:v_zonas_riesgos_no_mitigable` | `get_zonas_riesgos_no_mitigable()` | Department (25) |

---

## 📦 Installation

Install the development version from GitHub:

```r
# Using pak (recommended)
install.packages("pak")
pak::pak("PaulESantos/rsdot")

# Or using remotes
install.packages("remotes")
remotes::install_github("PaulESantos/rsdot")
```

---

## 🚀 Usage Examples

### 1. Census Limits and Subsetting

```r
library(rsdot)
library(sf)
library(ggplot2)
library(dplyr)

# 1. Download official department boundaries
deps <- get_departamentos()

# 2. Get 2025 updated districts for a specific province
dist_cusco_2025 <- get_distritos_2025(departamento = "CUSCO", provincia = "CUSCO")

# Visualize
ggplot(dist_cusco_2025) +
  geom_sf(aes(fill = nombdist), color = "white", linewidth = 0.3) +
  theme_minimal() +
  labs(
    title = "Districts of the Province of Cusco (2025)",
    subtitle = "INEI Census Reference Boundary Update",
    fill = "District"
  )
```

### 2. Economic Activities and Housing Densities

```r
# Commercial activity density in Arequipa
comercio_arequipa <- get_densidad_comercio(departamento = "AREQUIPA")

# Plot density
ggplot(comercio_arequipa) +
  geom_sf(aes(fill = factor(nivel)), color = NA) +
  scale_fill_viridis_d(option = "magma", name = "Density Level") +
  theme_minimal() +
  labs(
    title = "Commercial Activity Density Grid",
    subtitle = "Department of Arequipa | GeoSDOT Modeling",
    caption = "Source: SDOT - PCM"
  )

# Gross Value Added (VAB) for Lima Metropolitan Area
vab_lima <- get_vab_ccpp(cobertura = "lima")
head(vab_lima)
```

### 3. Inter-District Commuting and Mobility Flows

```r
# Retrieve student mobility flows with volume > 100 trips
flujos_est <- get_flujos_estudiantes(volumen = "mayores_100")

# Summary of highest origin-destination student mobility pairs
flujos_est |>
  st_drop_geometry() |>
  arrange(desc(viajes)) |>
  head(10)
```

### 4. Rural Cadastre and Property Formalization

```r
# Retrieve rural cadastre parcels in San Martín
catastro_sm <- get_catastro_rural(departamento = "SAN MARTIN")

# Summary of parcel conditions
table(catastro_sm$condicion)
```

### 5. Multi-Department Healthcare & Education Infrastructure

```r
# Retrieve health establishments across southern departments
ipress_sur <- get_ipress(departamento = c("CUSCO", "AREQUIPA", "PUNO"))

# Tally by health category
table(ipress_sur$categoria)
```

### 6. Natural Hazards and Disaster Risk Management

```r
# Download critical flood points in Callao
pc_callao <- get_puntos_criticos(departamento = "CALLAO")

# Retrieve non-mitigable disaster risk zones in Lima
rnm_lima <- get_zonas_riesgos_no_mitigable(departamento = "LIMA")

# Query mass movement critical zones in Cusco
zc_cusco <- get_zonas_criticas(departamento = "CUSCO")
```

### 7. Discovery and Maintenance

```r
# List all 35 available layers
all_layers <- list_available_layers()
all_layers[, c("Capa", "Descripcion", "Funcion_R")]

# Check local cache status
rsdot_check_updates()

# Clear specific cached layer
clear_rsdot_cache("ipress")
```

---

## 📊 Data Sources

The geographic datasets provided by this package are compiled from official sources by:

- **Secretariat of Demarcation and Territorial Organization (SDOT-PCM)**: [GeoSDOT Geoportal](https://geosdot.servicios.gob.pe/visor/)
- **National Institute of Statistics and Informatics (INEI)**
- **Ministry of Transport and Communications (MTC)**
- **Ministry of Health (MINSA)**
- **Ministry of Education (MINEDU)**
- **Ministry of Agrarian Development and Irrigation (MIDAGRI)**
- **Open Science Framework (OSF)**: Repositories `qy4j6` and `demarca`

---

## 🤝 Contributions

Contributions, bug reports, and suggestions are welcome! Please open an issue or pull request:

👉 [https://github.com/PaulESantos/rsdot/issues](https://github.com/PaulESantos/rsdot/issues)

---

## 📄 License

This package is licensed under the MIT License. See [LICENSE](LICENSE) for details.

---

## 👤 Author

**Paul Efren Santos Andrade**

- ORCID: [0000-0002-6635-0375](https://orcid.org/0000-0002-6635-0375)
- Email: [paulefrens@gmail.com](mailto:paulefrens@gmail.com)

---

## 🌐 Package Documentation Website

Extended reference documentation and articles:

👉 [https://paulesantos.github.io/rsdot/](https://paulesantos.github.io/rsdot/)
