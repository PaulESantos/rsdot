# rsdot: Quick Overview (English)

## Overview

`rsdot` provides programmatic access to official geospatial information
produced and compiled by the Secretariat of Demarcation and Territorial
Organization (**SDOT**) of the Presidency of the Council of Ministers
(**PCM**) of Peru.

The package facilitates seamless downloading, persistent local caching,
administrative filtering, and spatial manipulation in R across **35
official vector layers** (226 partitioned GeoPackages hosted on OSF):

- **Official Census Limits**: Departments, provinces, and districts
  (INEI 2023 & updated 2025 boundaries).
- **Population Centers & Demography**: Capitals, dispersed settlements,
  intercensal growth rates, and population density grids.
- **Transportation Networks**: National (SINAC), departmental, and
  local/rural roads, plus aerodromes.
- **Social Infrastructure**: Health facilities (IPRESS - MINSA) and
  educational facilities/services (MINEDU).
- **Economic & Housing Densities**: Spatial concentration grids for
  commerce, markets, financial services, real estate, housing, and Gross
  Value Added (VAB).
- **Mobility & Travel Times**: Inter-district commuting flows (students
  and workers) and travel time isochrones to nearest capitals.
- **Hazards & Disaster Risk Management**: Flood exposure, critical
  points/reaches, mass movement critical zones, risk assessments (EVAR),
  and non-mitigable risk zones.
- **Telecommunications & Rural Cadastre**: Mobile coverage (2G/3G/4G/5G)
  and MIDAGRI rural property parcels.

> **Legal Notice**: The INEI census limits and SDOT modeling datasets
> provided by `rsdot` are **referential** and do not constitute official
> territorial boundaries nor carry legal demarcatory effect.

------------------------------------------------------------------------

## Installation

``` r

# Install from GitHub using pak (recommended)
install.packages("pak")
pak::pak("PaulESantos/rsdot")

# Or using remotes
install.packages("remotes")
remotes::install_github("PaulESantos/rsdot")
```

------------------------------------------------------------------------

## Main Functions by Domain

| Domain | Key Functions |
|:---|:---|
| **Census Limits** | [`get_departamentos()`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md), [`get_provincias()`](https://paulesantos.github.io/rsdot/reference/get_provincias.md), [`get_distritos()`](https://paulesantos.github.io/rsdot/reference/get_distritos.md), [`get_distritos_2025()`](https://paulesantos.github.io/rsdot/reference/get_distritos.md), [`get_capitales()`](https://paulesantos.github.io/rsdot/reference/get_capitales.md), [`get_centros_poblados()`](https://paulesantos.github.io/rsdot/reference/get_centros_poblados.md) |
| **Transport** | [`get_red_vial_nacional()`](https://paulesantos.github.io/rsdot/reference/get_red_vial_nacional.md), [`get_red_vial_departamental()`](https://paulesantos.github.io/rsdot/reference/get_red_vial_departamental.md), [`get_red_vial_vecinal()`](https://paulesantos.github.io/rsdot/reference/get_red_vial_vecinal.md), [`get_aerodromos()`](https://paulesantos.github.io/rsdot/reference/get_aerodromos.md) |
| **Services** | [`get_ipress()`](https://paulesantos.github.io/rsdot/reference/get_ipress.md), [`get_locales_educativos()`](https://paulesantos.github.io/rsdot/reference/get_locales_educativos.md), [`get_servicios_educativos()`](https://paulesantos.github.io/rsdot/reference/get_servicios_educativos.md) |
| **Demography & Accessibility** | [`get_densidad_poblacional()`](https://paulesantos.github.io/rsdot/reference/get_densidad_poblacional.md), [`get_centros_poblados_crecimiento()`](https://paulesantos.github.io/rsdot/reference/get_centros_poblados_crecimiento.md), [`get_tiempos_desplazamiento()`](https://paulesantos.github.io/rsdot/reference/get_tiempos_desplazamiento.md) |
| **Economic & Housing** | [`get_densidad_comercio()`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md), [`get_densidad_mercados()`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md), [`get_densidad_financiero()`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md), [`get_densidad_inmobiliario()`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md), [`get_densidad_viviendas()`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md), [`get_vab_ccpp()`](https://paulesantos.github.io/rsdot/reference/get_vab_ccpp.md) |
| **Mobility Flows** | [`get_flujos_estudiantes()`](https://paulesantos.github.io/rsdot/reference/get_flujos_movilidad.md), [`get_flujos_trabajadores()`](https://paulesantos.github.io/rsdot/reference/get_flujos_movilidad.md) |
| **Hazards & Risks** | [`get_peligros()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md), [`get_puntos_criticos()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md), [`get_tramos_criticos()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md), [`get_zonas_criticas()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md), [`get_areas_inundacion()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md), [`get_areas_movimientos_masa()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md), [`get_inventario_inundaciones()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md), [`get_riesgos()`](https://paulesantos.github.io/rsdot/reference/get_riesgos.md), [`get_evaluacion_riesgos()`](https://paulesantos.github.io/rsdot/reference/get_riesgos.md), [`get_zonas_riesgos_no_mitigable()`](https://paulesantos.github.io/rsdot/reference/get_riesgos.md) |
| **Telecom & Cadastre** | [`get_cobertura_movil()`](https://paulesantos.github.io/rsdot/reference/get_cobertura_movil_c.md), [`get_cobertura_movil_c()`](https://paulesantos.github.io/rsdot/reference/get_cobertura_movil_c.md), [`get_catastro_rural()`](https://paulesantos.github.io/rsdot/reference/get_catastro_rural.md) |
| **Catalog & Cache** | [`list_available_layers()`](https://paulesantos.github.io/rsdot/reference/list_available_layers.md), [`rsdot_check_updates()`](https://paulesantos.github.io/rsdot/reference/rsdot_check_updates.md), [`clear_rsdot_cache()`](https://paulesantos.github.io/rsdot/reference/clear_rsdot_cache.md) |

------------------------------------------------------------------------

## Quick Start Examples

``` r

library(rsdot)

# 1. Discover all 35 available layers
list_available_layers()

# 2. Retrieve updated 2025 districts for Cusco province
dist_cusco <- get_distritos_2025(departamento = "CUSCO", provincia = "CUSCO")

# 3. Query critical flood points and non-mitigable risk zones
pc_callao <- get_puntos_criticos(departamento = "CALLAO")
rnm_lima  <- get_zonas_riesgos_no_mitigable(departamento = "LIMA")

# 4. Retrieve economic density grids or commuting flows
comercio  <- get_densidad_comercio(departamento = "AREQUIPA")
flujos    <- get_flujos_estudiantes(rango = "100_1000")

# 5. Audit local cache
rsdot_check_updates()
```
