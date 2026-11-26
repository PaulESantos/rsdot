# Changelog

## rsdot 0.2.0

- **Complete Cloud Data Pipeline & OSF Integration**:
  - Full remote synchronization of **226 GeoPackage layers** into the
    official DEMARCA OSF repository (`qy4j6`).
  - Corrected direct binary file download streams via Waterbutler
    (`https://osf.io/download/{id}/`), eliminating HTML payload
    anomalies and optimizing download throughput.
  - Added dedicated OSF folder organization for Census Boundaries
    (2025), Geographic Modeling (Densities & VAB), Mobility Flows,
    Travel Times, Natural Hazards, and Disaster Risks.
- **Architecture Refactoring & Performance Optimizations**:
  - Unified departmental spatial dataset downloads through
    `load_departmental_spatial_data()` and `combine_sf_list()`,
    eliminating over 2,500 lines of repetitive boilerplate across 15+
    layer modules.
  - Centralized the complete catalog of 25 departments and OSF hashes in
    [`get_rsdot_catalog()`](https://paulesantos.github.io/rsdot/reference/get_rsdot_catalog.md),
    replacing hardcoded per-file dictionaries.
  - Standardized administrative filtering (`filter_spatial_admin()`)
    supporting case-insensitive department, province, and district
    queries across varied schema column names (`nombdep`/`nom_dep`,
    `nombprov`/`nom_prov`, `nombdist`/`nom_dist`).
  - Added intelligent local staging detection (`sdot_data/` directory
    fallback) for offline data processing and development environments.
  - Optimized geometry verification: skips redundant
    [`st_make_valid()`](https://r-spatial.github.io/sf/reference/valid.html)
    calls when `all(st_is_valid(x))` is TRUE.
- **New Spatial Layers & Direct Shortcut Functions**:
  - **Updated Census Boundaries**: Added
    [`get_distritos_2025()`](https://paulesantos.github.io/rsdot/reference/get_distritos.md)
    and `ano = 2025` support in
    [`get_distritos()`](https://paulesantos.github.io/rsdot/reference/get_distritos.md).
  - **Economic & Housing Activity Densities**: Added
    [`get_densidad_actividades()`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md)
    with specialized aliases
    [`get_densidad_comercio()`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md),
    [`get_densidad_mercados()`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md),
    [`get_densidad_financiero()`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md),
    [`get_densidad_inmobiliario()`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md),
    and
    [`get_densidad_viviendas()`](https://paulesantos.github.io/rsdot/reference/get_densidad_actividades.md).
  - **Gross Value Added (VAB) Modeling**: Added
    [`get_vab_ccpp()`](https://paulesantos.github.io/rsdot/reference/get_vab_ccpp.md)
    covering total national population centers as well as metropolitan
    partitions (`lima`, `callao`).
  - **Territorial Mobility & Commuting Flows**: Added
    [`get_flujos_movilidad()`](https://paulesantos.github.io/rsdot/reference/get_flujos_movilidad.md)
    with specialized shortcuts
    [`get_flujos_estudiantes()`](https://paulesantos.github.io/rsdot/reference/get_flujos_movilidad.md)
    and
    [`get_flujos_trabajadores()`](https://paulesantos.github.io/rsdot/reference/get_flujos_movilidad.md)
    supporting volume thresholds.
  - **Natural Hazards Shortcuts**: Added direct convenience functions
    [`get_puntos_criticos()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md),
    [`get_tramos_criticos()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md),
    [`get_zonas_criticas()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md),
    [`get_areas_inundacion()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md),
    [`get_areas_movimientos_masa()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md),
    and
    [`get_inventario_inundaciones()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md).
  - **Disaster Risks Shortcuts**: Added direct convenience functions
    [`get_evaluacion_riesgos()`](https://paulesantos.github.io/rsdot/reference/get_riesgos.md)
    and
    [`get_zonas_riesgos_no_mitigable()`](https://paulesantos.github.io/rsdot/reference/get_riesgos.md).
  - **Rural Cadastre**: Added
    [`get_catastro_rural()`](https://paulesantos.github.io/rsdot/reference/get_catastro_rural.md)
    for rural property parcels and titling polygons.
- **Discovery & Metadata**:
  - Expanded
    [`list_available_layers()`](https://paulesantos.github.io/rsdot/reference/list_available_layers.md)
    to **35 layers** across 8 thematic domains (`limits`, `transport`,
    `services`, `demography`, `telecom`, `hazards`, `mobility`,
    `cadastre`).
  - Comprehensive unit test suite expanded to **159 passing unit tests**
    with 0 failures and 0 warnings.
  - 100% CRAN check compliance: passes
    `devtools::check(cran = TRUE, manual = TRUE)` with **0 errors, 0
    warnings, 0 notes**.

## rsdot 0.1.0

- Initial release of the `rsdot` package for programmatic access to
  spatial data from the Secretariat of Demarcation and Territorial
  Organization (SDOT-PCM) of Peru.
- Added 20 spatial retrieval functions covering 7 thematic domains:
  - **Administrative Limits**:
    [`get_departamentos()`](https://paulesantos.github.io/rsdot/reference/get_departamentos.md),
    [`get_provincias()`](https://paulesantos.github.io/rsdot/reference/get_provincias.md),
    [`get_distritos()`](https://paulesantos.github.io/rsdot/reference/get_distritos.md).
  - **Population Centers & Demography**:
    [`get_capitales()`](https://paulesantos.github.io/rsdot/reference/get_capitales.md),
    [`get_centros_poblados()`](https://paulesantos.github.io/rsdot/reference/get_centros_poblados.md),
    [`get_centros_poblados_crecimiento()`](https://paulesantos.github.io/rsdot/reference/get_centros_poblados_crecimiento.md),
    [`get_densidad_poblacional()`](https://paulesantos.github.io/rsdot/reference/get_densidad_poblacional.md).
  - **Transportation Infrastructure**:
    [`get_aerodromos()`](https://paulesantos.github.io/rsdot/reference/get_aerodromos.md),
    [`get_red_vial_nacional()`](https://paulesantos.github.io/rsdot/reference/get_red_vial_nacional.md),
    [`get_red_vial_departamental()`](https://paulesantos.github.io/rsdot/reference/get_red_vial_departamental.md),
    [`get_red_vial_vecinal()`](https://paulesantos.github.io/rsdot/reference/get_red_vial_vecinal.md).
  - **Social Infrastructure & Public Services**:
    [`get_ipress()`](https://paulesantos.github.io/rsdot/reference/get_ipress.md),
    [`get_locales_educativos()`](https://paulesantos.github.io/rsdot/reference/get_locales_educativos.md),
    [`get_servicios_educativos()`](https://paulesantos.github.io/rsdot/reference/get_servicios_educativos.md).
  - **Telecommunications**:
    [`get_cobertura_movil()`](https://paulesantos.github.io/rsdot/reference/get_cobertura_movil_c.md)
    and
    [`get_cobertura_movil_c()`](https://paulesantos.github.io/rsdot/reference/get_cobertura_movil_c.md).
  - **Accessibility & Modeling**:
    [`get_tiempos_desplazamiento()`](https://paulesantos.github.io/rsdot/reference/get_tiempos_desplazamiento.md).
  - **Hazards & Risks**:
    [`get_peligros()`](https://paulesantos.github.io/rsdot/reference/get_peligros.md),
    [`get_riesgos()`](https://paulesantos.github.io/rsdot/reference/get_riesgos.md).
- Added discovery and maintenance tools:
  - [`list_available_layers()`](https://paulesantos.github.io/rsdot/reference/list_available_layers.md)
    to inspect the catalog of available layers by thematic domain.
  - [`rsdot_check_updates()`](https://paulesantos.github.io/rsdot/reference/rsdot_check_updates.md)
    to audit local cache status, file sizes, and freshness.
  - [`clear_rsdot_cache()`](https://paulesantos.github.io/rsdot/reference/clear_rsdot_cache.md)
    to easily purge downloaded data.
- Implemented persistent user cache with
  `tools::R_user_dir("rsdot", "cache")`.
- Automated Latin1/UTF-8 encoding correction and Mojibake repair
  (`fix_encoding_text()`) protecting characters like ‘Ñ’ and accents.
- Full test suite with 115 passing tests verified for CRAN compliance.
