# rsdot 0.2.0

* **Complete Cloud Data Pipeline & OSF Integration**:
  - Full remote synchronization of **226 GeoPackage layers** into the official DEMARCA OSF repository (`qy4j6`).
  - Corrected direct binary file download streams via Waterbutler (`https://osf.io/download/{id}/`), eliminating HTML payload anomalies and optimizing download throughput.
  - Added dedicated OSF folder organization for Census Boundaries (2025), Geographic Modeling (Densities & VAB), Mobility Flows, Travel Times, Natural Hazards, and Disaster Risks.

* **Architecture Refactoring & Performance Optimizations**:
  - Unified departmental spatial dataset downloads through `load_departmental_spatial_data()` and `combine_sf_list()`, eliminating over 2,500 lines of repetitive boilerplate across 15+ layer modules.
  - Centralized the complete catalog of 25 departments and OSF hashes in `get_rsdot_catalog()`, replacing hardcoded per-file dictionaries.
  - Standardized administrative filtering (`filter_spatial_admin()`) supporting case-insensitive department, province, and district queries across varied schema column names (`nombdep`/`nom_dep`, `nombprov`/`nom_prov`, `nombdist`/`nom_dist`).
  - Added intelligent local staging detection (`sdot_data/` directory fallback) for offline data processing and development environments.
  - Optimized geometry verification: skips redundant `st_make_valid()` calls when `all(st_is_valid(x))` is TRUE.

* **New Spatial Layers & Direct Shortcut Functions**:
  - **Updated Census Boundaries**: Added `get_distritos_2025()` and `ano = 2025` support in `get_distritos()`.
  - **Economic & Housing Activity Densities**: Added `get_densidad_actividades()` with specialized aliases `get_densidad_comercio()`, `get_densidad_mercados()`, `get_densidad_financiero()`, `get_densidad_inmobiliario()`, and `get_densidad_viviendas()`.
  - **Gross Value Added (VAB) Modeling**: Added `get_vab_ccpp()` covering total national population centers as well as metropolitan partitions (`lima`, `callao`).
  - **Territorial Mobility & Commuting Flows**: Added `get_flujos_movilidad()` with specialized shortcuts `get_flujos_estudiantes()` and `get_flujos_trabajadores()` supporting volume thresholds.
  - **Natural Hazards Shortcuts**: Added direct convenience functions `get_puntos_criticos()`, `get_tramos_criticos()`, `get_zonas_criticas()`, `get_areas_inundacion()`, `get_areas_movimientos_masa()`, and `get_inventario_inundaciones()`.
  - **Disaster Risks Shortcuts**: Added direct convenience functions `get_evaluacion_riesgos()` and `get_zonas_riesgos_no_mitigable()`.
  - **Rural Cadastre**: Added `get_catastro_rural()` for rural property parcels and titling polygons.

* **Discovery & Metadata**:
  - Expanded `list_available_layers()` to **35 layers** across 8 thematic domains (`limits`, `transport`, `services`, `demography`, `telecom`, `hazards`, `mobility`, `cadastre`).
  - Comprehensive unit test suite expanded to **159 passing unit tests** with 0 failures and 0 warnings.
  - 100% CRAN check compliance: passes `devtools::check(cran = TRUE, manual = TRUE)` with **0 errors, 0 warnings, 0 notes**.

# rsdot 0.1.0

* Initial release of the `rsdot` package for programmatic access to spatial data from the Secretariat of Demarcation and Territorial Organization (SDOT-PCM) of Peru.
* Added 20 spatial retrieval functions covering 7 thematic domains:
  - **Administrative Limits**: `get_departamentos()`, `get_provincias()`, `get_distritos()`.
  - **Population Centers & Demography**: `get_capitales()`, `get_centros_poblados()`, `get_centros_poblados_crecimiento()`, `get_densidad_poblacional()`.
  - **Transportation Infrastructure**: `get_aerodromos()`, `get_red_vial_nacional()`, `get_red_vial_departamental()`, `get_red_vial_vecinal()`.
  - **Social Infrastructure & Public Services**: `get_ipress()`, `get_locales_educativos()`, `get_servicios_educativos()`.
  - **Telecommunications**: `get_cobertura_movil()` and `get_cobertura_movil_c()`.
  - **Accessibility & Modeling**: `get_tiempos_desplazamiento()`.
  - **Hazards & Risks**: `get_peligros()`, `get_riesgos()`.
* Added discovery and maintenance tools:
  - `list_available_layers()` to inspect the catalog of available layers by thematic domain.
  - `rsdot_check_updates()` to audit local cache status, file sizes, and freshness.
  - `clear_rsdot_cache()` to easily purge downloaded data.
* Implemented persistent user cache with `tools::R_user_dir("rsdot", "cache")`.
* Automated Latin1/UTF-8 encoding correction and Mojibake repair (`fix_encoding_text()`) protecting characters like 'Ñ' and accents.
* Full test suite with 115 passing tests verified for CRAN compliance.
