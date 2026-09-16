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
