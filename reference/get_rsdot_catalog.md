# Internal Catalog Lookup

Internal Catalog Lookup

## Usage

``` r
get_rsdot_catalog(dataset)
```

## Arguments

- dataset:

  Character. Dataset name (e.g. "departamentos", "provincias",
  "distritos", "distritos_2025", "aerodromos", "densidad_poblacional",
  "capitales", "centros_poblados", "red_vial_nacional",
  "red_vial_departamental", "red_vial_vecinal", "ipress",
  "locales_educativos", "servicios_educativos", "cobertura_movil",
  "centros_poblados_crecimiento", "tiempo_desplazamiento",
  "peligros\_*", "riesgos\_*", "densidad\_*", "vab_ccpp\_*",
  "flujos\_\*", "catastro_rural").

## Value

A data.frame with columns `departamento` (optional), `file_name`,
`id_osf`, `pattern_shp` (optional).
