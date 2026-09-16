#' List Available Layers in GeoSDOT and rsdot
#'
#' Retrieves the complete catalog of spatial layers supported by the \code{rsdot}
#' package, detailing the layer key, thematic domain, source layer in GeoServer,
#' geographic partitioning level, and the recommended R access function.
#'
#' @param domain Character. Optional filter by thematic domain:
#'   \code{"limits"}, \code{"transport"}, \code{"services"}, \code{"demography"},
#'   \code{"telecom"}, \code{"hazards"}, or \code{NULL} (default, returns all).
#' @param show_details Logical. If \code{TRUE}, returns additional technical metadata
#'   such as GeoServer layer names and partition columns. Default is \code{FALSE}.
#'
#' @return A \code{data.frame} containing the catalog of layers.
#'
#' @examples
#' \dontrun{
#' # List all supported layers
#' list_available_layers()
#'
#' # Filter only transportation layers
#' list_available_layers(domain = "transport")
#' }
#'
#' @export
list_available_layers <- function(domain = NULL, show_details = FALSE) {
  # Base catalog definition
  layers_df <- data.frame(
    layer_key = c(
      "departamentos", "provincias", "distritos", "capitales", "centros_poblados",
      "red_vial_nacional", "red_vial_departamental", "red_vial_vecinal", "aerodromos",
      "ipress", "locales_educativos", "servicios_educativos",
      "densidad_poblacional", "centros_poblados_crecimiento", "tiempos_desplazamiento",
      "cobertura_movil", "cobertura_movil_c",
      "peligros_inundacion", "peligros_movimientos_masa", "riesgos_no_mitigable"
    ),
    title = c(
      "Limites Departamentales INEI 2023",
      "Limites Provinciales INEI 2023",
      "Limites Distritales INEI 2023",
      "Capitales por Jerarquia (Censo 2017)",
      "Centros Poblados y Asentamientos Dispersos",
      "Red Vial Nacional (SINAC / MTC)",
      "Red Vial Departamental",
      "Red Vial Vecinal y Rural",
      "Aerodromos y Pistas de Aterrizaje",
      "Establecimientos de Salud (IPRESS - MINSA)",
      "Locales Educativos (MINEDU)",
      "Servicios Educativos por Nivel/Modalidad",
      "Densidad Poblacional Intercensal (2007-2017)",
      "Tasa de Crecimiento de Centros Poblados",
      "Tiempos de Desplazamiento a Capitales",
      "Cobertura Movil por Operador (2G/3G/4G/5G)",
      "Cobertura y Estaciones Base por CCPP",
      "Peligros: Areas de Exposicion a Inundacion",
      "Peligros: Movimientos en Masa y Zonas Criticas",
      "Riesgos: Zonas de Riesgo No Mitigable"
    ),
    domain = c(
      "limits", "limits", "limits", "limits", "limits",
      "transport", "transport", "transport", "transport",
      "services", "services", "services",
      "demography", "demography", "demography",
      "telecom", "telecom",
      "hazards", "hazards", "hazards"
    ),
    domain_label = c(
      "Limites y Territorio", "Limites y Territorio", "Limites y Territorio", "Limites y Territorio", "Limites y Territorio",
      "Transporte y Vialidad", "Transporte y Vialidad", "Transporte y Vialidad", "Transporte y Vialidad",
      "Infraestructura Social", "Infraestructura Social", "Infraestructura Social",
      "Demografia y Accesibilidad", "Demografia y Accesibilidad", "Demografia y Accesibilidad",
      "Telecomunicaciones", "Telecomunicaciones",
      "Peligros y Riesgos", "Peligros y Riesgos", "Peligros y Riesgos"
    ),
    r_function = c(
      "get_departamentos()", "get_provincias()", "get_distritos()", "get_capitales()", "get_centros_poblados()",
      "get_red_vial_nacional()", "get_red_vial_departamental()", "get_red_vial_vecinal()", "get_aerodromos()",
      "get_ipress()", "get_locales_educativos()", "get_servicios_educativos()",
      "get_densidad_poblacional()", "get_centros_poblados_crecimiento()", "get_tiempos_desplazamiento()",
      "get_cobertura_movil()", "get_cobertura_movil_c()",
      "get_peligros()", "get_peligros()", "get_riesgos()"
    ),
    partition = c(
      "Nacional", "Nacional / Dep.", "Nacional / Prov.", "Nacional / Dep.", "Departamental (25)",
      "Nacional / Dep.", "Departamental (25)", "Departamental (25)", "Departamental (25)",
      "Departamental (25)", "Departamental (25)", "Departamental (25)",
      "Departamental (25)", "Departamental (25)", "Departamental (25)",
      "Departamental (25)", "Departamental (25)",
      "Departamental (25)", "Departamental (25)", "Departamental (25)"
    ),
    source_layer = c(
      "geoportal:v_departamentos_2023", "geoportal:v_provincias_2023", "geoportal:v_distritos_2023", "geoportal:v_capitales", "geoportal:ccpp_asentamientos_dispersos",
      "geoportal:v_red_vial_nacional", "geoportal:v_red_vial_departamental", "geoportal:v_red_vial_vecinal", "geoportal:v_aerodromos",
      "geoportal:v_ipress", "geoportal:v_locales_educativos", "geoportal:v_servicios_educativos",
      "geoportal:v_densidad_poblacional", "geoportal:v_ccpp_crecimiento", "geoportal:v_tiempo_desplazamiento",
      "geoportal:cobertura_movil_oper", "geoportal:cobertura_movil_oper",
      "geoportal:v_peligros_inundacion", "geoportal:v_peligros_movimientos_masa", "geoportal:v_zonas_riesgos_no_mitigable"
    ),
    stringsAsFactors = FALSE
  )

  # Filter by domain if specified
  if (!is.null(domain)) {
    dom_clean <- tolower(trimws(domain))
    valid_domains <- unique(layers_df$domain)
    if (!dom_clean %in% valid_domains) {
      stop(
        "Dominio no valido: '", domain, "'.\n",
        "Opciones disponibles: ", paste(valid_domains, collapse = ", "),
        call. = FALSE
      )
    }
    layers_df <- layers_df[layers_df$domain == dom_clean, ]
  }

  if (!show_details) {
    result <- layers_df[, c("layer_key", "title", "domain_label", "r_function", "partition")]
    colnames(result) <- c("Capa", "Descripcion", "Dominio", "Funcion_R", "Particion")
  } else {
    result <- layers_df
  }

  # Print informative header
  message(sprintf("=== Catalogo de Capas Espaciales SDOT (%d capas disponibles) ===", nrow(result)))
  return(result)
}
