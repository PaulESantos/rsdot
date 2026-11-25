#' Get Economic Activity and Housing Density Models of Peru
#'
#' Downloads and loads spatial density models of commercial, market, financial,
#' real estate, and housing activities developed for territorial modeling
#' by SDOT-PCM. Data includes polygon/grid geometry with standardized density indicators.
#'
#' @param tipo Character. Activity density model to retrieve. Valid options:
#'   \itemize{
#'     \item \code{"comercio"}: Density of retail and wholesale commerce.
#'     \item \code{"mercado"} (or \code{"mercados"}): Density of municipal/neighborhood supply markets.
#'     \item \code{"financiero"}: Density of banking and financial services.
#'     \item \code{"inmobiliario"}: Density of real estate activity and developments.
#'     \item \code{"viviendas"}: Density of residential housing units.
#'   }
#'   Case-insensitive. If \code{NULL}, lists available options.
#' @param departamento Character vector. Name(s) of the department(s) to filter. Optional.
#' @param provincia Character vector. Name(s) of the province(s) to filter. Optional.
#' @param distrito Character vector. Name(s) of the district(s) to filter. Optional.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative messages.
#' @param force_update Logical. If \code{TRUE}, forces a new download of files. Default \code{FALSE}.
#' @param ... Additional arguments passed to internal filtering functions.
#'
#' @return An \code{sf} (simple feature) object containing the requested density model.
#'
#' @details
#' Cache is stored under \code{tools::R_user_dir("rsdot", which = "cache")/densidad_actividades/}.
#'
#' @examples
#' \dontrun{
#' # List available types
#' get_densidad_actividades()
#'
#' # Load commerce density for Cusco
#' dens_com <- get_densidad_comercio(departamento = "CUSCO")
#'
#' # Load financial services density
#' dens_fin <- get_densidad_financiero(departamento = "LIMA")
#' }
#'
#' @seealso
#' \code{\link{get_densidad_poblacional}}, \code{\link{get_distritos}}
#'
#' @export
#' @importFrom sf read_sf
get_densidad_actividades <- function(tipo = NULL,
                                     departamento = NULL,
                                     provincia = NULL,
                                     distrito = NULL,
                                     show_progress = TRUE,
                                     force_update = FALSE,
                                     ...) {
  tipos_validos <- c(
    "comercio"     = "densidad_comercio",
    "mercado"      = "densidad_mercados",
    "mercados"     = "densidad_mercados",
    "financiero"   = "densidad_financiero",
    "inmobiliario" = "densidad_inmobiliario",
    "viviendas"    = "densidad_viviendas"
  )

  if (is.null(tipo)) {
    message("Available density models (tipo):")
    message(paste0("  - ", sort(unique(names(tipos_validos))), collapse = "\n"))
    return(invisible(unique(names(tipos_validos))))
  }

  tipo_input <- tolower(trimws(tipo))
  if (!tipo_input %in% names(tipos_validos)) {
    stop(
      "Invalid density type: '", tipo, "'.\n",
      "Run get_densidad_actividades() without arguments to see available options.",
      call. = FALSE
    )
  }

  dataset_key <- tipos_validos[[tipo_input]]
  catalog <- get_rsdot_catalog(dataset_key)

  datos_sf <- download_and_read_spatial_data(
    osf_id = catalog$id_osf,
    file_name = catalog$file_name,
    show_progress = show_progress,
    force_update = force_update
  )

  datos_sf <- filter_spatial_admin(
    datos = datos_sf,
    departamento = departamento,
    provincia = provincia,
    distrito = distrito,
    show_progress = show_progress,
    item_label = "density cells"
  )

  return(datos_sf)
}

#' @rdname get_densidad_actividades
#' @export
get_densidad_comercio <- function(...) {
  get_densidad_actividades(tipo = "comercio", ...)
}

#' @rdname get_densidad_actividades
#' @export
get_densidad_mercados <- function(...) {
  get_densidad_actividades(tipo = "mercados", ...)
}

#' @rdname get_densidad_actividades
#' @export
get_densidad_financiero <- function(...) {
  get_densidad_actividades(tipo = "financiero", ...)
}

#' @rdname get_densidad_actividades
#' @export
get_densidad_inmobiliario <- function(...) {
  get_densidad_actividades(tipo = "inmobiliario", ...)
}

#' @rdname get_densidad_actividades
#' @export
get_densidad_viviendas <- function(...) {
  get_densidad_actividades(tipo = "viviendas", ...)
}
