#' Get Natural Hazards Spatial Data of Peru
#'
#' Downloads and loads spatial data of natural hazards (floods, mass movements, etc.)
#' prepared by SDOT-PCM. Data includes polygons, lines, or points representing areas
#' of exposure, critical points, and tramos.
#'
#' @param tipo Character. The type of hazard layer to retrieve. Valid options:
#'   \itemize{
#'     \item \code{"areas_exposicion_inundacion"} (or alias \code{"exp_inundacion"})
#'     \item \code{"areas_exposicion_movimientos_masa"} (or alias \code{"exp_mov_masa"})
#'     \item \code{"inventario_inundaciones"}
#'     \item \code{"puntos_criticos_inundaciones"} (or alias \code{"puntos_criticos"})
#'     \item \code{"tramos_criticos_inundaciones"} (or alias \code{"tramos_criticos"})
#'     \item \code{"zonas_criticas_movimientos_masa"} (or alias \code{"zonas_criticas"})
#'   }
#'   Case-insensitive. If \code{NULL}, lists all available types.
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Case-insensitive. If \code{NULL}, lists available departments for the selected type.
#' @param provincia Character vector. Name(s) of the province(s) to filter. Optional.
#' @param distrito Character vector. Name(s) of the district(s) to filter. Optional.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative messages.
#' @param force_update Logical. If \code{TRUE}, forces a new download of files. Default \code{FALSE}.
#' @param ... Additional arguments passed to internal functions or wrapper methods.
#'
#' @return An \code{sf} (simple feature) object representing the requested hazard layer.
#'
#' @details
#' Cache is stored under subdirectories of \code{tools::R_user_dir("rsdot", which = "cache")/}.
#'
#' @examples
#' \dontrun{
#' # List available types
#' get_peligros()
#'
#' # Load critical flood points for Cusco
#' pc_cusco <- get_peligros(tipo = "puntos_criticos", departamento = "CUSCO")
#' }
#'
#' @export
#' @importFrom sf read_sf
get_peligros <- function(tipo = NULL,
                         departamento = NULL,
                         provincia = NULL,
                         distrito = NULL,
                         show_progress = TRUE,
                         force_update = FALSE,
                         ...) {
  tipos_validos <- c(
    "areas_exposicion_inundacion"        = "peligros_areas_exposicion_inundacion",
    "exp_inundacion"                     = "peligros_areas_exposicion_inundacion",
    "areas_exposicion_movimientos_masa"  = "peligros_areas_exposicion_movimientos_masa",
    "exp_mov_masa"                       = "peligros_areas_exposicion_movimientos_masa",
    "inventario_inundaciones"            = "peligros_inventario_inundaciones",
    "puntos_criticos_inundaciones"       = "peligros_puntos_criticos_inundaciones",
    "puntos_criticos"                    = "peligros_puntos_criticos_inundaciones",
    "tramos_criticos_inundaciones"       = "peligros_tramos_criticos_inundaciones",
    "tramos_criticos"                    = "peligros_tramos_criticos_inundaciones",
    "zonas_criticas_movimientos_masa"    = "peligros_zonas_criticas_movimientos_masa",
    "zonas_criticas"                     = "peligros_zonas_criticas_movimientos_masa"
  )

  if (is.null(tipo)) {
    message("Available hazard types (tipo):")
    message(paste0("  - ", sort(unique(names(tipos_validos))), collapse = "\n"))
    return(invisible(unique(names(tipos_validos))))
  }

  tipo_input <- tolower(trimws(tipo))
  if (!tipo_input %in% names(tipos_validos)) {
    stop("Invalid hazard type. Run get_peligros() without arguments to see options.", call. = FALSE)
  }

  dataset_key <- tipos_validos[[tipo_input]]

  load_departmental_spatial_data(
    dataset_key = dataset_key,
    departamento = departamento,
    provincia = provincia,
    distrito = distrito,
    show_progress = show_progress,
    force_update = force_update,
    subfolder = dataset_key,
    item_label = "hazard records",
    usage_fn = paste0("get_peligros(tipo = '", tipo_input, "', departamento = 'CUSCO')"),
    lang = "en"
  )
}

#' @rdname get_peligros
#' @export
get_puntos_criticos <- function(...) {
  get_peligros(tipo = "puntos_criticos", ...)
}

#' @rdname get_peligros
#' @export
get_tramos_criticos <- function(...) {
  get_peligros(tipo = "tramos_criticos", ...)
}

#' @rdname get_peligros
#' @export
get_zonas_criticas <- function(...) {
  get_peligros(tipo = "zonas_criticas", ...)
}

#' @rdname get_peligros
#' @export
get_areas_inundacion <- function(...) {
  get_peligros(tipo = "areas_exposicion_inundacion", ...)
}

#' @rdname get_peligros
#' @export
get_areas_movimientos_masa <- function(...) {
  get_peligros(tipo = "areas_exposicion_movimientos_masa", ...)
}

#' @rdname get_peligros
#' @export
get_inventario_inundaciones <- function(...) {
  get_peligros(tipo = "inventario_inundaciones", ...)
}

