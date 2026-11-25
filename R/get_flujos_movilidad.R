#' Get Territorial Mobility and Commuting Flows of Peru
#'
#' Downloads and loads spatial flow lines of inter-district commuting and mobility
#' patterns for students and workers in Peru, modeled by SDOT-PCM. Available
#' across multiple flow volume brackets (e.g. 100-1000, 1000-3500, etc.).
#'
#' @param tipo Character. Type of mobility flow:
#'   \itemize{
#'     \item \code{"estudiantes"} (or \code{"est"}): Student commuting flows.
#'     \item \code{"trabajadores"} (or \code{"trb"}): Worker commuting flows.
#'   }
#'   Case-insensitive. If \code{NULL}, lists available types and volume intervals.
#' @param volumen Character or numeric. Volume interval bracket:
#'   \itemize{
#'     \item For students: \code{"100_1000"} (default), \code{"1000_3500"}, \code{"3500_7000"},
#'       \code{"7000_13000"}, \code{"13000"}.
#'     \item For workers: \code{"100_1500"} (default), \code{"1500_5000"}, \code{"5000_10000"},
#'       \code{"10000_25000"}, \code{"25000"}.
#'   }
#' @param departamento Character vector. Name(s) of department(s) to filter. Optional.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative messages.
#' @param force_update Logical. If \code{TRUE}, forces a new download of files. Default \code{FALSE}.
#' @param ... Additional arguments passed to internal filtering functions.
#'
#' @return An \code{sf} (simple feature) object with LINESTRING geometries representing mobility vectors.
#'
#' @details
#' Cache is stored under \code{tools::R_user_dir("rsdot", which = "cache")/flujos_movilidad/}.
#'
#' @examples
#' \dontrun{
#' # List available options
#' get_flujos_movilidad()
#'
#' # Load student flows between 100 and 1,000 trips
#' flujos_est <- get_flujos_estudiantes(volumen = "100_1000")
#'
#' # Load worker flows
#' flujos_trb <- get_flujos_trabajadores(volumen = "100_1500")
#' }
#'
#' @seealso
#' \code{\link{get_red_vial_nacional}}, \code{\link{get_tiempos_desplazamiento}}
#'
#' @export
#' @importFrom sf read_sf
get_flujos_movilidad <- function(tipo = NULL,
                                 volumen = NULL,
                                 departamento = NULL,
                                 show_progress = TRUE,
                                 force_update = FALSE,
                                 ...) {
  volumenes_est <- c("100_1000", "1000_3500", "3500_7000", "7000_13000", "13000")
  volumenes_trb <- c("100_1500", "1500_5000", "5000_10000", "10000_25000", "25000")

  if (is.null(tipo)) {
    message("Available mobility flow types (tipo):")
    message("  - 'estudiantes' (Brackets: ", paste(volumenes_est, collapse = ", "), ")")
    message("  - 'trabajadores' (Brackets: ", paste(volumenes_trb, collapse = ", "), ")")
    return(invisible(c("estudiantes", "trabajadores")))
  }

  tipo_clean <- tolower(trimws(tipo))
  if (tipo_clean %in% c("estudiantes", "estudiante", "est")) {
    vol_sel <- if (is.null(volumen)) "100_1000" else as.character(volumen)
    if (!vol_sel %in% volumenes_est) {
      stop(
        "Invalid volume bracket '", volumen, "' for students.\n",
        "Valid options: ", paste(volumenes_est, collapse = ", "),
        call. = FALSE
      )
    }
    dataset_key <- paste0("flujo_est_", vol_sel)
  } else if (tipo_clean %in% c("trabajadores", "trabajador", "trb")) {
    vol_sel <- if (is.null(volumen)) "100_1500" else as.character(volumen)
    if (!vol_sel %in% volumenes_trb) {
      stop(
        "Invalid volume bracket '", volumen, "' for workers.\n",
        "Valid options: ", paste(volumenes_trb, collapse = ", "),
        call. = FALSE
      )
    }
    dataset_key <- paste0("flujo_trb_", vol_sel)
  } else {
    stop("Invalid flow type: '", tipo, "'. Use 'estudiantes' or 'trabajadores'.", call. = FALSE)
  }

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
    show_progress = show_progress,
    item_label = "flow lines"
  )

  return(datos_sf)
}

#' @rdname get_flujos_movilidad
#' @export
get_flujos_estudiantes <- function(volumen = "100_1000", ...) {
  get_flujos_movilidad(tipo = "estudiantes", volumen = volumen, ...)
}

#' @rdname get_flujos_movilidad
#' @export
get_flujos_trabajadores <- function(volumen = "100_1500", ...) {
  get_flujos_movilidad(tipo = "trabajadores", volumen = volumen, ...)
}
