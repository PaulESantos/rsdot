#' Get Disaster Risk Spatial Data of Peru
#'
#' Downloads and loads spatial data of disaster risk assessments and non-mitigable
#' risk zones prepared by SDOT-PCM and CENEPRED.
#'
#' @param tipo Character. The type of risk layer to retrieve. Valid options:
#'   \itemize{
#'     \item \code{"evaluacion_riesgos"} (or alias \code{"evaluacion"})
#'     \item \code{"zonas_riesgos_no_mitigable"} (or alias \code{"no_mitigable"})
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
#' @return An \code{sf} (simple feature) object representing the requested risk layer.
#'
#' @details
#' Cache is stored under subdirectories of \code{tools::R_user_dir("rsdot", which = "cache")/}.
#'
#' @examples
#' \dontrun{
#' # List available types
#' get_riesgos()
#'
#' # Load non-mitigable risk zones for Cusco
#' r_cusco <- get_riesgos(tipo = "no_mitigable", departamento = "CUSCO")
#' }
#'
#' @export
#' @importFrom sf read_sf
get_riesgos <- function(tipo = NULL,
                        departamento = NULL,
                        provincia = NULL,
                        distrito = NULL,
                        show_progress = TRUE,
                        force_update = FALSE,
                        ...) {
  tipos_validos <- c(
    "evaluacion_riesgos"          = "riesgos_evaluacion_riesgos",
    "evaluacion"                  = "riesgos_evaluacion_riesgos",
    "zonas_riesgos_no_mitigable"  = "riesgos_zonas_riesgos_no_mitigable",
    "no_mitigable"                = "riesgos_zonas_riesgos_no_mitigable"
  )

  if (is.null(tipo)) {
    message("Available risk types (tipo):")
    message(paste0("  - ", sort(unique(names(tipos_validos))), collapse = "\n"))
    return(invisible(unique(names(tipos_validos))))
  }

  tipo_input <- tolower(trimws(tipo))
  if (!tipo_input %in% names(tipos_validos)) {
    stop("Invalid risk type. Run get_riesgos() without arguments to see options.", call. = FALSE)
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
    item_label = "risk records",
    usage_fn = paste0("get_riesgos(tipo = '", tipo_input, "', departamento = 'CUSCO')"),
    lang = "en"
  )
}

#' @rdname get_riesgos
#' @export
get_evaluacion_riesgos <- function(...) {
  get_riesgos(tipo = "evaluacion_riesgos", ...)
}

#' @rdname get_riesgos
#' @export
get_zonas_riesgos_no_mitigable <- function(...) {
  get_riesgos(tipo = "zonas_riesgos_no_mitigable", ...)
}

