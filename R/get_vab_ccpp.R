#' Get Gross Added Value (VAB) of Population Centers
#'
#' Downloads and loads the socioeconomic model of Gross Added Value (VAB - Valor Agregado Bruto)
#' estimated at a 150-meter buffer around population centers (CCPP) in Peru, prepared
#' by SDOT-PCM. Available at national coverage or focused metropolitan domains (Lima, Callao).
#'
#' @param cobertura Character. Geographic scope to retrieve:
#'   \itemize{
#'     \item \code{"total"} (or \code{"nacional"}): Complete national model.
#'     \item \code{"lima"}: Metropolitan Lima model.
#'     \item \code{"callao"}: Callao region model.
#'   }
#'   Default is \code{"total"}. Case-insensitive.
#' @param departamento Character vector. Name(s) of department(s) to filter. Optional.
#' @param provincia Character vector. Name(s) of province(s) to filter. Optional.
#' @param distrito Character vector. Name(s) of district(s) to filter. Optional.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative messages.
#' @param force_update Logical. If \code{TRUE}, forces a new download of files. Default \code{FALSE}.
#'
#' @return An \code{sf} (simple feature) object containing population centers with estimated VAB attributes.
#'
#' @details
#' Cache is stored under \code{tools::R_user_dir("rsdot", which = "cache")/vab_ccpp/}.
#'
#' @examples
#' \dontrun{
#' # Load national VAB model
#' vab_nac <- get_vab_ccpp()
#'
#' # Load VAB model for Lima
#' vab_lima <- get_vab_ccpp(cobertura = "lima")
#' }
#'
#' @seealso
#' \code{\link{get_centros_poblados}}, \code{\link{get_densidad_actividades}}
#'
#' @export
#' @importFrom sf read_sf
get_vab_ccpp <- function(cobertura = "total",
                         departamento = NULL,
                         provincia = NULL,
                         distrito = NULL,
                         show_progress = TRUE,
                         force_update = FALSE) {
  cob_clean <- tolower(trimws(cobertura))

  valid_options <- c(
    "total"    = "vab_ccpp_total",
    "nacional" = "vab_ccpp_total",
    "lima"     = "vab_ccpp_lima",
    "callao"   = "vab_ccpp_callao"
  )

  if (!cob_clean %in% names(valid_options)) {
    stop(
      "Invalid 'cobertura': '", cobertura, "'.\n",
      "Valid options are: 'total', 'lima', 'callao'.",
      call. = FALSE
    )
  }

  dataset_key <- valid_options[[cob_clean]]
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
    item_label = "population centers with VAB"
  )

  return(datos_sf)
}
