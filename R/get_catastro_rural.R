#' Get Rural Cadastre Spatial Data of Peru
#'
#' Downloads and loads spatial cadastral parcels and property polygons from the
#' official Rural Cadastre (Catastro Rural) of the Ministry of Agrarian Development
#' and Irrigation (MIDAGRI), integrated into the SDOT platform.
#'
#' @param departamento Character vector. Name(s) of department(s) to filter. Optional.
#' @param provincia Character vector. Name(s) of province(s) to filter. Optional.
#' @param distrito Character vector. Name(s) of district(s) to filter. Optional.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative messages.
#' @param force_update Logical. If \code{TRUE}, forces a new download of files. Default \code{FALSE}.
#'
#' @return An \code{sf} (simple feature) object containing cadastral parcels.
#'
#' @details
#' Cache is stored under \code{tools::R_user_dir("rsdot", which = "cache")/catastro_rural/}.
#'
#' @examples
#' \dontrun{
#' # Load rural cadastre for a department
#' catastro_cusco <- get_catastro_rural(departamento = "CUSCO")
#' }
#'
#' @seealso
#' \code{\link{get_distritos}}, \code{\link{get_departamentos}}
#'
#' @references
#' MIDAGRI. Sistema Catastral Rural de Predios Individuales y Comunales.
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom sf read_sf
get_catastro_rural <- function(departamento = NULL,
                               provincia = NULL,
                               distrito = NULL,
                               show_progress = TRUE,
                               force_update = FALSE) {
  catalog <- get_rsdot_catalog("catastro_rural")

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
    item_label = "cadastral parcels"
  )

  return(datos_sf)
}
