#' Get Intercensal Population Density and Growth of Peru
#'
#' Downloads and loads information on intercensal population density and growth
#' (2007-2017) of Peru, prepared by INEI and distributed through the SDOT Viewer.
#' Data includes MULTIPOLYGON geometry at the block or census area level with
#' indicators of density, population change, and growth categories.
#' Allows stepwise filtering by department, province, and district.
#'
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Valid options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
#'   "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA",
#'   "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
#'   Case-insensitive. If \code{NULL}, shows the list of available departments.
#' @param provincia Character vector. Name(s) of the province(s) to filter
#'   within the specified department(s). Optional.
#' @param distrito Character vector. Name(s) of the district(s) to filter
#'   within the specified province(s). Optional.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative
#'   messages about download progress. If \code{FALSE}, executes silently.
#' @param force_update Logical. If \code{TRUE}, forces a new download of the
#'   file even if it exists in cache. Default \code{FALSE}.
#'
#' @return An \code{sf} (simple feature) object with MULTIPOLYGON geometry containing
#'   population density and growth attributes.
#'
#' @details
#' The function downloads data from OSF (Open Science Framework) and stores it
#' in cache: \code{tools::R_user_dir("rsdot", which = "cache")/densidad_poblacional/}.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_densidad_poblacional()
#'
#' # Load population density for Cusco
#' dens_cusco <- get_densidad_poblacional(departamento = "CUSCO")
#' }
#'
#' @seealso
#' \code{\link{get_centros_poblados_crecimiento}}, \code{\link{get_departamentos}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' INEI. National Population and Housing Censuses 2007 and 2017.
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom sf read_sf
get_densidad_poblacional <- function(departamento = NULL,
                                     provincia = NULL,
                                     distrito = NULL,
                                     show_progress = TRUE,
                                     force_update = FALSE) {
  load_departmental_spatial_data(
    dataset_key = "densidad_poblacional",
    departamento = departamento,
    provincia = provincia,
    distrito = distrito,
    show_progress = show_progress,
    force_update = force_update,
    subfolder = "densidad_poblacional",
    item_label = "features",
    usage_fn = "get_densidad_poblacional",
    lang = "en"
  )
}
