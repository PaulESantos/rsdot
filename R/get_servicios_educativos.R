#' Get Educational Services of Peru
#'
#' Downloads and loads information on educational services in Peru, prepared by
#' the Ministry of Education (MINEDU). Data includes POINT type geometry and
#' detailed programmatic, level, and enrollment attributes for each service. Updated to 2024.
#' Allows stepwise filtering by department and province.
#'
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Valid options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
#'   "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA",
#'   "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
#'   Case-insensitive. If \code{NULL}, shows the list of available departments.
#' @param provincia Character vector. Name(s) of the province(s) to filter
#'   within the specified department(s). Optional.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative
#'   messages about download progress. If \code{FALSE}, executes silently.
#' @param force_update Logical. If \code{TRUE}, forces a new download of the
#'   file even if it exists in cache. Default \code{FALSE}.
#'
#' @return An \code{sf} (simple feature) object with POINT geometry containing
#'   educational services information.
#'
#' @details
#' The function downloads data from OSF (Open Science Framework) and stores it
#' in cache: \code{tools::R_user_dir("rsdot", which = "cache")/servicios_educativos/}.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_servicios_educativos()
#'
#' # Load educational services for Cusco
#' serv_cusco <- get_servicios_educativos(departamento = "CUSCO")
#' }
#'
#' @seealso
#' \code{\link{get_locales_educativos}}, \code{\link{get_departamentos}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' MINEDU - ESCALE. Padrón de Instituciones Educativas 2024.
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom sf read_sf
get_servicios_educativos <- function(departamento = NULL,
                                     provincia = NULL,
                                     show_progress = TRUE,
                                     force_update = FALSE) {
  load_departmental_spatial_data(
    dataset_key = "servicios_educativos",
    departamento = departamento,
    provincia = provincia,
    distrito = NULL,
    show_progress = show_progress,
    force_update = force_update,
    subfolder = "servicios_educativos",
    item_label = "educational services",
    usage_fn = "get_servicios_educativos",
    lang = "en"
  )
}
