#' Get Educational Venues of Peru
#'
#' Downloads and loads information on educational venues in Peru, prepared by
#' the Ministry of Education (MINEDU). Data includes POINT type geometry and
#' detailed physical and operational attributes for each venue. Updated to 2024.
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
#' @return An \code{sf} (simple feature) object with POINT geometry containing
#'   educational venues information.
#'
#' @details
#' The function downloads data from OSF (Open Science Framework) and stores it
#' in cache: \code{tools::R_user_dir("rsdot", which = "cache")/locales_educativos/}.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_locales_educativos()
#'
#' # Load educational venues for Cusco
#' locales_cusco <- get_locales_educativos(departamento = "CUSCO")
#' }
#'
#' @seealso
#' \code{\link{get_servicios_educativos}}, \code{\link{get_departamentos}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' MINEDU - ESCALE. Padrón de Locales Educativos 2024.
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom sf read_sf
get_locales_educativos <- function(departamento = NULL,
                                   provincia = NULL,
                                   distrito = NULL,
                                   show_progress = TRUE,
                                   force_update = FALSE) {
  load_departmental_spatial_data(
    dataset_key = "locales_educativos",
    departamento = departamento,
    provincia = provincia,
    distrito = distrito,
    show_progress = show_progress,
    force_update = force_update,
    subfolder = "locales_educativos",
    item_label = "venues",
    usage_fn = "get_locales_educativos",
    lang = "en"
  )
}
