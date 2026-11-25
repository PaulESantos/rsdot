#' Get Population Centers with Population Growth Rate
#'
#' Downloads and loads information on population centers with their average annual
#' intercensal population growth rate (2007-2017) from the DEMARCA OSF repository.
#' Data includes POINT type geometry and allows identifying changes in population
#' volume for each population center. Allows stepwise filtering by department,
#' province, and district.
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
#' @return An \code{sf} (simple feature) object with POINT type geometry containing
#'   information on population centers and their growth rate.
#'
#' @details
#' The function downloads data from OSF (Open Science Framework) and stores it
#' in cache: \code{tools::R_user_dir("rsdot", which = "cache")/densidad_crecimiento_pob_ccpp/}.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_centros_poblados_crecimiento()
#'
#' # Load population growth rate for Cusco
#' ccpp_cusco <- get_centros_poblados_crecimiento(departamento = "CUSCO")
#' }
#'
#' @seealso
#' \code{\link{get_centros_poblados}}, \code{\link{get_densidad_poblacional}},
#' \code{\link{get_departamentos}}, \code{\link[sf]{read_sf}}
#'
#' @references
#' INEI. National Population and Housing Censuses 2007 and 2017.
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom sf read_sf
get_centros_poblados_crecimiento <- function(departamento = NULL,
                                            provincia = NULL,
                                            distrito = NULL,
                                            show_progress = TRUE,
                                            force_update = FALSE) {
  load_departmental_spatial_data(
    dataset_key = "centros_poblados_crecimiento",
    departamento = departamento,
    provincia = provincia,
    distrito = distrito,
    show_progress = show_progress,
    force_update = force_update,
    subfolder = "densidad_crecimiento_pob_ccpp",
    item_label = "population centers",
    usage_fn = "get_centros_poblados_crecimiento",
    lang = "en"
  )
}
