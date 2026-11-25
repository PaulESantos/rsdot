#' Get Departmental Road Network of Peru
#'
#' Downloads and loads information on the Departmental Road Network updated by
#' PROVIAS NACIONAL as of July 2022, according to the Route Classifier approved
#' by Supreme Decree 011-2016-MTC and its amendments. Data includes
#' MULTILINESTRING type geometry and information on trajectories, status, and
#' surface of departmental roads. Allows stepwise filtering by department and
#' province.
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
#' @return An \code{sf} (simple feature) object with MULTILINESTRING geometry
#'   containing departmental road network information.
#'
#' @details
#' The function downloads data from OSF (Open Science Framework) and stores it
#' in cache: \code{tools::R_user_dir("rsdot", which = "cache")/red_vial_departamental/}.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_red_vial_departamental()
#'
#' # Load road network for Cusco
#' vias_cusco <- get_red_vial_departamental(departamento = "CUSCO")
#' }
#'
#' @seealso
#' \code{\link{get_red_vial_nacional}}, \code{\link{get_departamentos}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' PROVIAS NACIONAL - MTC. Base Cartográfica de la Red Vial Departamental 2022.
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom sf read_sf
get_red_vial_departamental <- function(departamento = NULL,
                                       provincia = NULL,
                                       show_progress = TRUE,
                                       force_update = FALSE) {
  load_departmental_spatial_data(
    dataset_key = "red_vial_departamental",
    departamento = departamento,
    provincia = provincia,
    distrito = NULL,
    show_progress = show_progress,
    force_update = force_update,
    subfolder = "red_vial_departamental",
    item_label = "road sections",
    usage_fn = "get_red_vial_departamental",
    lang = "en"
  )
}
