#' Get National Road Network of Peru
#'
#' Downloads and loads information on the National Road Network (Red Vial Nacional - RVN)
#' of Peru updated to 2022, prepared by PROVIAS NACIONAL and the Ministry of
#' Transport and Communications (MTC). Data includes MULTILINESTRING geometry
#' and detailed attributes for each road section.
#'
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Valid options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
#'   "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA",
#'   "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
#'   Case-insensitive. If \code{NULL}, shows the list of available departments.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative
#'   messages about download progress. If \code{FALSE}, executes silently.
#' @param force_update Logical. If \code{TRUE}, forces a new download of the
#'   file even if it exists in cache. Default \code{FALSE}.
#'
#' @return An \code{sf} (simple feature) object with MULTILINESTRING geometry
#'   containing national road network information.
#'
#' @details
#' The function downloads data from OSF (Open Science Framework) and stores it
#' in cache: \code{tools::R_user_dir("rsdot", which = "cache")/red_vial_nacional/}.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_red_vial_nacional()
#'
#' # Load road network for Cusco
#' vias_cusco <- get_red_vial_nacional(departamento = "CUSCO")
#' }
#'
#' @seealso
#' \code{\link{get_red_vial_departamental}}, \code{\link{get_departamentos}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' PROVIAS NACIONAL - MTC. Base Cartográfica de la Red Vial Nacional 2022.
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom sf read_sf
get_red_vial_nacional <- function(departamento = NULL,
                                  show_progress = TRUE,
                                  force_update = FALSE) {
  load_departmental_spatial_data(
    dataset_key = "red_vial_nacional",
    departamento = departamento,
    provincia = NULL,
    distrito = NULL,
    show_progress = show_progress,
    force_update = force_update,
    subfolder = "red_vial_nacional",
    item_label = "road sections",
    usage_fn = "get_red_vial_nacional",
    lang = "en"
  )
}
