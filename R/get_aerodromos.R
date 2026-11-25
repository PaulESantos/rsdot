#' Get Airfields of Peru
#'
#' Downloads and loads information on airfields in Peru updated to the year 2022,
#' prepared by the Ministry of Transport and Communications (MTC). Data includes
#' POINT type geometry and information on airports, airfields, and heliports.
#' Allows stepwise filtering by department, province, and district.
#'
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Valid options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CUSCO", "HUANUCO", "ICA", "JUNIN", "LA LIBERTAD", "LAMBAYEQUE",
#'   "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO",
#'   "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
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
#'   airfield information, including:
#'   \itemize{
#'     \item POINT geometry (airfield coordinates)
#'     \item \code{nombre}: Airfield name
#'     \item \code{label}: Full descriptive label
#'     \item \code{tipo}: Type of facility (INTERNATIONAL AIRPORT, AIRFIELD, HELIPORT)
#'     \item \code{codidep}: Department code
#'     \item \code{nombdep}: Department name
#'     \item \code{nombprov}: Province name
#'     \item \code{nombdist}: District name
#'     \item \code{escala}: Airfield scale
#'     \item \code{lat}: Latitude
#'     \item \code{lon}: Longitude
#'     \item \code{estado}: Operating status (OPERATIONAL, etc.)
#'     \item \code{administ}: Managing entity
#'     \item \code{jerarquia}: Hierarchy (NATIONAL, etc.)
#'     \item \code{titular}: Ownership type (PUBLIC, PRIVATE)
#'   }
#'
#'   If multiple departments are requested, it returns a combined sf object.
#'
#' @details
#' The function downloads data from OSF (Open Science Framework) and stores it
#' in cache during the R session. Data is in GeoPackage (.gpkg) format.
#'
#' Cache is stored in: \code{tools::R_user_dir("rsdot", which = "cache")/aerodromos/}
#'
#' **NOTE:** Geometries are POINT type and represent the location of each airfield.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_aerodromos()
#'
#' # Load airfields of a full department
#' aero_cusco <- get_aerodromos(departamento = "CUSCO")
#'
#' # Filter by specific province
#' aero_prov_cusco <- get_aerodromos(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link{get_distritos}}, \code{\link[sf]{read_sf}}
#'
#' @references
#' Ministry of Transport and Communications (MTC). Airfields of Peru 2022.
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom sf read_sf
get_aerodromos <- function(departamento = NULL,
                           provincia = NULL,
                           distrito = NULL,
                           show_progress = TRUE,
                           force_update = FALSE) {
  load_departmental_spatial_data(
    dataset_key = "aerodromos",
    departamento = departamento,
    provincia = provincia,
    distrito = distrito,
    show_progress = show_progress,
    force_update = force_update,
    subfolder = "aerodromos",
    item_label = "airfields",
    usage_fn = "get_aerodromos",
    lang = "en"
  )
}
