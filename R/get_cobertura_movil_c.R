#' Get Mobile Coverage by Population Center
#'
#' Downloads and loads information on mobile coverage by population center
#' from the DEMARCA OSF repository. Data includes binary information
#' (YES/NO) on the existence of 2G, 3G, 4G, and 5G coverage for each population
#' center according to the SDOT Viewer.
#'
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Valid options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
#'   "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA",
#'   "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
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
#'   mobile coverage information.
#'
#' @details
#' La función descarga datos desde OSF (Open Science Framework) y los almacena
#' en caché: \code{tools::R_user_dir("rsdot", which = "cache")/cobertura_movil/}.
#' Los datos están en formato GeoPackage (.gpkg).
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_cobertura_movil()
#'
#' # Cargar cobertura móvil de Cusco
#' cob_cusco <- get_cobertura_movil(departamento = "CUSCO")
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link{get_distritos}}, \code{\link{get_centros_poblados}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' Ministerio de Transportes y Comunicaciones (MTC) / OSIPTEL.
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom sf read_sf
get_cobertura_movil_c <- function(departamento = NULL,
                                  provincia = NULL,
                                  distrito = NULL,
                                  show_progress = TRUE,
                                  force_update = FALSE) {
  load_departmental_spatial_data(
    dataset_key = "cobertura_movil",
    departamento = departamento,
    provincia = provincia,
    distrito = distrito,
    show_progress = show_progress,
    force_update = force_update,
    subfolder = "cobertura_movil",
    item_label = "centros poblados",
    usage_fn = "get_cobertura_movil",
    lang = "es"
  )
}

#' @rdname get_cobertura_movil_c
#' @export
get_cobertura_movil <- get_cobertura_movil_c
