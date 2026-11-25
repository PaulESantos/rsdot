#' Get Population Centers of Peru
#'
#' Downloads and loads information on population centers of Peru from the
#' 2017 Population and Housing Census of the National Institute of Statistics
#' and Informatics (INEI). Data includes demographic and geographic information
#' at the departmental, provincial, and district levels, with POINT type geometry.
#'
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Valid options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
#'   "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA",
#'   "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
#'   Case-insensitive. If \code{NULL}, shows the list of available departments.
#' @param provincia Character vector. Name(s) of province(s) to filter the
#'   results (optional). It is applied after loading the data.
#' @param distrito Character vector. Name(s) of district(s) to filter the
#'   results (optional). It is applied after loading the data.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative
#'   messages about download progress. If \code{FALSE}, executes silently.
#' @param force_update Logical. If \code{TRUE}, forces a new download of the
#'   file even if it exists in cache. Default \code{FALSE}.
#'
#' @return An \code{sf} (simple feature) object with POINT type geometry
#'   containing population center information.
#'
#' @details
#' La función descarga datos desde OSF (Open Science Framework) y los almacena
#' en caché durante la sesión de R. Los datos están en formato GeoPackage (.gpkg).
#'
#' Cache is stored in: \code{tools::R_user_dir("rsdot", which = "cache")/centros_poblados/}
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_centros_poblados()
#'
#' # Cargar centros poblados de un departamento
#' cp_tacna <- get_centros_poblados(departamento = "TACNA")
#' }
#'
#' @seealso
#' \code{\link{get_capitales}}, \code{\link{get_departamentos}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' INEI. Censo de Población y Vivienda 2017. Instituto Nacional de Estadística e Informática.
#' Visor SDOT: \url{https://geosdot.servicios.gob.pe/visor/}
#'
#' @export
#' @importFrom sf read_sf
get_centros_poblados <- function(departamento = NULL,
                                 provincia = NULL,
                                 distrito = NULL,
                                 show_progress = TRUE,
                                 force_update = FALSE) {
  load_departmental_spatial_data(
    dataset_key = "centros_poblados",
    departamento = departamento,
    provincia = provincia,
    distrito = distrito,
    show_progress = show_progress,
    force_update = force_update,
    subfolder = "centros_poblados",
    item_label = "centros poblados",
    usage_fn = "get_centros_poblados",
    lang = "es"
  )
}
