#' Get District Capitals of Peru
#'
#' Downloads and loads information on district capitals identified in the
#' 2017 Population and Housing Census of the National Institute of Statistics and
#' Informatics (INEI). Data includes demographic and geographic information of
#' the capitals at the departmental, provincial, and district levels, with POINT type
#' geometry.
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
#'   containing district capital information, including:
#'   \itemize{
#'     \item POINT geometry (location of the capital)
#'     \item \code{gid}: Unique record identifier
#'     \item \code{ubigeo}: Geographic location code (6 digits)
#'     \item \code{nombdep}: Department name
#'     \item \code{nombprov}: Province name
#'     \item \code{nombdist}: District name
#'     \item \code{codccpp}: Population center code (10 digits)
#'     \item \code{cen_pob}: Population center name
#'     \item \code{pob}: Total population
#'     \item \code{capital}: Capital type (1=departmental, 2=provincial, 3=distrital)
#'   }
#'
#'   If multiple departments are requested, it returns a combined sf object.
#'
#' @details
#' The function downloads data from OSF (Open Science Framework) and stores it
#' in cache during the R session. Data is in GeoPackage (.gpkg) format.
#'
#' Cache is stored in: \code{tools::R_user_dir("rsdot", which = "cache")/capitales/}
#'
#' **NOTE:** Geometries are POINT type and represent the approximate geographic
#' location of the capitals according to the 2017 Census. Population data
#' correspond to the census year.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_capitales()
#'
#' # Load capitals of a department
#' capitales_cusco <- get_capitales(departamento = "CUSCO")
#'
#' # Load multiple departments
#' capitales_sur <- get_capitales(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Filter by province
#' capitales_lima_prov <- get_capitales(
#'   departamento = "LIMA",
#'   provincia = "LIMA"
#' )
#'
#' # Filter by district
#' capital_cusco_dist <- get_capitales(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "CUSCO"
#' )
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Map of capitals by hierarchy
#' ggplot(capitales_cusco) +
#'   geom_sf(aes(color = factor(capital), size = pob), alpha = 0.7) +
#'   scale_color_manual(
#'     values = c("1" = "red", "2" = "blue", "3" = "green"),
#'     labels = c("Departmental", "Provincial", "Distrital"),
#'     name = "Capital Type"
#'   ) +
#'   scale_size_continuous(range = c(1, 10), name = "Population") +
#'   labs(
#'     title = "Capitals of Cusco by Hierarchy",
#'     subtitle = "2017 Population and Housing Census",
#'     caption = "Source: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#' }
#'
#' @seealso
#' \code{\link{get_centros_poblados}}, \code{\link{get_departamentos}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' INEI. 2017 Population and Housing Census. National Institute of Statistics
#' and Informatics.
#'
#' SDOT Viewer: \url{https://geosdot.servicios.gob.pe/visor/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf st_sf st_sfc st_drop_geometry
#' @importFrom utils head
get_capitales <- function(departamento = NULL,
                          provincia = NULL,
                          distrito = NULL,
                          show_progress = TRUE,
                          force_update = FALSE) {
  load_departmental_spatial_data(
    dataset_key = "capitales",
    departamento = departamento,
    provincia = provincia,
    distrito = distrito,
    show_progress = show_progress,
    force_update = force_update,
    subfolder = "capitales",
    item_label = "capitales",
    usage_fn = "get_capitales",
    lang = "es"
  )
}
