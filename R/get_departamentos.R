#' Get Departmental Census Limits of Peru
#'
#' Downloads and loads the official departmental limits from INEI (2023) from the
#' DEMARCA OSF repository. Implements persistent caching to optimize repeated
#' downloads and keeps the console clean during execution.
#'
#' @param departamento Character vector. Name(s) of the department(s) to filter.
#'   Can be a single department (e.g., "CUSCO") or multiple departments
#'   (e.g., c("CUSCO", "PUNO")). Case-insensitive.
#'   If \code{NULL} (default), returns all departments of Peru.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative
#'   messages about download progress. If \code{FALSE}, executes silently.
#' @param force_update Logical. If \code{TRUE}, forces a new download of the
#'   file even if it exists in cache. Default \code{FALSE} to leverage cached data.
#'
#' @return An \code{sf} (simple feature) object with the geometry of the requested
#'   departments, including their geographic and administrative attributes.
#'
#' @details
#' The function downloads data directly from OSF (Open Science Framework)
#' and stores it in the user's cache directory (`tools::R_user_dir("rsdot", "cache")`).
#' The data corresponds to the official INEI 2023 census limits.
#'
#' Data source: DEMARCA Repository on OSF (\url{https://osf.io/grhe3/})
#'
#' @examples
#' \dontrun{
#' # Load all departments of Peru
#' peru <- get_departamentos()
#'
#' # Load a specific department (case-insensitive)
#' cusco <- get_departamentos(departamento = "Cusco")
#'
#' # Load multiple departments (southern region)
#' sur <- get_departamentos(
#'   departamento = c("PUNO", "TACNA", "MOQUEGUA", "AREQUIPA")
#' )
#'
#' # Force data update
#' peru_updated <- get_departamentos(force_update = TRUE)
#'
#' # Silent execution
#' lima <- get_departamentos(departamento = "LIMA", show_progress = FALSE)
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#'
#' ggplot(peru) +
#'   geom_sf(fill = "#ff9999", color = "#e60000", linewidth = 1) +
#'   labs(
#'     title = "Departmental Census Limits of Peru",
#'     subtitle = "INEI 2023 Census Limits",
#'     caption = "Source: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#' }
#'
#' @seealso
#' \code{\link[sf]{read_sf}}, \code{\link[sf]{st_geometry}}
#'
#' @references
#' INEI (2023). Departmental Census Limits of Peru.
#' SDOT DEMARCA PERU VISOR Repository: \url{https://osf.io/qy4j6/overview}
#'
#' @export
#' @importFrom sf read_sf
#' @importFrom utils head
get_departamentos <- function(departamento = NULL,
                              show_progress = TRUE,
                              force_update = FALSE) {
  # ==========================================================================
  # 1. PARAMETER VALIDATION
  # ==========================================================================

  if (!is.logical(show_progress) || length(show_progress) != 1) {
    stop("The 'show_progress' parameter must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.logical(force_update) || length(force_update) != 1) {
    stop("The 'force_update' parameter must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.null(departamento) && !is.character(departamento)) {
    stop("The 'departamento' parameter must be a character vector or NULL",
      call. = FALSE
    )
  }

  # ==========================================================================
  # 2. DATA ACQUISITION (GENERIC)
  # ==========================================================================

  catalog <- get_rsdot_catalog("departamentos")

  datos_sf <- download_and_read_spatial_data(
    osf_id = catalog$id_osf,
    file_name = catalog$file_name,
    show_progress = show_progress,
    force_update = force_update,
    pattern_shp = catalog$pattern_shp
  )

  # ==========================================================================
  # 3. FILTER BY DEPARTMENT
  # ==========================================================================

  # If no department is specified, return all
  if (is.null(departamento)) {
    if (show_progress) {
      message("\u2713 Data loaded: ", nrow(datos_sf), " departments")
    }
    return(datos_sf)
  }

  # Check if name column exists
  if (!"nombdep" %in% names(datos_sf)) {
    warning(
      "Column 'nombdep' not found. Returning unfiltered data.",
      call. = FALSE
    )
    return(datos_sf)
  }

  # Normalize department names (uppercase for comparison)
  dep_input <- toupper(trimws(departamento))

  # Filter departments
  datos_sf_filtrado <- datos_sf[toupper(datos_sf$nombdep) %in% dep_input, ]

  # Validate filter results
  if (nrow(datos_sf_filtrado) == 0) {
    departamentos_disponibles <- unique(datos_sf$nombdep)
    warning(
      "No matches found for: ",
      paste(departamento, collapse = ", "), "\n",
      "Available departments:\n  ",
      paste(sort(departamentos_disponibles), collapse = "\n  "),
      call. = FALSE
    )
    return(datos_sf) # Return complete data as fallback
  }

  if (show_progress) {
    message(
      "\u2713 Filtering completed: ",
      nrow(datos_sf_filtrado),
      " department(s)"
    )
  }

  return(datos_sf_filtrado)
}
