#' Get Provincial Census Limits of Peru
#'
#' Downloads and loads the official provincial limits from INEI (2023) from the
#' DEMARCA OSF repository. Implements persistent caching to optimize repeated
#' downloads and keeps the console clean during execution.
#'
#' @param provincia Character vector. Name(s) of the province(s) to filter.
#'   Can be a single province (e.g., "URUBAMBA") or multiple provinces
#'   (e.g., c("CAÑETE", "CHINCHA")). Case-insensitive.
#'   If \code{NULL}, no filtering is performed by province.
#' @param departamento Character vector. Name of the department to filter
#'   all its provinces (e.g., "CUSCO"). If specified, it returns all the
#'   provinces of the indicated department. Case-insensitive.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative
#'   messages about download progress. If \code{FALSE}, executes silently.
#' @param force_update Logical. If \code{TRUE}, forces a new download of the
#'   file even if it exists in cache. Default \code{FALSE} to leverage cached data.
#'
#' @return An \code{sf} (simple feature) object with the geometry of the requested
#'   provinces, including their geographic and administrative attributes.
#'
#' @details
#' The function downloads data directly from OSF (Open Science Framework)
#' and stores it in the user's cache directory (`tools::R_user_dir("rsdot", "cache")`).
#' The data corresponds to the official INEI 2023 census limits.
#'
#' The function automatically corrects character encoding issues
#' (especially Ñ and accents) in the original data.
#'
#' Data source: DEMARCA Repository on OSF (\url{https://osf.io/t36aj/})
#'
#' @examples
#' \dontrun{
#' # Load all provinces of Peru
#' provincias_peru <- get_provincias()
#'
#' # Load a specific province
#' urubamba <- get_provincias(provincia = "Urubamba")
#'
#' # Load all provinces of a department
#' provincias_cusco <- get_provincias(departamento = "Cusco")
#'
#' # Load multiple specific provinces
#' sur_chico <- get_provincias(
#'   provincia = c("CAÑETE", "CHINCHA", "PISCO")
#' )
#'
#' # Combine filters: specific provinces of a department
#' costa_lima <- get_provincias(
#'   provincia = c("CAÑETE", "YAUYOS"),
#'   departamento = "LIMA"
#' )
#'
#' # Force data update
#' provincias_updated <- get_provincias(force_update = TRUE)
#'
#' # Silent execution
#' pasco <- get_provincias(departamento = "PASCO", show_progress = FALSE)
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#'
#' # Map of all provinces in Cusco
#' ggplot(provincias_cusco) +
#'   geom_sf(aes(fill = nombprov), color = "white", linewidth = 0.3) +
#'   labs(
#'     title = "Provinces of the Department of Cusco",
#'     subtitle = "INEI 2023 Census Limits",
#'     fill = "Province",
#'     caption = "Source: INEI | Visor - SDOT"
#'   ) +
#'   scale_fill_viridis_d(option = "plasma") +
#'   theme_minimal() +
#'   theme(legend.position = "right")
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link[sf]{read_sf}},
#' \code{\link[sf]{st_geometry}}
#'
#' @references
#' INEI (2023). Provincial Census Limits of Peru.
#' DEMARCA Repository: \url{https://osf.io/t36aj/}
#'
#' @export
#' @importFrom sf read_sf
#' @importFrom utils head
get_provincias <- function(provincia = NULL,
                           departamento = NULL,
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

  if (!is.null(provincia) && !is.character(provincia)) {
    stop("The 'provincia' parameter must be a character vector or NULL", call. = FALSE)
  }

  if (!is.null(departamento) && !is.character(departamento)) {
    stop("The 'departamento' parameter must be a character vector or NULL", call. = FALSE)
  }

  # ==========================================================================
  # 2. DATA ACQUISITION (GENERIC)
  # ==========================================================================

  catalog <- get_rsdot_catalog("provincias")

  datos_sf <- download_and_read_spatial_data(
    osf_id = catalog$id_osf,
    file_name = catalog$file_name,
    show_progress = show_progress,
    force_update = force_update,
    pattern_shp = catalog$pattern_shp
  )

  # ==========================================================================
  # 3. FILTER BY DEPARTMENT AND PROVINCE
  # ==========================================================================

  # Filter by DEPARTMENT
  if (!is.null(departamento)) {
    if ("nombdep" %in% names(datos_sf)) {
      dep_input <- toupper(trimws(departamento))
      datos_sf <- datos_sf[toupper(datos_sf$nombdep) %in% dep_input, ]

      if (nrow(datos_sf) == 0) {
        warning(
          "No provinces found for department: ",
          paste(departamento, collapse = ", "),
          call. = FALSE
        )
        return(empty_sf())
      }

      if (show_progress) {
        message(
          "\u2713 Filtering by department: ",
          nrow(datos_sf),
          " province(s) in ",
          paste(departamento, collapse = ", ")
        )
      }
    } else {
      warning(
        "Column 'nombdep' not found for department filtering.",
        call. = FALSE
      )
    }
  }

  # Filter by PROVINCE
  if (!is.null(provincia)) {
    if ("nombprov" %in% names(datos_sf)) {
      prov_input <- toupper(trimws(provincia))
      datos_sf_filtrado <- datos_sf[toupper(datos_sf$nombprov) %in% prov_input, ]

      if (nrow(datos_sf_filtrado) == 0) {
        provincias_disponibles <- unique(datos_sf$nombprov)
        warning(
          "No matches found for: ",
          paste(provincia, collapse = ", "), "\n",
          "Available provinces",
          if (!is.null(departamento)) paste0(" in ", departamento) else "",
          " (first 20):\n  ",
          paste(head(sort(provincias_disponibles), 20), collapse = "\n  "),
          call. = FALSE
        )
        return(datos_sf) # Returns original filtered by dept (or all) if province fails
      }

      datos_sf <- datos_sf_filtrado
    } else {
      warning(
        "Column 'nombprov' not found for province filtering.",
        call. = FALSE
      )
    }
  }

  if (show_progress) {
    message(
      "\u2713 Data loaded: ",
      nrow(datos_sf),
      " province(s)"
    )
  }

  return(datos_sf)
}
