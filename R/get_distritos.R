#' Get District Census Limits of Peru
#'
#' Downloads and loads the official district limits from INEI (2023 or 2025 update)
#' from the DEMARCA OSF repository. Implements persistent caching to optimize repeated
#' downloads and keeps the console clean during execution.
#'
#' @param distrito Character vector. Name(s) of the district(s) to filter.
#'   Can be a single district (e.g., "WANCHAQ") or multiple districts
#'   (e.g., c("CUSCO", "WANCHAQ", "SANTIAGO")). Case-insensitive.
#'   If \code{NULL}, no filtering is performed by district.
#' @param provincia Character vector. Name of the province to filter
#'   all its districts (e.g., "CUSCO"). If specified, it returns all the
#'   districts of the indicated province. Case-insensitive.
#' @param departamento Character vector. Name of the department to filter
#'   all its districts (e.g., "CUSCO"). If specified, it returns all the
#'   districts of the indicated department. Case-insensitive.
#' @param ano Numeric or character. Year of the district limits: \code{2023} (default)
#'   or \code{2025} (updated boundary layer).
#' @param show_progress Logical. If \code{TRUE} (default), shows informative
#'   messages about download progress. If \code{FALSE}, executes silently.
#' @param force_update Logical. If \code{TRUE}, forces a new download of the
#'   file even if it exists in cache. Default \code{FALSE} to leverage cached data.
#' @param ... Additional arguments passed to internal functions.
#'
#' @return An \code{sf} (simple feature) object with the geometry of the requested
#'   districts, including their geographic and administrative attributes.
#'
#' @details
#' The function downloads data directly from OSF (Open Science Framework)
#' and stores it in the user's cache directory (`tools::R_user_dir("rsdot", "cache")`).
#' The data corresponds to the official INEI census limits (2023 or 2025 update).
#'
#' The function automatically handles special character encoding (accents, ñ)
#' to ensure compatibility across different operating systems.
#'
#' Filters can be combined hierarchically:
#' \itemize{
#'   \item Department only: returns all districts in the department
#'   \item Department + province: returns all districts in that province
#'   \item Department + province + district: returns specific districts
#'   \item District only: returns that district from anywhere in Peru
#' }
#'
#' Data source: DEMARCA Repository on OSF (\url{https://osf.io/j9ax8/})
#'
#' @examples
#' \dontrun{
#' # Load all districts of Peru (2023)
#' distritos_peru <- get_distritos()
#'
#' # Load 2025 districts update
#' distritos_2025 <- get_distritos_2025()
#'
#' # Load a specific district
#' wanchaq <- get_distritos(distrito = "Wanchaq")
#'
#' # Load all districts of a province
#' distritos_cusco <- get_distritos(provincia = "Cusco")
#' }
#'
#' @seealso
#' \code{\link[sf]{read_sf}}, \code{\link[sf]{st_geometry}}
#'
#' @references
#' INEI (2023/2025). District Census Limits of Peru.
#' SDOT DEMARCA PERU VISOR Repository: \url{https://osf.io/qy4j6/overview}
#'
#' @export
#' @importFrom sf read_sf
#' @importFrom utils head
get_distritos <- function(distrito = NULL,
                          provincia = NULL,
                          departamento = NULL,
                          ano = 2023,
                          show_progress = TRUE,
                          force_update = FALSE,
                          ...) {
  # ==========================================================================
  # 1. PARAMETER VALIDATION
  # ==========================================================================

  # Validate parameter types
  if (!is.logical(show_progress) || length(show_progress) != 1) {
    stop("The 'show_progress' parameter must be TRUE or FALSE",
      call. = FALSE
    )
  }

  if (!is.logical(force_update) || length(force_update) != 1) {
    stop("The 'force_update' parameter must be TRUE or FALSE",
      call. = FALSE
    )
  }

  if (!is.null(distrito) && !is.character(distrito)) {
    stop("The 'distrito' parameter must be a character vector or NULL",
      call. = FALSE
    )
  }

  if (!is.null(provincia) && !is.character(provincia)) {
    stop("The 'provincia' parameter must be a character vector or NULL",
      call. = FALSE
    )
  }

  if (!is.null(departamento) && !is.character(departamento)) {
    stop("The 'departamento' parameter must be a character vector or NULL",
      call. = FALSE
    )
  }

  # ==========================================================================
  # 2. DATA ACQUISITION (GENERIC)
  # ==========================================================================

  catalog_key <- if (as.character(ano) %in% c("2025")) "distritos_2025" else "distritos"
  catalog <- get_rsdot_catalog(catalog_key)

  datos_sf <- download_and_read_spatial_data(
    osf_id = catalog$id_osf,
    file_name = catalog$file_name,
    show_progress = show_progress,
    force_update = force_update,
    pattern_shp = catalog$pattern_shp
  )

  # Save copy for help messages in case of empty filters
  datos_originales <- datos_sf

  # ==========================================================================
  # 3. HIERARCHICAL FILTERING: DEPARTMENT > PROVINCE > DISTRICT
  # ==========================================================================

  # Filter by DEPARTMENT
  if (!is.null(departamento)) {
    if ("nombdep" %in% names(datos_sf)) {
      dep_input <- toupper(trimws(departamento))
      datos_sf <- datos_sf[toupper(datos_sf$nombdep) %in% dep_input, ]

      if (nrow(datos_sf) == 0) {
        departamentos_disponibles <- sort(unique(datos_originales$nombdep))
        warning(
          "No districts found for department: ",
          paste(departamento, collapse = ", "), "\n",
          "Available departments:\n  ",
          paste(departamentos_disponibles, collapse = "\n  "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtering by department: ",
          nrow(datos_sf),
          " district(s) in ",
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
      datos_sf_temp <- datos_sf[toupper(datos_sf$nombprov) %in% prov_input, ]

      if (nrow(datos_sf_temp) == 0) {
        provincias_disponibles <- sort(unique(datos_sf$nombprov))
        warning(
          "No districts found for province: ",
          paste(provincia, collapse = ", "), "\n",
          "Available provinces",
          if (!is.null(departamento)) paste0(" in ", departamento) else "",
          ":\n  ",
          paste(provincias_disponibles, collapse = "\n  "),
          call. = FALSE
        )
        return(datos_sf)
      }

      datos_sf <- datos_sf_temp

      if (show_progress) {
        message(
          "\u2713 Filtering by province: ",
          nrow(datos_sf),
          " district(s) in ",
          paste(provincia, collapse = ", ")
        )
      }
    } else {
      warning(
        "Column 'nombprov' not found for province filtering.",
        call. = FALSE
      )
    }
  }

  # Filter by DISTRICT
  if (!is.null(distrito)) {
    if ("nombdist" %in% names(datos_sf)) {
      dist_input <- toupper(trimws(distrito))
      datos_sf_filtrado <- datos_sf[toupper(datos_sf$nombdist) %in% dist_input, ]

      if (nrow(datos_sf_filtrado) == 0) {
        distritos_disponibles <- sort(unique(datos_sf$nombdist))
        warning(
          "No matches found for: ",
          paste(distrito, collapse = ", "), "\n",
          "Available districts",
          if (!is.null(provincia)) paste0(" in ", provincia) else "",
          if (!is.null(departamento) && is.null(provincia)) {
            paste0(" in ", departamento)
          } else {
            ""
          },
          " (showing first 20):\n  ",
          paste(head(distritos_disponibles, 20), collapse = "\n  "),
          if (length(distritos_disponibles) > 20) {
            paste0("\n  ... and ", length(distritos_disponibles) - 20, " more")
          } else {
            ""
          },
          call. = FALSE
        )
        return(datos_sf)
      }

      datos_sf <- datos_sf_filtrado
    } else {
      warning(
        "Column 'nombdist' not found for district filtering.",
        call. = FALSE
      )
    }
  }

  # Final success message
  if (show_progress) {
    filtros_aplicados <- c()
    if (!is.null(departamento)) filtros_aplicados <- c(filtros_aplicados, "department")
    if (!is.null(provincia)) filtros_aplicados <- c(filtros_aplicados, "province")
    if (!is.null(distrito)) filtros_aplicados <- c(filtros_aplicados, "district")

    if (length(filtros_aplicados) > 0) {
      message(
        "\u2713 Filtering completed by ",
        paste(filtros_aplicados, collapse = " > "),
        ": ",
        nrow(datos_sf),
        " district(s)"
      )
    } else {
      message("\u2713 Data loaded: ", nrow(datos_sf), " districts")
    }
  }

  return(datos_sf)
}

#' @rdname get_distritos
#' @export
get_distritos_2025 <- function(...) {
  get_distritos(..., ano = 2025)
}
