#' Get Population Density and Growth by Department
#'
#' Downloads and loads intercensal population density and growth information
#' by department from the DEMARCA OSF repository. Data comes from INEI
#' 2007 and 2017 population censuses, processed through spatial interpolation.
#' Allows stepwise filtering by department, province, and district.
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
#' @return An \code{sf} (simple feature) object with density and population growth
#'   information, including:
#'   \itemize{
#'     \item Geometry of interpolated polygons (with validated geometries)
#'     \item \code{ubigeo}: District UBIGEO code
#'     \item \code{nombdep}: Department name
#'     \item \code{nombprov}: Province name
#'     \item \code{nombdist}: District name
#'     \item \code{nivel}: Population growth level
#'     \item \code{rango}: Growth rate range
#'     \item \code{descrip}: Category description
#'   }
#'
#'   If multiple departments are requested, it returns a combined sf object.
#'
#' @details
#' The function downloads data from OSF (Open Science Framework) and stores it
#' in cache during the R session. Data is in GeoPackage (.gpkg) format.
#'
#' **Data Methodology:**
#' \itemize{
#'   \item Source: INEI 2007 and 2017 Population Censuses
#'   \item Coverage: Population centers with > 150 inhabitants
#'   \item Technique: Spatial interpolation for continuous representation
#'   \item Variable: Annual intercensal population growth rate
#' }
#'
#' **Hierarchical Filtering:**
#' Filters are applied in cascade:
#' \enumerate{
#'   \item Specified departments are loaded first
#'   \item Then provinces are filtered (if specified)
#'   \item Finally, districts are filtered (if specified)
#' }
#'
#' Cache is stored in: \code{tempdir()/DEMARCA_cache/densidad_poblacional/}
#'
#' **NOTE:** Geometries are automatically validated with \code{st_make_valid()}
#' to prevent errors in subsequent spatial operations.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_densidad_poblacional()
#'
#' # Load a full department
#' densidad_cusco <- get_densidad_poblacional(departamento = "CUSCO")
#'
#' # Load a specific province
#' densidad_prov_cusco <- get_densidad_poblacional(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Load a specific district
#' densidad_wanchaq <- get_densidad_poblacional(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "WANCHAQ"
#' )
#'
#' # Load multiple departments
#' densidad_sur <- get_densidad_poblacional(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Filter multiple provinces
#' densidad_valle <- get_densidad_poblacional(
#'   departamento = "CUSCO",
#'   provincia = c("CUSCO", "URUBAMBA", "CALCA")
#' )
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Growth level map
#' ggplot(densidad_cusco) +
#'   geom_sf(aes(fill = nivel), color = NA) +
#'   scale_fill_brewer(
#'     palette = "RdYlGn",
#'     name = "Growth\nLevel"
#'   ) +
#'   labs(
#'     title = "Population Growth by Level",
#'     subtitle = "Department of Cusco (2007-2017)",
#'     caption = "Source: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Aggregated layer by district (one polygon per district)
#' distritos_anta <- prov_anta |>
#'   group_by(ubigeo, nombdist) |>
#'   summarise(.groups = "drop")
#' distritos_anta
#'
#' # Points to place labels inside each district
#' distritos_centroides <- st_point_on_surface(distritos_anta)
#' distritos_centroides
#'
#' ggplot() +
#'   # 1. Cells / polygons with growth level
#'   geom_sf(
#'     data = prov_anta,
#'     aes(fill = nivel),
#'     color = NA
#'   ) +
#'   # 2. District outline (to make them stand out)
#'   geom_sf(
#'     data = distritos_anta,
#'     fill = NA,
#'     color = "grey20",
#'     linewidth = 0.4
#'   ) +
#'   # 3. District names
#'   geom_sf_text(
#'     data = distritos_centroides,
#'     aes(label = nombdist),
#'     size = 3
#'   ) +
#'   scale_fill_brewer(
#'     palette = "RdYlGn",
#'     name    = "Growth\nlevel"
#'   ) +
#'   labs(
#'     title    = "Population Growth",
#'     subtitle = "Province of Anta (2007–2017)",
#'     caption  = "Source: INEI | Visor SDOT"
#'   ) +
#'   theme_minimal() +
#'   theme(
#'     legend.position = "right",
#'     panel.grid.major = element_line(linewidth = 0.2, colour = "grey90")
#'   )
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link{get_distritos}}, \code{\link[sf]{read_sf}},
#' \code{\link[sf]{st_make_valid}}
#'
#' @references
#' INEI. National Population and Housing Censuses 2007 and 2017.
#'
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf st_make_valid
#' @importFrom utils head
get_densidad_poblacional <- function(departamento = NULL,
                                     provincia = NULL,
                                     distrito = NULL,
                                     show_progress = TRUE,
                                     force_update = FALSE) {
  # ==========================================================================
  # 1. DEPARTMENT REFERENCE TABLE
  # ==========================================================================

  tabla_departamentos <- get_rsdot_catalog("densidad_poblacional")

  # ==========================================================================
  # 2. PARAMETER VALIDATION
  # ==========================================================================

  if (!is.logical(show_progress) || length(show_progress) != 1) {
    stop("The 'show_progress' parameter must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.logical(force_update) || length(force_update) != 1) {
    stop("The 'force_update' parameter must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.null(provincia) && !is.character(provincia)) {
    stop("The 'provincia' parameter must be a character vector or NULL",
      call. = FALSE
    )
  }

  if (!is.null(distrito) && !is.character(distrito)) {
    stop("The 'distrito' parameter must be a character vector or NULL",
      call. = FALSE
    )
  }

  # If no department is specified, show list
  if (is.null(departamento)) {
    message("Available departments:")
    message(paste0("  - ", tabla_departamentos$departamento, collapse = "\n"))
    message("\nUsage: get_densidad_poblacional(departamento = 'CUSCO')")
    return(invisible(tabla_departamentos$departamento))
  }

  if (!is.character(departamento)) {
    stop("The 'departamento' parameter must be a character vector",
      call. = FALSE
    )
  }

  # Normalize names
  departamento_input <- toupper(trimws(departamento))
  departamento_input <- gsub("_", " ", departamento_input)

  # Validate departments
  departamentos_validos <- departamento_input %in% tabla_departamentos$departamento

  if (!all(departamentos_validos)) {
    departamentos_invalidos <- departamento_input[!departamentos_validos]
    stop(
      "Invalid department(s): ",
      paste(departamentos_invalidos, collapse = ", "), "\n",
      "Use get_densidad_poblacional() to see the full list.",
      call. = FALSE
    )
  }

  # Filter table
  tabla_filtrada <- tabla_departamentos[
    tabla_departamentos$departamento %in% departamento_input,
  ]

  # ==========================================================================
  # 3. DIRECTORY CONFIGURATION
  # ==========================================================================

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "densidad_poblacional")
  if (!dir.exists(ruta_cache_dir)) {
    dir.create(ruta_cache_dir, recursive = TRUE)
  }

  # ==========================================================================
  # 4. DATA DOWNLOAD AND LOAD
  # ==========================================================================

  lista_datos <- list()

  for (i in seq_len(nrow(tabla_filtrada))) {
    dep <- tabla_filtrada$departamento[i]
    file_name <- tabla_filtrada$file_name[i]
    id_osf <- tabla_filtrada$id_osf[i]

    url_descarga <- paste0("https://osf.io/download/", id_osf, "/")
    archivo_local <- file.path(ruta_cache_dir, file_name)

    # Download if doesn't exist or if forced
    if (!file.exists(archivo_local) || force_update) {
      if (show_progress) message("Downloading: ", dep, "...")

      descarga_exitosa <- download_file_safe(
        url = url_descarga,
        destfile = archivo_local,
        show_progress = show_progress
      )

      if (descarga_exitosa && show_progress) {
        message("\u2713 Download completed: ", dep)
      } else if (!descarga_exitosa) {
        next # Skip to next department
      }
    } else {
      if (show_progress) message("\u2713 Using cache: ", dep)
    }

    # Read GeoPackage
    if (file.exists(archivo_local)) {
      if (show_progress) message("Loading data: ", dep, "...")

      # Use read_sf_safe with geometry validation and encoding correction
      datos_dep <- read_sf_safe(
        archivo_local,
        fix_encoding = TRUE,
        valid_geom = TRUE,
        quiet = TRUE
      )

      if (!is.null(datos_dep)) {
        # Add department column if doesn't exist
        if (!"departamento" %in% names(datos_dep)) {
          datos_dep$departamento <- dep
        }

        # Specific correction for this dataset: A' -> Ñ
        if ("nombdist" %in% names(datos_dep)) {
          datos_dep$nombdist <- sub("A'", "\u00d1", datos_dep$nombdist, fixed = TRUE)
        }
        if ("nombprov" %in% names(datos_dep)) {
          datos_dep$nombprov <- sub("A'", "\u00d1", datos_dep$nombprov, fixed = TRUE)
        }

        lista_datos[[dep]] <- datos_dep

        if (show_progress) {
          message("\u2713 Loaded: ", dep, " (", nrow(datos_dep), " records)")
        }
      }
    }
  }

  # ==========================================================================
  # 5. COMBINE DATA
  # ==========================================================================

  if (length(lista_datos) == 0) {
    stop("Could not load any department", call. = FALSE)
  }

  if (length(lista_datos) == 1) {
    datos_final <- lista_datos[[1]]
  } else {
    if (show_progress) message("Combining data from multiple departments...")

    tryCatch(
      {
        datos_final <- do.call(rbind, lista_datos)
      },
      error = function(e) {
        # Fallback with dplyr::bind_rows if rbind fails
        if (requireNamespace("dplyr", quietly = TRUE)) {
          datos_final <<- dplyr::bind_rows(lista_datos)
        } else {
          stop("Error combining data: ", e$message)
        }
      }
    )
  }

  # ==========================================================================
  # 6. FILTER BY PROVINCE AND DISTRICT
  # ==========================================================================

  # Filter by PROVINCE
  if (!is.null(provincia)) {
    if ("nombprov" %in% names(datos_final)) {
      prov_input <- toupper(trimws(provincia))
      datos_final <- datos_final[toupper(datos_final$nombprov) %in% prov_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No data found for province(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(empty_sf())
      }

      if (show_progress) {
        message(
          "\u2713 Filtered by province: ",
          nrow(datos_final),
          " records"
        )
      }
    } else {
      warning("Column 'nombprov' not found for filtering", call. = FALSE)
    }
  }

  # Filter by DISTRICT
  if (!is.null(distrito)) {
    if ("nombdist" %in% names(datos_final)) {
      dist_input <- toupper(trimws(distrito))
      datos_final <- datos_final[toupper(datos_final$nombdist) %in% dist_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No data found for district(s): ",
          paste(distrito, collapse = ", "),
          call. = FALSE
        )
        return(empty_sf())
      }

      if (show_progress) {
        message(
          "\u2713 Filtered by district: ",
          nrow(datos_final),
          " records"
        )
      }
    } else {
      warning("Column 'nombdist' not found for filtering", call. = FALSE)
    }
  }

  # ==========================================================================
  # 7. FINAL MESSAGE
  # ==========================================================================

  if (show_progress) {
    message(
      "\u2713 Data loaded successfully: ",
      nrow(datos_final), " records"
    )
  }

  return(datos_final)
}
