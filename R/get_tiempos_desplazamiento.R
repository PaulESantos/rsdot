#' Get Travel Times to the Nearest Capital
#'
#' Downloads and loads information on travel times to the nearest departmental capital
#' from the DEMARCA OSF repository. The data represents accessibility layers
#' from the SDOT-PCM Spatial Data Viewer.
#' Supports spatial clipping via an area of interest, or filtering by department,
#' province, and district.
#'
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Valid options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
#'   "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA",
#'   "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
#'   Case-insensitive. If \code{NULL} and \code{area_interes} is also \code{NULL},
#'   shows the list of available departments.
#' @param provincia Character vector. Name(s) of the province(s) to filter. Optional.
#'   Since the raw dataset lacks administrative columns, filtering is performed spatially
#'   using boundary geometries.
#' @param distrito Character vector. Name(s) of the district(s) to filter. Optional.
#'   Performed spatially using boundary geometries.
#' @param area_interes An \code{sf} or \code{sfc} object. Bounding boxes are used
#'   to dynamically identify and download the corresponding departments, and the resulting
#'   layer is cropped to the exact geometry of the area of interest.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative
#'   messages about download progress. If \code{FALSE}, executes silently.
#' @param force_update Logical. If \code{TRUE}, forces a new download of the
#'   files even if they exist in cache. Default \code{FALSE}.
#'
#' @return An \code{sf} (simple feature) object with MULTIPOLYGON geometry containing
#'   accessibility attributes:
#'   \itemize{
#'     \item \code{dn}: Numeric classification of time values.
#'     \item \code{cat_tiempo}: Time interval category (e.g. "<= 30 min", etc.).
#'     \item \code{fid}: Feature identifier.
#'     \item \code{capa}: Source layer name.
#'   }
#'
#' @details
#' The function downloads data from OSF (Open Science Framework) and stores it
#' in persistent cache: \code{tools::R_user_dir("rsdot", which = "cache")/tiempo_desplazamiento/}.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_tiempos_desplazamiento()
#'
#' # Load times for a specific department
#' tiempos_cusco <- get_tiempos_desplazamiento(departamento = "CUSCO")
#'
#' # Using area of interest (clipping)
#' my_area <- get_distritos(departamento = "CUSCO", provincia = "CUSCO")
#' tiempos_clip <- get_tiempos_desplazamiento(area_interes = my_area)
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}}, \code{\link{get_distritos}}
#'
#' @export
#' @importFrom sf read_sf st_intersects st_intersection st_make_valid st_transform st_geometry st_crs
#' @importFrom utils download.file
get_tiempos_desplazamiento <- function(departamento = NULL,
                                       provincia = NULL,
                                       distrito = NULL,
                                       area_interes = NULL,
                                       show_progress = TRUE,
                                       force_update = FALSE) {
  # ==========================================================================
  # 1. CATALOG AND REFERENCE
  # ==========================================================================

  tabla_departamentos <- get_rsdot_catalog("tiempo_desplazamiento")

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

  # ==========================================================================
  # 3. AREA OF INTEREST INTERSECTION LOGIC
  # ==========================================================================

  if (!is.null(area_interes)) {
    if (!inherits(area_interes, "sf") && !inherits(area_interes, "sfc")) {
      stop("The 'area_interes' parameter must be an sf or sfc object", call. = FALSE)
    }

    if (show_progress) message("Determining departments overlapping with area of interest...")
    deps_all <- get_departamentos(show_progress = FALSE)

    # Transform CRS of area of interest if they differ
    if (sf::st_crs(deps_all) != sf::st_crs(area_interes)) {
      area_interes_proj <- sf::st_transform(area_interes, sf::st_crs(deps_all))
    } else {
      area_interes_proj <- area_interes
    }

    # Intersect
    intersect_matrix <- sf::st_intersects(deps_all, area_interes_proj, sparse = FALSE)
    overlapping_deps <- deps_all[rowSums(intersect_matrix) > 0, ]

    if (nrow(overlapping_deps) == 0) {
      stop("The 'area_interes' does not overlap with any department boundary of Peru", call. = FALSE)
    }

    departamentos_a_descargar <- unique(overlapping_deps$nombdep)
    if (show_progress) {
      message("\u2713 Area of interest overlaps with: ", paste(departamentos_a_descargar, collapse = ", "))
    }
  } else {
    departamentos_a_descargar <- departamento
  }

  # If no department and no area is specified, show list
  if (is.null(departamentos_a_descargar)) {
    message("Available departments:")
    message(paste0("  - ", tabla_departamentos$departamento, collapse = "\n"))
    message("\nUsage: get_tiempos_desplazamiento(departamento = 'CUSCO') or get_tiempos_desplazamiento(area_interes = sf_obj)")
    return(invisible(tabla_departamentos$departamento))
  }

  # Normalize names
  departamento_input <- toupper(trimws(departamentos_a_descargar))
  departamento_input <- gsub("_", " ", departamento_input)

  # Validate departments
  departamentos_validos <- departamento_input %in% tabla_departamentos$departamento

  if (!all(departamentos_validos)) {
    departamentos_invalidos <- departamento_input[!departamentos_validos]
    stop(
      "Invalid department(s): ",
      paste(departamentos_invalidos, collapse = ", "), "\n",
      "Use get_tiempos_desplazamiento() to see the full list.",
      call. = FALSE
    )
  }

  # Filter table
  tabla_filtrada <- tabla_departamentos[
    tabla_departamentos$departamento %in% departamento_input,
  ]

  # ==========================================================================
  # 4. DIRECTORY CONFIGURATION
  # ==========================================================================

  ruta_cache_dir <- file.path(get_cache_dir(), "tiempo_desplazamiento")
  if (!dir.exists(ruta_cache_dir)) {
    dir.create(ruta_cache_dir, recursive = TRUE)
  }

  # ==========================================================================
  # 5. DATA DOWNLOAD AND LOAD
  # ==========================================================================

  lista_datos <- list()

  for (i in seq_len(nrow(tabla_filtrada))) {
    dep <- tabla_filtrada$departamento[i]
    file_name <- tabla_filtrada$file_name[i]
    id_osf <- tabla_filtrada$id_osf[i]

    # Verify if file is loaded on OSF
    if (grepl("^(TODO_|STAGING_)", id_osf)) {
      stop(
        "Los datos de tiempos de desplazamiento para '", dep, "' se encuentran en preparaci\u00f3n en sdot_data antes de su publicaci\u00f3n oficial en OSF.",
        call. = FALSE
      )
    }

    url_descarga <- paste0("https://osf.io/download/", id_osf, "/")
    archivo_local <- file.path(ruta_cache_dir, file_name)

    # Download
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
        next
      }
    } else {
      if (show_progress) message("\u2713 Using cache: ", dep)
    }

    # Read GeoPackage
    if (file.exists(archivo_local)) {
      if (show_progress) message("Loading data: ", dep, "...")

      datos_dep <- read_sf_safe(
        archivo_local,
        fix_encoding = TRUE,
        valid_geom = FALSE,
        quiet = TRUE
      )

      if (!is.null(datos_dep)) {
        lista_datos[[dep]] <- datos_dep
        if (show_progress) {
          message("\u2713 Loaded: ", dep, " (", nrow(datos_dep), " records)")
        }
      }
    }
  }

  # ==========================================================================
  # 6. COMBINE DATA
  # ==========================================================================

  if (length(lista_datos) == 0) {
    stop("Could not load any department data", call. = FALSE)
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
        if (requireNamespace("dplyr", quietly = TRUE)) {
          datos_final <<- dplyr::bind_rows(lista_datos)
        } else {
          stop("Error combining data: ", e$message)
        }
      }
    )
  }

  # ==========================================================================
  # 7. SPATIAL FILTERING (PROVINCE / DISTRICT / AREA OF INTEREST)
  # ==========================================================================

  # Province filter (spatial join)
  if (!is.null(provincia)) {
    if (show_progress) message("Filtering spatially by province...")
    prov_sf <- get_provincias(
      departamento = departamento_input,
      provincia = provincia,
      show_progress = FALSE
    )
    
    datos_final <- sf::st_make_valid(datos_final)
    prov_sf <- sf::st_make_valid(prov_sf)
    
    if (sf::st_crs(datos_final) != sf::st_crs(prov_sf)) {
      prov_sf <- sf::st_transform(prov_sf, sf::st_crs(datos_final))
    }
    
    # Intersect with the geometry of provinces
    datos_final <- sf::st_intersection(datos_final, sf::st_geometry(prov_sf))
    
    if (nrow(datos_final) == 0) {
      warning("No data found for province(s): ", paste(provincia, collapse = ", "), call. = FALSE)
      return(empty_sf())
    }
  }

  # District filter (spatial join)
  if (!is.null(distrito)) {
    if (show_progress) message("Filtering spatially by district...")
    dist_sf <- get_distritos(
      departamento = departamento_input,
      provincia = provincia,
      distrito = distrito,
      show_progress = FALSE
    )
    
    datos_final <- sf::st_make_valid(datos_final)
    dist_sf <- sf::st_make_valid(dist_sf)
    
    if (sf::st_crs(datos_final) != sf::st_crs(dist_sf)) {
      dist_sf <- sf::st_transform(dist_sf, sf::st_crs(datos_final))
    }
    
    # Intersect with the geometry of districts
    datos_final <- sf::st_intersection(datos_final, sf::st_geometry(dist_sf))
    
    if (nrow(datos_final) == 0) {
      warning("No data found for district(s): ", paste(distrito, collapse = ", "), call. = FALSE)
      return(empty_sf())
    }
  }

  # Area of interest clipping
  if (!is.null(area_interes)) {
    if (show_progress) message("Cropping data to the area of interest...")
    datos_final <- sf::st_make_valid(datos_final)
    area_interes_valid <- sf::st_make_valid(area_interes)
    
    if (sf::st_crs(datos_final) != sf::st_crs(area_interes_valid)) {
      area_interes_valid <- sf::st_transform(area_interes_valid, sf::st_crs(datos_final))
    }
    
    datos_final <- sf::st_intersection(datos_final, area_interes_valid)
    
    if (nrow(datos_final) == 0) {
      warning("No data found within the specified area of interest", call. = FALSE)
      return(empty_sf())
    }
  }

  # ==========================================================================
  # 8. FINAL MESSAGE
  # ==========================================================================

  if (show_progress) {
    message("\u2713 Data loaded successfully: ", nrow(datos_final), " records")
  }

  return(datos_final)
}
