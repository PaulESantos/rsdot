#' Get Natural Risks Spatial Data of Peru
#'
#' Downloads and loads spatial data of natural risks (risk evaluations, non-mitigatable
#' risk areas, etc.) prepared by SDOT-PCM.
#'
#' @param tipo Character. The type of risk layer to retrieve. Valid options:
#'   \itemize{
#'     \item \code{"evaluacion_riesgos"} (or alias \code{"evaluacion"})
#'     \item \code{"zonas_riesgos_no_mitigable"} (or alias \code{"no_mitigable"})
#'   }
#'   Case-insensitive. If \code{NULL}, lists all available types.
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Case-insensitive. If \code{NULL}, lists available departments for the selected type.
#' @param provincia Character vector. Name(s) of the province(s) to filter. Optional.
#' @param distrito Character vector. Name(s) of the district(s) to filter. Optional.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative messages.
#' @param force_update Logical. If \code{TRUE}, forces a new download of files. Default \code{FALSE}.
#'
#' @return An \code{sf} (simple feature) object representing the requested risk layer.
#'
#' @details
#' Cache is stored under subdirectories of \code{tools::R_user_dir("rsdot", which = "cache")/}.
#'
#' @examples
#' \dontrun{
#' # List available types
#' get_riesgos()
#'
#' # Load non-mitigatable risk areas for Cusco
#' nm_cusco <- get_riesgos(tipo = "no_mitigable", departamento = "CUSCO")
#' }
#'
#' @export
#' @importFrom sf read_sf
get_riesgos <- function(tipo = NULL,
                        departamento = NULL,
                        provincia = NULL,
                        distrito = NULL,
                        show_progress = TRUE,
                        force_update = FALSE) {
  # ==========================================================================
  # 1. VALID TYPES MAPPING
  # ==========================================================================

  tipos_validos <- c(
    "evaluacion_riesgos"          = "riesgos_evaluacion_riesgos",
    "evaluacion"                 = "riesgos_evaluacion_riesgos",
    "zonas_riesgos_no_mitigable" = "riesgos_zonas_riesgos_no_mitigable",
    "no_mitigable"               = "riesgos_zonas_riesgos_no_mitigable"
  )

  if (is.null(tipo)) {
    message("Available risk types (tipo):")
    message(paste0("  - ", sort(unique(names(tipos_validos))), collapse = "\n"))
    return(invisible(unique(names(tipos_validos))))
  }

  tipo_input <- tolower(trimws(tipo))
  if (!tipo_input %in% names(tipos_validos)) {
    stop("Invalid risk type. Run get_riesgos() without arguments to see options.", call. = FALSE)
  }

  dataset_key <- tipos_validos[[tipo_input]]
  tabla_departamentos <- get_rsdot_catalog(dataset_key)

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
    stop("The 'provincia' parameter must be a character vector or NULL", call. = FALSE)
  }

  if (!is.null(distrito) && !is.character(distrito)) {
    stop("The 'distrito' parameter must be a character vector or NULL", call. = FALSE)
  }

  # If no department, show list
  if (is.null(departamento)) {
    message("Available departments:")
    message(paste0("  - ", tabla_departamentos$departamento, collapse = "\n"))
    message(paste0("\nUsage: get_riesgos(tipo = '", tipo_input, "', departamento = 'CUSCO')"))
    return(invisible(tabla_departamentos$departamento))
  }

  if (!is.character(departamento)) {
    stop("The 'departamento' parameter must be a character vector", call. = FALSE)
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
      "Run get_riesgos(tipo = '", tipo_input, "') to see available departments.",
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

  prefijo_capa <- sub("^riesgos_", "", dataset_key)
  ruta_cache_dir <- file.path(get_cache_dir(), paste0("riesgos_", prefijo_capa))
  if (!dir.exists(ruta_cache_dir)) {
    dir.create(ruta_cache_dir, recursive = TRUE)
  }

  # ==========================================================================
  # 4. DOWNLOAD AND LOAD DATA
  # ==========================================================================

  lista_datos <- list()

  for (i in seq_len(nrow(tabla_filtrada))) {
    dep <- tabla_filtrada$departamento[i]
    file_name <- tabla_filtrada$file_name[i]
    id_osf <- tabla_filtrada$id_osf[i]

    if (grepl("^(TODO_|STAGING_)", id_osf)) {
      stop(
        "Los datos de riesgos para '", dep, "' se encuentran en preparaci\u00f3n en sdot_data antes de su publicaci\u00f3n oficial en OSF.",
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
  # 5. COMBINE DATA
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
  # 6. FILTRADO POR PROVINCIA Y DISTRITO
  # ==========================================================================

  find_col <- function(df, names_to_check) {
    cols <- names(df)
    matched <- cols[cols %in% names_to_check]
    if (length(matched) > 0) matched[1] else NULL
  }

  # Filter by PROVINCE
  if (!is.null(provincia)) {
    col_prov <- find_col(datos_final, c("provincia", "nom_prov", "nom_provincia", "ubi_prov"))
    if (!is.null(col_prov)) {
      prov_input <- toupper(trimws(provincia))
      datos_final <- datos_final[toupper(datos_final[[col_prov]]) %in% prov_input, ]
      
      if (nrow(datos_final) == 0) {
        warning("No data found for province(s): ", paste(provincia, collapse = ", "), call. = FALSE)
        return(empty_sf())
      }
      if (show_progress) message("\u2713 Filtered by province: ", nrow(datos_final), " records")
    } else {
      # Fallback: Spatial filtering by province boundaries if no province column is found
      if (show_progress) message("Province column not found. Filtering spatially by province boundaries...")
      prov_sf <- get_provincias(departamento = departamento_input, provincia = provincia, show_progress = FALSE)
      datos_final <- sf::st_make_valid(datos_final)
      prov_sf <- sf::st_make_valid(prov_sf)
      if (sf::st_crs(datos_final) != sf::st_crs(prov_sf)) {
        prov_sf <- sf::st_transform(prov_sf, sf::st_crs(datos_final))
      }
      datos_final <- sf::st_intersection(datos_final, sf::st_geometry(prov_sf))
      if (nrow(datos_final) == 0) {
        warning("No data found for province(s): ", paste(provincia, collapse = ", "), call. = FALSE)
        return(empty_sf())
      }
    }
  }

  # Filter by DISTRICT
  if (!is.null(distrito)) {
    col_dist <- find_col(datos_final, c("distrito", "nom_dist", "nom_distrito", "ubi_dist"))
    if (!is.null(col_dist)) {
      dist_input <- toupper(trimws(distrito))
      datos_final <- datos_final[toupper(datos_final[[col_dist]]) %in% dist_input, ]
      
      if (nrow(datos_final) == 0) {
        warning("No data found for district(s): ", paste(distrito, collapse = ", "), call. = FALSE)
        return(empty_sf())
      }
      if (show_progress) message("\u2713 Filtered by district: ", nrow(datos_final), " records")
    } else {
      # Fallback: Spatial filtering by district boundaries if no district column is found
      if (show_progress) message("District column not found. Filtering spatially by district boundaries...")
      dist_sf <- get_distritos(departamento = departamento_input, provincia = provincia, distrito = distrito, show_progress = FALSE)
      datos_final <- sf::st_make_valid(datos_final)
      dist_sf <- sf::st_make_valid(dist_sf)
      if (sf::st_crs(datos_final) != sf::st_crs(dist_sf)) {
        dist_sf <- sf::st_transform(dist_sf, sf::st_crs(datos_final))
      }
      datos_final <- sf::st_intersection(datos_final, sf::st_geometry(dist_sf))
      if (nrow(datos_final) == 0) {
        warning("No data found for district(s): ", paste(distrito, collapse = ", "), call. = FALSE)
        return(empty_sf())
      }
    }
  }

  # ==========================================================================
  # 7. FINAL MESSAGE
  # ==========================================================================

  if (show_progress) {
    message("\u2713 Data loaded successfully: ", nrow(datos_final), " records")
  }

  return(datos_final)
}
