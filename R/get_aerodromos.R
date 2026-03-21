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
#' **Data Source:**
#' \itemize{
#'   \item Source: Ministry of Transport and Communications (MTC)
#'   \item Year: 2022
#'   \item Level: Airfield/Airport/Heliport
#'   \item Application: Territorial planning and connectivity analysis
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
#' Cache is stored in: \code{tempdir()/DEMARCA_cache/aerodromos/}
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
#'
#' # Filter by specific district
#' aero_san_sebastian <- get_aerodromos(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "SAN SEBASTIAN"
#' )
#'
#' # Load multiple departments
#' aero_sur <- get_aerodromos(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Map of airfields by type
#' ggplot(aero_cusco) +
#'   geom_sf(aes(color = tipo, shape = tipo), size = 3) +
#'   scale_color_manual(
#'     values = c(
#'       "AEROPUERTO INTERNACIONAL" = "darkred",
#'       "AERODROMO" = "darkblue",
#'       "HELIPUERTO" = "darkgreen"
#'     ),
#'     name = "Type"
#'   ) +
#'   labs(
#'     title = "Airfields of the Department of Cusco",
#'     subtitle = "Updated 2022 - MTC",
#'     caption = "Source: MTC | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Airfields by ownership type
#' aero_cusco |>
#'   group_by(tipo, titular) |>
#'   summarise(n = n(), .groups = "drop") |>
#'   arrange(desc(n))
#'
#' # Filter operational public airfields
#' aero_publicos <- aero_cusco |>
#'   filter(titular == "PUBLICA", estado == "OPERATIVO")
#'
#' # View airfields by administrator
#' aero_cusco |>
#'   filter(!is.na(administ)) |>
#'   group_by(administ) |>
#'   summarise(n = n(), .groups = "drop") |>
#'   arrange(desc(n))
#'
#' # Map of airfields with labels
#' ggplot(aero_cusco) +
#'   geom_sf(aes(color = tipo), size = 4) +
#'   geom_sf_text(
#'     aes(label = nombre),
#'     size = 2.5,
#'     nudge_y = 0.1,
#'     check_overlap = TRUE
#'   ) +
#'   scale_color_brewer(palette = "Set1", name = "Type") +
#'   labs(
#'     title = "Airfields of Cusco",
#'     subtitle = "MTC 2022",
#'     caption = "Source: MTC | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link{get_distritos}}, \code{\link[sf]{read_sf}}
#'
#' @references
#' Ministry of Transport and Communications (MTC). Airfields of Peru 2022.
#'
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom utils head
get_aerodromos <- function(departamento = NULL,
                           provincia = NULL,
                           distrito = NULL,
                           show_progress = TRUE,
                           force_update = FALSE) {
  # ==========================================================================
  # 1. REFERENCE TABLE
  # ==========================================================================

  tabla_departamentos <- get_rsdot_catalog("aerodromos")

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
    message("\nUsage: get_aerodromos(departamento = 'CUSCO')")
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
      "Use get_aerodromos() to see the full list.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "aerodromos")
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

      datos_dep <- read_sf_safe(archivo_local, fix_encoding = TRUE)

      if (!is.null(datos_dep)) {
        lista_datos[[dep]] <- datos_dep
        if (show_progress) {
          message("\u2713 Loaded: ", dep, " (", nrow(datos_dep), " airfields)")
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
        stop(
          "Error combining data from multiple departments: ",
          e$message,
          call. = FALSE
        )
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
          "No airfields found for province(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(empty_sf())
      }

      if (show_progress) {
        message(
          "\u2713 Filtered by province: ",
          nrow(datos_final),
          " airfields"
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
          "No airfields found for district(s): ",
          paste(distrito, collapse = ", "),
          call. = FALSE
        )
        return(empty_sf())
      }

      if (show_progress) {
        message(
          "\u2713 Filtered by district: ",
          nrow(datos_final),
          " airfields"
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
      nrow(datos_final), " airfields"
    )
  }

  return(datos_final)
}
