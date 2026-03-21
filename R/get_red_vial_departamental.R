#' Get Departmental Road Network of Peru
#'
#' Downloads and loads information on the Departmental Road Network updated by
#' PROVIAS NACIONAL as of July 2022, according to the Route Classifier approved
#' by Supreme Decree 011-2016-MTC and its amendments. Data includes
#' MULTILINESTRING type geometry and information on trajectories, status, and
#' surface of departmental roads. Allows stepwise filtering by department and
#' province.
#'
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Valid options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
#'   "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA",
#'   "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
#'   Case-insensitive. If \code{NULL}, shows the list of available departments.
#' @param provincia Character vector. Name(s) of the province(s) to filter
#'   within the specified department(s). Optional.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative
#'   messages about download progress. If \code{FALSE}, executes silently.
#' @param force_update Logical. If \code{TRUE}, forces a new download of the
#'   file even if it exists in cache. Default \code{FALSE}.
#'
#' @return An \code{sf} (simple feature) object with MULTILINESTRING geometry
#'   containing departmental road network information, including:
#'   \itemize{
#'     \item MULTILINESTRING geometry (road trajectory)
#'     \item \code{nombdep}: Department name
#'     \item \code{nombprov}: Province name
#'     \item \code{trayectori}: Description of the route trajectory
#'     \item \code{iddpto}: Department code
#'     \item \code{jerarq}: Route hierarchy (RD = Red Departamental)
#'     \item \code{estado}: Numeric road status (1, 2, 3, 4)
#'     \item \code{codruta}: Route code (e.g., PI-102, CU-105)
#'     \item \code{superfic}: Surface type code (1-4)
#'     \item \code{longitud}: Segment length in kilometers
#'     \item \code{estado_l}: Descriptive status (GOOD, REGULAR, BAD)
#'     \item \code{superfic_l}: Surface type (PAVED, UNPAVED, DIRT, TRACK)
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
#'   \item Source: PROVIAS NACIONAL - Ministry of Transport and Communications
#'   \item Update: July 2022
#'   \item Legal basis: Supreme Decree 011-2016-MTC and amendments
#'   \item Level: Departmental Road Network
#'   \item Application: Road planning and territorial connectivity analysis
#' }
#'
#' **Surface Classification:**
#' \itemize{
#'   \item 1 = PAVED
#'   \item 2 = UNPAVED
#'   \item 3 = DIRT
#'   \item 4 = TRACK
#' }
#'
#' **Status Classification:**
#' \itemize{
#'   \item 1 = GOOD
#'   \item 2 = REGULAR
#'   \item 3 = BAD
#'   \item 4 = VERY BAD
#' }
#'
#' **Hierarchical Filtering:**
#' Filters are applied in cascade:
#' \enumerate{
#'   \item Specified departments are loaded first
#'   \item Then provinces are filtered (if specified)
#' }
#'
#' Cache is stored in: \code{tempdir()/DEMARCA_cache/red_vial_departamental/}
#'
#' **NOTE:** Geometries are MULTILINESTRING type and represent the trajectories
#' of departmental roads.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_red_vial_departamental()
#'
#' # Load road network of a full department
#' vias_cusco <- get_red_vial_departamental(departamento = "CUSCO")
#'
#' # Filter by specific province
#' vias_cusco_prov <- get_red_vial_departamental(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Load multiple departments
#' vias_sur <- get_red_vial_departamental(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Map of road network by surface type
#' ggplot(vias_cusco) +
#'   geom_sf(aes(color = superfic_l), linewidth = 0.8) +
#'   scale_color_manual(
#'     values = c(
#'       "PAVIMENTADO" = "darkblue",
#'       "AFIRMADO" = "darkgreen",
#'       "SIN AFIRMAR" = "orange",
#'       "TROCHA" = "brown"
#'     ),
#'     name = "Surface Type"
#'   ) +
#'   labs(
#'     title = "Departmental Road Network of Cusco",
#'     subtitle = "By Surface Type - PROVIAS 2022",
#'     caption = "Source: PROVIAS NACIONAL | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Map by conservation status
#' ggplot(vias_cusco) +
#'   geom_sf(aes(color = estado_l), linewidth = 1) +
#'   scale_color_manual(
#'     values = c(
#'       "BUENO" = "darkgreen",
#'       "REGULAR" = "orange",
#'       "MALO" = "red"
#'     ),
#'     name = "Status"
#'   ) +
#'   labs(
#'     title = "Conservation Status of the Road Network",
#'     subtitle = "Department of Cusco",
#'     caption = "Source: PROVIAS NACIONAL | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Length analysis by surface type
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   group_by(superfic_l) |>
#'   summarise(
#'     total_km = sum(longitud, na.rm = TRUE),
#'     n_tramos = n(),
#'     km_promedio = mean(longitud, na.rm = TRUE)
#'   ) |>
#'   arrange(desc(total_km))
#'
#' # Analysis by status and surface
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   group_by(estado_l, superfic_l) |>
#'   summarise(
#'     longitud_total = sum(longitud, na.rm = TRUE),
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(longitud_total))
#'
#' # Filter paved roads in good condition
#' vias_buenas <- vias_cusco |>
#'   filter(superfic_l == "PAVIMENTADO", estado_l == "BUENO")
#'
#' # Analysis by province
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   group_by(nombprov) |>
#'   summarise(
#'     longitud_total = sum(longitud, na.rm = TRUE),
#'     n_rutas = n_distinct(codruta),
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(longitud_total))
#'
#' # Visualize a specific province
#' vias_prov <- vias_cusco |>
#'   filter(nombprov == "CUSCO")
#'
#' ggplot(vias_prov) +
#'   geom_sf(aes(color = codruta), linewidth = 1.2) +
#'   labs(
#'     title = "Road Network of the Province of Cusco",
#'     subtitle = "Route Codes",
#'     caption = "Source: PROVIAS NACIONAL"
#'   ) +
#'   theme_minimal() +
#'   theme(legend.position = "right")
#'
#' # General statistics
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   summarise(
#'     longitud_total = sum(longitud, na.rm = TRUE),
#'     n_rutas = n_distinct(codruta),
#'     n_provincias = n_distinct(nombprov),
#'     km_pavimentado = sum(longitud[superfic_l == "PAVIMENTADO"], na.rm = TRUE),
#'     pct_pavimentado = (km_pavimentado / longitud_total) * 100
#'   )
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' PROVIAS NACIONAL - MTC. Base Cartográfica de la Red Vial Nacional 2022.
#'
#' Supreme Decree 011-2016-MTC - Route Classifier of the National Road System (SINAC).
#'
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf st_sf st_sfc
#' @importFrom utils head
get_red_vial_departamental <- function(departamento = NULL,
                                       provincia = NULL,
                                       show_progress = TRUE,
                                       force_update = FALSE) {
  # ==========================================================================
  # 1. TABLA DE REFERENCIA DE DEPARTAMENTOS Y URLs
  # ==========================================================================

  tabla_departamentos <- data.frame(
    departamento = c(
      "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
      "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO",
      "ICA", "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA",
      "LORETO", "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA",
      "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI"
    ),
    file_name = c(
      "red_vial_departamental_amazonas.gpkg",
      "red_vial_departamental_ancash.gpkg",
      "red_vial_departamental_apurimac.gpkg",
      "red_vial_departamental_arequipa.gpkg",
      "red_vial_departamental_ayacucho.gpkg",
      "red_vial_departamental_cajamarca.gpkg",
      "red_vial_departamental_callao.gpkg",
      "red_vial_departamental_cusco.gpkg",
      "red_vial_departamental_huancavelica.gpkg",
      "red_vial_departamental_huanuco.gpkg",
      "red_vial_departamental_ica.gpkg",
      "red_vial_departamental_junin.gpkg",
      "red_vial_departamental_la_libertad.gpkg",
      "red_vial_departamental_lambayeque.gpkg",
      "red_vial_departamental_lima.gpkg",
      "red_vial_departamental_loreto.gpkg",
      "red_vial_departamental_madre_de_dios.gpkg",
      "red_vial_departamental_moquegua.gpkg",
      "red_vial_departamental_pasco.gpkg",
      "red_vial_departamental_piura.gpkg",
      "red_vial_departamental_puno.gpkg",
      "red_vial_departamental_san_martin.gpkg",
      "red_vial_departamental_tacna.gpkg",
      "red_vial_departamental_tumbes.gpkg",
      "red_vial_departamental_ucayali.gpkg"
    ),
    id_osf = c(
      "f8x6v", "5g39z", "yc3dn", "tf3pa", "fu9aj",
      "e56sn", "dtej3", "dn6ta", "rcyjd", "5pv8c",
      "42hf8", "mrb64", "8jfq9", "mdq8x", "x9u4f",
      "5zsp6", "evnw9", "2auqc", "kdht6", "cvn8d",
      "9de73", "9fsy2", "zv96x", "y3aew", "pnxjf"
    ),
    stringsAsFactors = FALSE
  )

  # ==========================================================================
  # 2. VALIDACIÓN DE PARÁMETROS
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

  # If no department is specified, show list
  if (is.null(departamento)) {
    message("Available departments:")
    message(paste0("  - ", tabla_departamentos$departamento, collapse = "\n"))
    message("\nUsage: get_red_vial_departamental(departamento = 'CUSCO')")
    return(invisible(tabla_departamentos$departamento))
  }

  if (!is.character(departamento)) {
    stop("The 'departamento' parameter must be a character vector",
      call. = FALSE
    )
  }

  # Normalizar nombres (manejar "LA LIBERTAD", "SAN MARTIN", "MADRE DE DIOS")
  departamento_input <- toupper(trimws(departamento))
  departamento_input <- gsub("_", " ", departamento_input)

  # Validate departments
  departamentos_validos <- departamento_input %in% tabla_departamentos$departamento

  if (!all(departamentos_validos)) {
    departamentos_invalidos <- departamento_input[!departamentos_validos]
    stop(
      "Invalid department(s): ",
      paste(departamentos_invalidos, collapse = ", "), "\n",
      "Use get_red_vial_departamental() to see the full list.",
      call. = FALSE
    )
  }

  # Filtrar tabla
  tabla_filtrada <- tabla_departamentos[
    tabla_departamentos$departamento %in% departamento_input,
  ]

  # ==========================================================================
  # 3. CONFIGURACIÓN DE DIRECTORIOS
  # ==========================================================================

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "red_vial_departamental")
  if (!dir.exists(ruta_cache_dir)) {
    dir.create(ruta_cache_dir, recursive = TRUE)
  }

  # ==========================================================================
  # 4. DESCARGA Y CARGA DE DATOS
  # ==========================================================================

  lista_datos <- list()

  for (i in seq_len(nrow(tabla_filtrada))) {
    dep <- tabla_filtrada$departamento[i]
    file_name <- tabla_filtrada$file_name[i]
    id_osf <- tabla_filtrada$id_osf[i]

    url_descarga <- paste0("https://osf.io/download/", id_osf, "/")
    archivo_local <- file.path(ruta_cache_dir, file_name)

    # Download if it doesn't exist or if force_update is TRUE
    if (!file.exists(archivo_local) || force_update) {
      if (show_progress) message("Downloading: ", dep, "...")

      # Attempt download with extended timeout
      timeout_original <- getOption("timeout")
      options(timeout = 300) # 5 minutes

      descarga_exitosa <- tryCatch(
        {
          utils::download.file(
            url = url_descarga,
            destfile = archivo_local,
            mode = "wb",
            quiet = !show_progress
          )
          TRUE
        },
        error = function(e) {
          warning(
            "Error downloading ", dep, ": ", e$message, "\n",
            "URL: ", url_descarga, "\n",
            "The file may be temporarily unavailable.",
            call. = FALSE,
            immediate. = TRUE
          )
          FALSE
        }
      )

      options(timeout = timeout_original)

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

      tryCatch(
        {
          datos_dep <- sf::read_sf(archivo_local, quiet = TRUE)

          # Apply UTF-8 encoding correction
          cols_char <- names(datos_dep)[sapply(datos_dep, is.character)]
          for (col in cols_char) {
            if (col != "geom" && col != "geometry") {
              datos_dep[[col]] <- fix_double_utf8(datos_dep[[col]])
            }
          }

          # Normalize column names
          colnames(datos_dep) <- tolower(colnames(datos_dep))

          lista_datos[[dep]] <- datos_dep

          if (show_progress) {
            longitud_total <- sum(datos_dep$longitud, na.rm = TRUE)
            message(
              "\u2713 Loaded: ", dep,
              " (", nrow(datos_dep), " segments, ",
              round(longitud_total, 1), " km)"
            )
          }
        },
        error = function(e) {
          warning(
            "Error reading file for ", dep, ": ", e$message,
            call. = FALSE,
            immediate. = TRUE
          )
        }
      )
    }
  }

  # ==========================================================================
  # 5. COMBINAR DATOS
  # ==========================================================================

  if (length(lista_datos) == 0) {
    stop("Could not load any department", call. = FALSE)
  }

  if (length(lista_datos) == 1) {
    datos_final <- lista_datos[[1]]
  } else {
    if (show_progress) message("Combining data from multiple departments...")

    # Use rbind with error handling
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
  # 6. FILTRADO POR PROVINCIA
  # ==========================================================================

  # Filter by PROVINCE
  if (!is.null(provincia)) {
    if ("nombprov" %in% names(datos_final)) {
      prov_input <- toupper(trimws(provincia))
      datos_final <- datos_final[toupper(datos_final$nombprov) %in% prov_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No roads found for the province(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        longitud_filtrada <- sum(datos_final$longitud, na.rm = TRUE)
        message(
          "\u2713 Filtered by province: ",
          nrow(datos_final), " segments (",
          round(longitud_filtrada, 1), " km)"
        )
      }
    } else {
      warning("Column 'nombprov' not found for filtering", call. = FALSE)
    }
  }

  # ==========================================================================
  # 7. MENSAJE FINAL
  # ==========================================================================

  if (show_progress) {
    longitud_total <- sum(datos_final$longitud, na.rm = TRUE)
    message(
      "\u2713 Data loaded successfully: ",
      nrow(datos_final), " segments (",
      round(longitud_total, 1), " total km)"
    )
  }

  return(datos_final)
}
