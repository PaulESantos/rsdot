#' Get Local Road Network of Peru
#'
#' Downloads and loads information on the Local Road Network updated by
#' PROVIAS DESCENTRALIZADO as of July 2022, according to the Route Classifier
#' approved by Supreme Decree 011-2016-MTC and its amendments. Data includes
#' MULTILINESTRING type geometry and information on trajectories, status, and
#' surface of local or rural roads. Allows stepwise filtering by department and
#' province.
#'
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Valid options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
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
#'   containing local road network information, including:
#'   \itemize{
#'     \item MULTILINESTRING geometry (road trajectory)
#'     \item \code{nombdep}: Department name
#'     \item \code{nombprov}: Province name
#'     \item \code{iddpto}: Department code
#'     \item \code{jerarq}: Route hierarchy (RV = Red Vecinal)
#'     \item \code{trayectori}: Description of the route trajectory
#'     \item \code{estado}: Numeric road status (1, 2, 3)
#'     \item \code{codruta}: Local route code (e.g., LO-517, CU-123)
#'     \item \code{superfic}: Surface type code (1-4)
#'     \item \code{longitud}: Segment length in kilometers
#'     \item \code{estado_l}: Descriptive status (Good, Regular, Bad)
#'     \item \code{superfic_l}: Surface type (Paved, Unpaved, Dirt, Track)
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
#'   \item Source: PROVIAS DESCENTRALIZADO - Ministry of Transport and Communications
#'   \item Update: July 2022
#'   \item Legal basis: Supreme Decree 011-2016-MTC and amendments
#'   \item Level: Local or Rural Road Network (National Road System - SINAC)
#'   \item Application: Local road planning and rural connectivity analysis
#' }
#'
#' **Surface Classification:**
#' \itemize{
#'   \item 1 = Paved
#'   \item 2 = Unpaved
#'   \item 3 = Dirt
#'   \item 4 = Track
#' }
#'
#' **Status Classification:**
#' \itemize{
#'   \item 1 = Good
#'   \item 2 = Regular
#'   \item 3 = Bad
#' }
#'
#' **Local Road Network Characteristics:**
#' Local or rural roads connect minor population centers, hamlets, and
#' production areas with the departmental and national road networks.
#' They are the responsibility of local governments (municipalities).
#'
#' **Hierarchical Filtering:**
#' Filters are applied in cascade:
#' \enumerate{
#'   \item Specified departments are loaded first
#'   \item Then provinces are filtered (if specified)
#' }
#'
#' Cache is stored in: \code{tempdir()/DEMARCA_cache/red_vial_vecinal/}
#'
#' **NOTE:** Geometries are MULTILINESTRING type and represent the trajectories
#' of local roads.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_red_vial_vecinal()
#'
#' # Load local road network of a full department
#' vias_cusco <- get_red_vial_vecinal(departamento = "CUSCO")
#'
#' # Filter by specific province
#' vias_cusco_prov <- get_red_vial_vecinal(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Load multiple departments
#' vias_sur <- get_red_vial_vecinal(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Map of local road network by surface type
#' ggplot(vias_cusco) +
#'   geom_sf(aes(color = superfic_l), linewidth = 0.6) +
#'   scale_color_manual(
#'     values = c(
#'       "Pavimentado" = "darkblue",
#'       "Afirmado" = "darkgreen",
#'       "Sin afirmar" = "orange",
#'       "Trocha" = "brown"
#'     ),
#'     name = "Surface Type"
#'   ) +
#'   labs(
#'     title = "Local Road Network of Cusco",
#'     subtitle = "By Surface Type - PROVIAS 2022",
#'     caption = "Source: PROVIAS DESCENTRALIZADO | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Map by conservation status
#' ggplot(vias_cusco) +
#'   geom_sf(aes(color = estado_l), linewidth = 0.8) +
#'   scale_color_manual(
#'     values = c(
#'       "Bueno" = "darkgreen",
#'       "Regular" = "orange",
#'       "Malo" = "red"
#'     ),
#'     name = "Status"
#'   ) +
#'   labs(
#'     title = "Conservation Status of the Local Road Network",
#'     subtitle = "Department of Cusco",
#'     caption = "Source: PROVIAS DESCENTRALIZADO | Visor - SDOT"
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
#' # Filter roads in bad condition needing intervention
#' vias_mal_estado <- vias_cusco |>
#'   filter(estado_l == "Malo")
#'
#' # Analysis by province
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   group_by(nombprov) |>
#'   summarise(
#'     longitud_total = sum(longitud, na.rm = TRUE),
#'     n_rutas = n_distinct(codruta),
#'     km_trocha = sum(longitud[superfic_l == "Trocha"], na.rm = TRUE),
#'     pct_trocha = (km_trocha / longitud_total) * 100,
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(longitud_total))
#'
#' # Bar chart: km by province
#' vias_prov_summary <- vias_cusco |>
#'   st_drop_geometry() |>
#'   group_by(nombprov) |>
#'   summarise(total_km = sum(longitud, na.rm = TRUE), .groups = "drop") |>
#'   arrange(desc(total_km)) |>
#'   head(10)
#'
#' ggplot(vias_prov_summary, aes(x = reorder(nombprov, total_km), y = total_km)) +
#'   geom_col(fill = "steelblue") +
#'   coord_flip() +
#'   labs(
#'     title = "Top 10 Provinces with Largest Local Road Network",
#'     subtitle = "Department of Cusco",
#'     x = "Province",
#'     y = "Total Length (km)",
#'     caption = "Source: PROVIAS DESCENTRALIZADO"
#'   ) +
#'   theme_minimal()
#'
#' # Surface analysis by province
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   group_by(nombprov, superfic_l) |>
#'   summarise(km = sum(longitud, na.rm = TRUE), .groups = "drop") |>
#'   pivot_wider(names_from = superfic_l, values_from = km, values_fill = 0) |>
#'   arrange(desc(Trocha))
#'
#' # Filter only paved or unpaved roads
#' vias_mejoradas <- vias_cusco |>
#'   filter(superfic_l %in% c("Pavimentado", "Afirmado"))
#'
#' # General statistics
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   summarise(
#'     longitud_total = sum(longitud, na.rm = TRUE),
#'     n_rutas = n_distinct(codruta),
#'     n_provincias = n_distinct(nombprov),
#'     km_trocha = sum(longitud[superfic_l == "Trocha"], na.rm = TRUE),
#'     pct_trocha = (km_trocha / longitud_total) * 100,
#'     km_afirmado = sum(longitud[superfic_l == "Afirmado"], na.rm = TRUE),
#'     pct_afirmado = (km_afirmado / longitud_total) * 100,
#'     km_mal_estado = sum(longitud[estado_l == "Malo"], na.rm = TRUE),
#'     pct_mal_estado = (km_mal_estado / longitud_total) * 100
#'   )
#'
#' # Comparison between provinces
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   group_by(nombprov) |>
#'   summarise(
#'     total_km = sum(longitud, na.rm = TRUE),
#'     pct_bueno = sum(longitud[estado_l == "Bueno"], na.rm = TRUE) / total_km * 100,
#'     pct_regular = sum(longitud[estado_l == "Regular"], na.rm = TRUE) / total_km * 100,
#'     pct_malo = sum(longitud[estado_l == "Malo"], na.rm = TRUE) / total_km * 100,
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(total_km))
#'
#' # Visualize a specific province
#' vias_prov <- vias_cusco |>
#'   filter(nombprov == "CUSCO")
#'
#' ggplot(vias_prov) +
#'   geom_sf(aes(color = estado_l), linewidth = 1) +
#'   scale_color_manual(
#'     values = c("Bueno" = "green", "Regular" = "orange", "Malo" = "red")
#'   ) +
#'   labs(
#'     title = "Local Road Network - Province of Cusco",
#'     subtitle = "Conservation Status",
#'     color = "Status"
#'   ) +
#'   theme_minimal()
#' }
#'
#' @seealso
#' \code{\link{get_red_vial_departamental}}, \code{\link{get_red_vial_nacional}},
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' PROVIAS DESCENTRALIZADO - MTC. Base Cartográfica de la Red Vial Vecinal 2022.
#'
#' Supreme Decree 011-2016-MTC - Route Classifier of the National Road System (SINAC).
#'
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf st_sf st_sfc st_drop_geometry
#' @importFrom utils head
get_red_vial_vecinal <- function(departamento = NULL,
                                 provincia = NULL,
                                 show_progress = TRUE,
                                 force_update = FALSE) {
  # ==========================================================================
  # 1. TABLA DE REFERENCIA DE DEPARTAMENTOS Y URLs
  # ==========================================================================

  tabla_departamentos <- data.frame(
    departamento = c(
      "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
      "CAJAMARCA", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA",
      "JUNIN", "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO",
      "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO",
      "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI"
    ),
    file_name = c(
      "red_vial_vecinal_amazonas.gpkg",
      "red_vial_vecinal_ancash.gpkg",
      "red_vial_vecinal_apurimac.gpkg",
      "red_vial_vecinal_arequipa.gpkg",
      "red_vial_vecinal_ayacucho.gpkg",
      "red_vial_vecinal_cajamarca.gpkg",
      "red_vial_vecinal_cusco.gpkg",
      "red_vial_vecinal_huancavelica.gpkg",
      "red_vial_vecinal_huanuco.gpkg",
      "red_vial_vecinal_ica.gpkg",
      "red_vial_vecinal_junin.gpkg",
      "red_vial_vecinal_la_libertad.gpkg",
      "red_vial_vecinal_lambayeque.gpkg",
      "red_vial_vecinal_lima.gpkg",
      "red_vial_vecinal_loreto.gpkg",
      "red_vial_vecinal_madre_de_dios.gpkg",
      "red_vial_vecinal_moquegua.gpkg",
      "red_vial_vecinal_pasco.gpkg",
      "red_vial_vecinal_piura.gpkg",
      "red_vial_vecinal_puno.gpkg",
      "red_vial_vecinal_san_martin.gpkg",
      "red_vial_vecinal_tacna.gpkg",
      "red_vial_vecinal_tumbes.gpkg",
      "red_vial_vecinal_ucayali.gpkg"
    ),
    id_osf = c(
      "trke8", "ctr5k", "2547d", "nhef7", "uta9p",
      "5vmx8", "cvyxg", "h3gyc", "wsz7c", "s3uat",
      "na549", "e6ru2", "htge2", "zrht5", "7gqsn",
      "82cxp", "d83r4", "p9s3n", "nfzj4", "7e3r5",
      "3bzum", "47bj3", "wefkj", "6v342"
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
    message("\nUsage: get_red_vial_vecinal(departamento = 'CUSCO')")
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
      "Use get_red_vial_vecinal() to see the full list.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "red_vial_vecinal")
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
