#' Get Educational Facilities of Peru
#'
#' Downloads and loads information on educational facilities in Peru, prepared
#' by the Ministry of Education (MINEDU). An educational facility is a property
#' where one or more educational establishments (educational services) operate.
#' Data includes POINT type geometry and information on educational services
#' operating at each facility, location, and census area.
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
#' @return An \code{sf} (simple feature) object with POINT geometry containing
#'   educational facility information, including:
#'
#'   \strong{Facility Identification:}
#'   \itemize{
#'     \item POINT geometry (facility coordinates)
#'     \item \code{codlocal}: Unique educational facility code assigned by MINEDU
#'     \item \code{servicios}: List of educational services (educational institutions)
#'       operating in the facility, including name, level, and modality. A single
#'       facility may house multiple educational services (e.g., initial, primary,
#'       and secondary)
#'     \item \code{dir_cen}: Educational facility address
#'   }
#'
#'   \strong{Geographic Location:}
#'   \itemize{
#'     \item \code{localidad}: Name of the locality where the facility is located
#'     \item \code{codcp_inei}: Population center code according to INEI
#'     \item \code{codccpp}: Population center code (alternative)
#'     \item \code{centro_pob}: Population center name
#'     \item \code{departamen}: Department name
#'     \item \code{provincia}: Province name
#'     \item \code{distrito}: District name
#'   }
#'
#'   \strong{Facility Characteristics:}
#'   \itemize{
#'     \item \code{area_censo}: Census area where the facility is located:
#'       \itemize{
#'         \item URBANA: Urban zone
#'         \item RURAL: Rural zone
#'       }
#'   }
#'
#'   \strong{Coordinates:}
#'   \itemize{
#'     \item \code{nlat_ie}: Educational facility latitude (Y coordinate)
#'     \item \code{nlong_ie}: Educational facility longitude (X coordinate)
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
#'   \item Source: Ministry of Education (MINEDU)
#'   \item Registration: Educational Institutions Registry
#'   \item Level: Educational facility
#'   \item Application: Education coverage analysis, school infrastructure
#'     planning, and accessibility studies
#' }
#'
#' **Key Concepts:**
#'
#' \strong{Educational Facility vs Educational Institution:}
#' \itemize{
#'   \item \strong{Educational Facility}: Physical infrastructure (property) where
#'     educational services operate. A facility may house multiple
#'     educational institutions.
#'   \item \strong{Educational Service/Institution}: Operating unit of the
#'     educational system that provides a specific type of education (initial,
#'     primary, secondary, CETPRO, etc.)
#' }
#'
#' **Educational Levels and Modalities in Peru:**
#' \itemize{
#'   \item \strong{Regular Basic Education (EBR)}:
#'     \itemize{
#'       \item Initial (0-5 years): Nursery, Kindergarten
#'       \item Primary (6-11 years)
#'       \item Secondary (12-16 years)
#'     }
#'   \item \strong{Alternative Basic Education (EBA)}: For persons who did not
#'     timely access EBR
#'   \item \strong{Special Basic Education (EBE)}: For persons with
#'     disabilities or gifted talent
#'   \item \strong{Technical-Productive Education (CETPRO)}: Technical training
#'     and vocational training
#'   \item \strong{Higher Education}: Institutes and schools of pedagogical,
#'     technological, and artistic higher education
#' }
#'
#' **Interpretation of the servicios field:**
#'
#' The \code{servicios} field contains a text string with all educational services
#' operating in the facility, separated by commas. Each service includes the
#' name of the institution and its level/modality in parentheses.
#'
#' Example: "501330 (PRIMARIA), 501330 (SECUNDARIA)" indicates that primary and
#' secondary levels of IE 501330 operate in that facility.
#'
#' **Hierarchical Filtering:**
#' Filters are applied in cascade:
#' \enumerate{
#'   \item Specified departments are loaded first
#'   \item Then provinces are filtered (if specified)
#'   \item Finally districts are filtered (if specified)
#' }
#'
#' Cache is stored in: \code{tools::R_user_dir("rsdot", which = "cache")/locales_educativos/}
#'
#' **NOTE:** Geometries are POINT type and represent the location
#' of each educational facility.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_locales_educativos()
#'
#' # Load educational facilities of a full department
#' locales_cusco <- get_locales_educativos(departamento = "CUSCO")
#'
#' # Filter by specific province
#' locales_prov_cusco <- get_locales_educativos(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Filter by specific district
#' locales_wanchaq <- get_locales_educativos(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "WANCHAQ"
#' )
#'
#' # Load multiple departments
#' locales_sur <- get_locales_educativos(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Map of educational facilities by census area
#' ggplot(locales_cusco) +
#'   geom_sf(aes(color = area_censo), size = 1, alpha = 0.6) +
#'   scale_color_manual(
#'     values = c("URBANA" = "darkblue", "RURAL" = "darkgreen"),
#'     name = "Area"
#'   ) +
#'   labs(
#'     title = "Educational Facilities of the Department of Cusco",
#'     subtitle = "Ministry of Education",
#'     caption = "Source: MINEDU - Educational Institutions Registry"
#'   ) +
#'   theme_minimal()
#'
#' # Summary by census area
#' locales_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(area_censo)
#'
#' # Distribution by district
#' locales_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(distrito, sort = TRUE) |>
#'   head(10)
#'
#' # Filter facilities with initial level
#' initial_locales <- locales_cusco |>
#'   filter(grepl("INICIAL", servicios, ignore.case = TRUE))
#'
#' # Filter facilities with secondary
#' secondary_locales <- locales_cusco |>
#'   filter(grepl("SECUNDARIA", servicios, ignore.case = TRUE))
#'
#' # Facilities with multiple levels (initial + primary + secondary)
#' complete_locales <- locales_cusco |>
#'   filter(
#'     grepl("INICIAL", servicios) &
#'       grepl("PRIMARIA", servicios) &
#'       grepl("SECUNDARIA", servicios)
#'   )
#'
#' # Coverage analysis by province
#' province_coverage <- locales_cusco |>
#'   sf::st_drop_geometry() |>
#'   group_by(provincia) |>
#'   summarise(
#'     total_locales = n(),
#'     urban_locales = sum(area_censo == "URBANA"),
#'     rural_locales = sum(area_censo == "RURAL"),
#'     pct_rural = round(sum(area_censo == "RURAL") / n() * 100, 1),
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(total_locales))
#'
#' # Map of rural facilities
#' locales_cusco |>
#'   filter(area_censo == "RURAL") |>
#'   ggplot() +
#'   geom_sf(color = "darkgreen", size = 0.8, alpha = 0.5) +
#'   labs(
#'     title = "Rural Educational Facilities - Cusco",
#'     subtitle = "MINEDU",
#'     caption = "Source: MINEDU - Educational Institutions Registry"
#'   ) +
#'   theme_minimal()
#'
#' # Count educational services per facility
#' locales_cusco |>
#'   sf::st_drop_geometry() |>
#'   mutate(
#'     n_servicios = stringr::str_count(servicios, "\\(")
#'   ) |>
#'   count(n_servicios, name = "n_locales")
#'
#' # Search facilities by institution name
#' emblematic_locales <- locales_cusco |>
#'   filter(grepl("INCA GARCILASO|CLORINDA MATTO", servicios, ignore.case = TRUE))
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link{get_distritos}}, \code{\link{get_centros_poblados}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' Ministry of Education of Peru (MINEDU). Educational Institutions and
#' Educational Programs Registry.
#'
#' ESCALE - Statistics of Educational Quality: \url{https://escale.minedu.gob.pe/}
#'
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom utils head
get_locales_educativos <- function(departamento = NULL,
                                   provincia = NULL,
                                   distrito = NULL,
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
      "locales_educativos_amazonas.gpkg",
      "locales_educativos_ancash.gpkg",
      "locales_educativos_apurimac.gpkg",
      "locales_educativos_arequipa.gpkg",
      "locales_educativos_ayacucho.gpkg",
      "locales_educativos_cajamarca.gpkg",
      "locales_educativos_callao.gpkg",
      "locales_educativos_cusco.gpkg",
      "locales_educativos_huancavelica.gpkg",
      "locales_educativos_huanuco.gpkg",
      "locales_educativos_ica.gpkg",
      "locales_educativos_junin.gpkg",
      "locales_educativos_la_libertad.gpkg",
      "locales_educativos_lambayeque.gpkg",
      "locales_educativos_lima.gpkg",
      "locales_educativos_loreto.gpkg",
      "locales_educativos_madre_de_dios.gpkg",
      "locales_educativos_moquegua.gpkg",
      "locales_educativos_pasco.gpkg",
      "locales_educativos_piura.gpkg",
      "locales_educativos_puno.gpkg",
      "locales_educativos_san_martin.gpkg",
      "locales_educativos_tacna.gpkg",
      "locales_educativos_tumbes.gpkg",
      "locales_educativos_ucayali.gpkg"
    ),
    id_osf = c(
      "k9rsb", "5zams", "2p96k", "syq4r", "cqnb8",
      "rxmyz", "suh8t", "zx47g", "pv4ms", "afkgz",
      "6fhsn", "2ejaz", "xvpcw", "cresx", "cpz9t",
      "d5b8e", "n2cmg", "phkda", "paqdv", "2mh5y",
      "5zg68", "7625d", "zurp3", "exuty", "wr3cj"
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

  if (!is.null(distrito) && !is.character(distrito)) {
    stop("The 'distrito' parameter must be a character vector or NULL",
      call. = FALSE
    )
  }

  # If no department is specified, show list
  if (is.null(departamento)) {
    message("Available departments:")
    message(paste0("  - ", tabla_departamentos$departamento, collapse = "\n"))
    message("\nUsage: get_locales_educativos(departamento = 'CUSCO')")
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
      "Use get_locales_educativos() to see the full list.",
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

  ruta_cache_dir <- file.path(get_cache_dir(), "locales_educativos")
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

          # Normalize column names to lowercase
          colnames(datos_dep) <- tolower(colnames(datos_dep))

          lista_datos[[dep]] <- datos_dep

          if (show_progress) {
            message("\u2713 Loaded: ", dep, " (", nrow(datos_dep), " educational facilities)")
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
  # 6. FILTRADO POR PROVINCIA Y DISTRITO
  # ==========================================================================

  # Filter by PROVINCE
  if (!is.null(provincia)) {
    if ("provincia" %in% names(datos_final)) {
      prov_input <- toupper(trimws(provincia))
      datos_final <- datos_final[toupper(datos_final$provincia) %in% prov_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No educational facilities found for the province(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtered by province: ",
          nrow(datos_final),
          " educational facilities"
        )
      }
    } else {
      warning("Column 'provincia' not found for filtering", call. = FALSE)
    }
  }

  # Filter by DISTRICT
  if (!is.null(distrito)) {
    if ("distrito" %in% names(datos_final)) {
      dist_input <- toupper(trimws(distrito))
      datos_final <- datos_final[toupper(datos_final$distrito) %in% dist_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No educational facilities found for the district(s): ",
          paste(distrito, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtered by district: ",
          nrow(datos_final),
          " educational facilities"
        )
      }
    } else {
      warning("Column 'distrito' not found for filtering", call. = FALSE)
    }
  }

  # ==========================================================================
  # 7. MENSAJE FINAL
  # ==========================================================================

  if (show_progress) {
    message(
      "\u2713 Data loaded successfully: ",
      nrow(datos_final), " educational facilities"
    )
  }

  return(datos_final)
}
