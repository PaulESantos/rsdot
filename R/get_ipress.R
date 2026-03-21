#' Get Health Service Provider Institutions (IPRESS) of Peru
#'
#' Downloads and loads information on Health Service Provider Institutions
#' (IPRESS) of Peru, prepared by the National Health Superintendence
#' (SUSALUD). Data includes POINT type geometry and information on health
#' establishments, supporting medical services, categorization, operating
#' hours, and health networks. Updated to 2024.
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
#'   IPRESS information, including:
#'
#'   \strong{Establishment Identification:}
#'   \itemize{
#'     \item POINT geometry (establishment coordinates)
#'     \item \code{cod_unic_e}: Unique establishment code (RENIPRESS)
#'     \item \code{nom_esta}: Health establishment name
#'     \item \code{dir_esta}: Full address of the establishment
#'     \item \code{num_tele}: Contact phone number
#'   }
#'
#'   \strong{Type and Classification:}
#'   \itemize{
#'     \item \code{cod_tipo_e}: Establishment type code
#'     \item \code{des_tipo_e}: Establishment type description:
#'       \itemize{
#'         \item 1 = Health establishment without inpatient care
#'         \item 2 = Health establishment with inpatient care
#'         \item 3 = Supporting medical service
#'         \item 4 = Flexible offer
#'         \item 5 = Therapeutic communities
#'       }
#'     \item \code{cod_clas_e}: Establishment classification code
#'     \item \code{des_clas_e}: Classification description (e.g., Health posts,
#'       Health centers, Hospitals, Clinics, Doctor's offices, etc.)
#'   }
#'
#'   \strong{Administering Institution:}
#'   \itemize{
#'     \item \code{cod_inst_d}: Administering institution code
#'     \item \code{des_inst_d}: Administering institution description
#'   }
#'
#'   \strong{Geographic Location:}
#'   \itemize{
#'     \item \code{cod_ubig_e}: Establishment ubigeo code
#'     \item \code{nombdep}: Department name
#'     \item \code{nombprov}: Province name
#'     \item \code{nombdist}: District name
#'     \item \code{val_lati}: Establishment latitude
#'     \item \code{val_long}: Establishment longitude
#'   }
#'
#'   \strong{Categorization and Operation:}
#'   \itemize{
#'     \item \code{cod_cate}: Establishment category code (e.g., I-1,
#'       I-2, I-3, I-4, II-1, II-2, II-E, III-1, III-2, III-E)
#'     \item \code{doc_cate}: Categorization resolution document
#'     \item \code{fec_inic_o}: Operations start date
#'     \item \code{des_hora_e}: Establishment operating hours
#'     \item \code{cod_cond_e}: Establishment condition (e.g., ACTIVE, INACTIVE)
#'   }
#'
#'   \strong{Health Network:}
#'   \itemize{
#'     \item \code{cod_auto_s}: Health authority code
#'     \item \code{des_auto_s}: Health authority description (region)
#'     \item \code{cod_reds}: Health network code
#'     \item \code{des_reds}: Health network name
#'     \item \code{cod_micr_r}: Micro-network code
#'     \item \code{des_micr_r}: Micro-network name
#'   }
#'
#'   \strong{Representative Data:}
#'   \itemize{
#'     \item \code{cod_nruc}: Institution RUC number
#'     \item \code{nom_repr_l}: Legal representative name
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
#'   \item Source: National Health Superintendence (SUSALUD)
#'   \item Registration: RENIPRESS (National IPRESS Registry)
#'   \item Update year: 2024
#'   \item Level: Health establishment
#'   \item Application: Analysis of access to health services, health
#'     planning, and epidemiological studies
#' }
#'
#' **Health Establishment Categorization System in Peru:**
#'
#' Categorization determines the resolutive capacity of the establishment:
#' \itemize{
#'   \item \strong{First level (I-1 to I-4)}: Basic and preventive care
#'     \itemize{
#'       \item I-1: Health post (nursing technician)
#'       \item I-2: Health post (health professional)
#'       \item I-3: Health center without inpatient care
#'       \item I-4: Health center with inpatient care
#'     }
#'   \item \strong{Second level (II-1, II-2, II-E)}: Specialized care
#'     \itemize{
#'       \item II-1: Hospital I
#'       \item II-2: Hospital II
#'       \item II-E: Specialized hospital
#'     }
#'   \item \strong{Third level (III-1, III-2, III-E)}: High specialization
#'     \itemize{
#'       \item III-1: Hospital III
#'       \item III-2: Specialized institute
#'       \item III-E: High complexity specialized institute
#'     }
#' }
#'
#' **IPRESS Types:**
#' \enumerate{
#'   \item \strong{Establishments without inpatient care}: Offices, health
#'     posts, medical centers, polyclinics
#'   \item \strong{Establishments with inpatient care}: Hospitals, clinics,
#'     health centers with beds
#'   \item \strong{Supporting medical services}: Laboratories, image
#'     diagnostic centers, blood banks, clinical pathology
#' }
#'
#' **Hierarchical Filtering:**
#' Filters are applied in cascade:
#' \enumerate{
#'   \item Specified departments are loaded first
#'   \item Then provinces are filtered (if specified)
#'   \item Finally districts are filtered (if specified)
#' }
#'
#' Cache is stored in: \code{tempdir()/DEMARCA_cache/ipress/}
#'
#' **NOTE:** Geometries are POINT type and represent the location
#' of each health establishment.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_ipress()
#'
#' # Load IPRESS of a full department
#' ipress_cusco <- get_ipress(departamento = "CUSCO")
#'
#' # Filter by specific province
#' ipress_prov_cusco <- get_ipress(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Filter by specific district
#' ipress_wanchaq <- get_ipress(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "WANCHAQ"
#' )
#'
#' # Load multiple departments
#' ipress_sur <- get_ipress(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Map of IPRESS by establishment type
#' ggplot(ipress_cusco) +
#'   geom_sf(aes(color = des_tipo_e), size = 1.5, alpha = 0.7) +
#'   scale_color_brewer(palette = "Set1", name = "Type") +
#'   labs(
#'     title = "Health Service Provider Institutions - Cusco",
#'     subtitle = "SUSALUD 2024",
#'     caption = "Source: SUSALUD - RENIPRESS"
#'   ) +
#'   theme_minimal()
#'
#' # Summary by establishment type
#' ipress_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(des_tipo_e, sort = TRUE)
#'
#' # Summary by administering institution
#' ipress_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(des_inst_d, sort = TRUE)
#'
#' # Filter active public establishments
#' ipress_publicos <- ipress_cusco |>
#'   filter(
#'     des_inst_d == "GOBIERNO REGIONAL",
#'     cod_cond_e == "ACTIVO"
#'   )
#'
#' # Analysis by category
#' ipress_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(cod_cate, sort = TRUE) |>
#'   head(10)
#'
#' # Map of hospitals and clinics (establishments with inpatient care)
#' ipress_cusco |>
#'   filter(cod_tipo_e == "2") |>
#'   ggplot() +
#'   geom_sf(aes(color = des_inst_d), size = 3) +
#'   scale_color_brewer(palette = "Dark2", name = "Administrator") +
#'   labs(
#'     title = "Health Establishments with Inpatient Care - Cusco",
#'     subtitle = "Hospitals and Clinics",
#'     caption = "Source: SUSALUD - RENIPRESS 2024"
#'   ) +
#'   theme_minimal()
#'
#' # Coverage analysis by district
#' cobertura_distrito <- ipress_cusco |>
#'   sf::st_drop_geometry() |>
#'   group_by(nombdist) |>
#'   summarise(
#'     total_ipress = n(),
#'     con_internamiento = sum(cod_tipo_e == "2"),
#'     sin_internamiento = sum(cod_tipo_e == "1"),
#'     supporting_services = sum(cod_tipo_e == "3"),
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(total_ipress))
#'
#' # Distribution by health network
#' ipress_cusco |>
#'   sf::st_drop_geometry() |>
#'   filter(!is.na(des_reds)) |>
#'   count(des_reds, sort = TRUE)
#'
#' # Map of establishments by category (first level)
#' ipress_cusco |>
#'   filter(grepl("^I-", cod_cate)) |>
#'   ggplot() +
#'   geom_sf(aes(color = cod_cate), size = 2, alpha = 0.7) +
#'   scale_color_viridis_d(name = "Category") +
#'   labs(
#'     title = "First Level Establishments - Cusco",
#'     subtitle = "Categories I-1 to I-4",
#'     caption = "Source: SUSALUD - RENIPRESS 2024"
#'   ) +
#'   theme_minimal()
#'
#' # Search establishments by name
#' regional_hospitals <- ipress_cusco |>
#'   filter(grepl("REGIONAL|HOSPITAL", nom_esta, ignore.case = TRUE))
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link{get_distritos}}, \code{\link{get_centros_poblados}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' National Health Superintendence (SUSALUD). National Registry of Health
#' Service Provider Institutions (RENIPRESS) 2024.
#'
#' Ministry of Health of Peru. Technical Standard for Categorization of
#' Health Establishments.
#'
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom utils head
get_ipress <- function(departamento = NULL,
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
      "instituciones_prestadoras_servicios_salud_amazonas.gpkg",
      "instituciones_prestadoras_servicios_salud_ancash.gpkg",
      "instituciones_prestadoras_servicios_salud_apurimac.gpkg",
      "instituciones_prestadoras_servicios_salud_arequipa.gpkg",
      "instituciones_prestadoras_servicios_salud_ayacucho.gpkg",
      "instituciones_prestadoras_servicios_salud_cajamarca.gpkg",
      "instituciones_prestadoras_servicios_salud_callao.gpkg",
      "instituciones_prestadoras_servicios_salud_cusco.gpkg",
      "instituciones_prestadoras_servicios_salud_huancavelica.gpkg",
      "instituciones_prestadoras_servicios_salud_huanuco.gpkg",
      "instituciones_prestadoras_servicios_salud_ica.gpkg",
      "instituciones_prestadoras_servicios_salud_junin.gpkg",
      "instituciones_prestadoras_servicios_salud_la_libertad.gpkg",
      "instituciones_prestadoras_servicios_salud_lambayeque.gpkg",
      "instituciones_prestadoras_servicios_salud_lima.gpkg",
      "instituciones_prestadoras_servicios_salud_loreto.gpkg",
      "instituciones_prestadoras_servicios_salud_madre_de_dios.gpkg",
      "instituciones_prestadoras_servicios_salud_moquegua.gpkg",
      "instituciones_prestadoras_servicios_salud_pasco.gpkg",
      "instituciones_prestadoras_servicios_salud_piura.gpkg",
      "instituciones_prestadoras_servicios_salud_puno.gpkg",
      "instituciones_prestadoras_servicios_salud_san_martin.gpkg",
      "instituciones_prestadoras_servicios_salud_tacna.gpkg",
      "instituciones_prestadoras_servicios_salud_tumbes.gpkg",
      "instituciones_prestadoras_servicios_salud_ucayali.gpkg"
    ),
    id_osf = c(
      "3b85s", "b9qap", "zvjwb", "8xydq", "zmjgx",
      "wk632", "ky3av", "pzfsx", "s768q", "c6w3j",
      "w9j8u", "gj95n", "28m7j", "4rt9x", "ngvu9",
      "2dy5w", "gya9x", "y5pma", "de8y3", "evxb5",
      "azmq2", "my4wp", "uenj3", "br4fv", "tg4vh"
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
    message("\nUsage: get_ipress(departamento = 'CUSCO')")
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
      "Use get_ipress() to see the full list.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "ipress")
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
            message("\u2713 Loaded: ", dep, " (", nrow(datos_dep), " establishments)")
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
    if ("nombprov" %in% names(datos_final)) {
      prov_input <- toupper(trimws(provincia))
      datos_final <- datos_final[toupper(datos_final$nombprov) %in% prov_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No establishments found for the province(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtered by province: ",
          nrow(datos_final),
          " establishments"
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
          "No establishments found for the district(s): ",
          paste(distrito, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtered by district: ",
          nrow(datos_final),
          " establishments"
        )
      }
    } else {
      warning("Column 'nombdist' not found for filtering", call. = FALSE)
    }
  }

  # ==========================================================================
  # 7. MENSAJE FINAL
  # ==========================================================================

  if (show_progress) {
    message(
      "\u2713 Data loaded successfully: ",
      nrow(datos_final), " health establishments (IPRESS)"
    )
  }

  return(datos_final)
}
