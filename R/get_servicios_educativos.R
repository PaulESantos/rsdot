#' Get Educational Services of Peru
#'
#' Downloads and loads information on educational services in Peru, prepared by
#' the Ministry of Education (MINEDU). An educational service is a set of
#' educational activities designed and organized to achieve a learning
#' objective. Data includes POINT type geometry and information on educational
#' level, modality, management, shift, and service characteristics.
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
#' @param show_progress Logical. If \code{TRUE} (default), shows informative
#'   messages about download progress. If \code{FALSE}, executes silently.
#' @param force_update Logical. If \code{TRUE}, forces a new download of the
#'   file even if it exists in cache. Default \code{FALSE}.
#'
#' @return An \code{sf} (simple feature) object with POINT geometry containing
#'   educational service information, including:
#'
#'   \strong{Service Identification:}
#'   \itemize{
#'     \item POINT geometry (educational service coordinates)
#'     \item \code{cod_mod}: Modular code of the educational service (unique
#'       identifier assigned by MINEDU)
#'     \item \code{anexo}: Annex number of the educational service (0 if it is
#'       the main headquarters)
#'     \item \code{cen_edu}: Name of the educational center or institution
#'     \item \code{codinst}: Institution code (RUC or internal code)
#'   }
#'
#'   \strong{Educational Level and Modality:}
#'   \itemize{
#'     \item \code{d_niv_mod}: Educational service level and modality
#'     \item \code{d_forma}: Form of care:
#'       \itemize{
#'         \item ESCOLARIZADO: Classroom-based education
#'         \item NO ESCOLARIZADO: Non-classroom programs (PRONOEI, etc.)
#'         \item NO APLICA: Not applicable
#'       }
#'   }
#'
#'   \strong{Management and Dependency:}
#'   \itemize{
#'     \item \code{d_gestion}: Management type:
#'       \itemize{
#'         \item PÚBLICA DE GESTIÓN DIRECTA: Directly administered by the State
#'         \item PÚBLICA DE GESTIÓN PRIVADA: Public but privately administered
#'           (agreements)
#'         \item PRIVADA: Private administration
#'       }
#'     \item \code{d_ges_dep}: Specific management dependency
#'     \item \code{d_dreugel}: DRE or UGEL of administrative dependency
#'   }
#'
#'   \strong{Service Characteristics:}
#'   \itemize{
#'     \item \code{d_cod_car}: Teacher characteristic based on number of grades
#'       in charge:
#'       \itemize{
#'         \item POLIDOCENTE COMPLETO: One teacher per grade/section
#'         \item POLIDOCENTE MULTIGRADO: Teachers attend several grades
#'         \item UNIDOCENTE: A single teacher for all grades
#'         \item NO APLICA: Does not correspond to the educational level
#'         \item NO DISPONIBLE: Not available
#'       }
#'     \item \code{d_tipssexo}: Service type based on students' sex:
#'       \itemize{
#'         \item MIXTO: Attends both sexes
#'         \item VARONES: Males only
#'         \item MUJERES: Females only
#'         \item NO APLICA: Not applicable
#'       }
#'     \item \code{d_tipoprog}: Program type (for non-classroom services)
#'     \item \code{d_cod_tur}: Service shift
#'   }
#'
#'   \strong{Geographic Location:}
#'   \itemize{
#'     \item \code{departamen}: Department name
#'     \item \code{provincia}: Province name
#'     \item \code{nlat_ie}: Educational service latitude (Y coordinate)
#'     \item \code{nlong_ie}: Educational service longitude (X coordinate)
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
#'   \item Level: Educational service
#'   \item Application: Educational offer analysis, educational policy
#'     planning, and coverage studies by level and modality
#' }
#'
#' **Difference between educational facility and educational service:**
#' \itemize{
#'   \item \strong{Educational Facility}: Physical infrastructure (property) where
#'     services operate. See \code{\link{get_locales_educativos}}.
#'   \item \strong{Educational Service}: Pedagogical management unit that provides
#'     a specific level or modality of education. A facility may house
#'     multiple educational services.
#' }
#'
#' **Peruvian Educational System - Levels and Modalities:**
#'
#' \strong{Regular Basic Education (EBR):}
#' \itemize{
#'   \item \strong{Initial}: Nursery (0-2 years), Kindergarten (3-5 years)
#'   \item \strong{Primary}: 6 grades (6-11 years)
#'   \item \strong{Secondary}: 5 grades (12-16 years)
#' }
#'
#'
#' **Classification by teacher characteristic (d_cod_car):**
#' \itemize{
#'   \item \strong{Full Polidocente}: Each section has its own teacher.
#'     Typical in urban areas.
#'   \item \strong{Multigrade Polidocente}: Several teachers who attend more than
#'     one grade each.
#'   \strong{Unidocente}: A single teacher attends all grades.
#'     Frequent in rural areas with low enrollment.
#' }
#'
#' **Hierarchical Filtering:**
#' Filters are applied in cascade:
#' \enumerate{
#'   \item Specified departments are loaded first
#'   \item Then provinces are filtered (if specified)
#' }
#'
#' Cache is stored in: \code{tempdir()/DEMARCA_cache/servicios_educativos/}
#'
#' **NOTE:** Geometries are POINT type and represent the location
#' of each educational service.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_servicios_educativos()
#'
#' # Load educational services of a full department
#' serv_cusco <- get_servicios_educativos(departamento = "CUSCO")
#'
#' # Filter by specific province
#' serv_prov_cusco <- get_servicios_educativos(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Filter by specific province
#' serv_prov_urubamba <- get_servicios_educativos(
#'   departamento = "CUSCO",
#'   provincia = "URUBAMBA"
#' )
#'
#' # Load multiple departments
#' serv_sur <- get_servicios_educativos(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Map of educational services by level
#' serv_cusco |>
#'   filter(d_niv_mod %in% c("PRIMARIA", "SECUNDARIA", "INICIAL - JARDÍN")) |>
#'   ggplot() +
#'   geom_sf(aes(color = d_niv_mod), size = 1, alpha = 0.6) +
#'   scale_color_brewer(palette = "Set1", name = "Level") +
#'   labs(
#'     title = "Educational Services by Level - Cusco",
#'     subtitle = "Regular Basic Education",
#'     caption = "Source: MINEDU - Educational Institutions Registry"
#'   ) +
#'   theme_minimal()
#'
#' # Summary by level and modality
#' serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(d_niv_mod, sort = TRUE)
#'
#' # Summary by management type
#' serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(d_gestion, sort = TRUE)
#'
#' # Public vs private services
#' serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(d_ges_dep, sort = TRUE)
#'
#' # Filter only initial education
#' initial_serv <- serv_cusco |>
#'   filter(grepl("INICIAL", d_niv_mod))
#'
#' # Filter public secondary services
#' public_secondary_serv <- serv_cusco |>
#'   filter(
#'     d_niv_mod == "SECUNDARIA",
#'     d_gestion == "PÚBLICA DE GESTIÓN DIRECTA"
#'   )
#'
#' # Analysis of unidocente schools (rural)
#' unidocente_serv <- serv_cusco |>
#'   filter(d_cod_car == "UNIDOCENTE")
#'
#' # Map of unidocente schools
#' ggplot(unidocente_serv) +
#'   geom_sf(color = "darkred", size = 1.5, alpha = 0.7) +
#'   labs(
#'     title = "Unidocente Educational Services - Cusco",
#'     subtitle = "One teacher for all grades",
#'     caption = "Source: MINEDU"
#'   ) +
#'   theme_minimal()
#'
#' # Analysis by UGEL
#' serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(d_dreugel, sort = TRUE)
#'
#' # Distribution by attendance shift
#' serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(d_cod_tur, sort = TRUE)
#'
#' # Non-classroom services (PRONOEI, etc.)
#' non_classroom_serv <- serv_cusco |>
#'   filter(d_forma == "NO ESCOLARIZADO")
#'
#' # Coverage analysis by province and level
#' coverage <- serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   filter(d_niv_mod %in% c("PRIMARIA", "SECUNDARIA")) |>
#'   group_by(provincia, d_niv_mod) |>
#'   summarise(
#'     total_services = n(),
#'     pct_public = round(
#'       sum(d_gestion == "PÚBLICA DE GESTIÓN DIRECTA") / n() * 100, 1
#'     ),
#'     .groups = "drop"
#'   ) |>
#'   tidyr::pivot_wider(
#'     names_from = d_niv_mod,
#'     values_from = c(total_services, pct_public)
#'   )
#'
#' # Comparative map public vs private
#' serv_cusco |>
#'   filter(d_niv_mod == "SECUNDARIA") |>
#'   mutate(
#'     management_type = ifelse(
#'       grepl("PÚBLICA", d_gestion), "Public", "Private"
#'     )
#'   ) |>
#'   ggplot() +
#'   geom_sf(aes(color = management_type), size = 2, alpha = 0.6) +
#'   scale_color_manual(
#'     values = c("Public" = "darkblue", "Private" = "darkred"),
#'     name = "Management"
#'   ) +
#'   labs(
#'     title = "Secondary Services by Management Type - Cusco",
#'     caption = "Source: MINEDU"
#'   ) +
#'   theme_minimal()
#'
#' # CETPRO - Technical productive education
#' cetpro <- serv_cusco |>
#'   filter(grepl("TÉCNICO PRODUCTIVA", d_niv_mod))
#'
#' # Higher institutes
#' higher_institutes <- serv_cusco |>
#'   filter(grepl("SUPERIOR", d_niv_mod))
#' }
#'
#' @seealso
#' \code{\link{get_locales_educativos}}, \code{\link{get_departamentos}},
#' \code{\link{get_provincias}}, \code{\link{get_distritos}},
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
get_servicios_educativos <- function(departamento = NULL,
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
      "servicios_educativos_amazonas.gpkg",
      "servicios_educativos_ancash.gpkg",
      "servicios_educativos_apurimac.gpkg",
      "servicios_educativos_arequipa.gpkg",
      "servicios_educativos_ayacucho.gpkg",
      "servicios_educativos_cajamarca.gpkg",
      "servicios_educativos_callao.gpkg",
      "servicios_educativos_cusco.gpkg",
      "servicios_educativos_huancavelica.gpkg",
      "servicios_educativos_huanuco.gpkg",
      "servicios_educativos_ica.gpkg",
      "servicios_educativos_junin.gpkg",
      "servicios_educativos_la_libertad.gpkg",
      "servicios_educativos_lambayeque.gpkg",
      "servicios_educativos_lima.gpkg",
      "servicios_educativos_loreto.gpkg",
      "servicios_educativos_madre_de_dios.gpkg",
      "servicios_educativos_moquegua.gpkg",
      "servicios_educativos_pasco.gpkg",
      "servicios_educativos_piura.gpkg",
      "servicios_educativos_puno.gpkg",
      "servicios_educativos_san_martin.gpkg",
      "servicios_educativos_tacna.gpkg",
      "servicios_educativos_tumbes.gpkg",
      "servicios_educativos_ucayali.gpkg"
    ),
    id_osf = c(
      "zv64n", "wy42d", "kgq2p", "sujwm", "y8rnk",
      "wfc8b", "ndcyf", "9nkeb", "kebgs", "nb3cu",
      "v9g8d", "q76fj", "54hek", "6b9tm", "csyxd",
      "bv4t7", "xe5sj", "vmzfg", "9w2ne", "ywdgc",
      "s5mvt", "tk842", "qrdfb", "mj5gf", "38qaw"
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
    message("\nUsage: get_servicios_educativos(departamento = 'CUSCO')")
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
      "Use get_servicios_educativos() to see the full list.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "servicios_educativos")
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
            message("\u2713 Loaded: ", dep, " (", nrow(datos_dep), " educational services)")
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
          "No educational services found for the province(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtered by province: ",
          nrow(datos_final),
          " educational services"
        )
      }
    } else {
      warning("Column 'provincia' not found for filtering", call. = FALSE)
    }
  }

  # Filtro por DISTRITO
  # if (!is.null(distrito)) {
  #  # Nota: En la estructura proporcionada no se ve columna 'distrito'
  #  # Verificar si existe en los datos reales
  #  col_distrito <- intersect(c("distrito", "nombdist"), names(datos_final))

  #  if (length(col_distrito) > 0) {
  #    col_usar <- col_distrito[1]
  #    dist_input <- toupper(trimws(distrito))
  #    datos_final <- datos_final[toupper(datos_final[[col_usar]]) %in% dist_input, ]

  #    if (nrow(datos_final) == 0) {
  #      warning(
  #        "No se encontraron servicios educativos para el/los distrito(s): ",
  #        paste(distrito, collapse = ", "),
  #        call. = FALSE
  #      )
  #      return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
  #    }

  #    if (show_progress) {
  #      message(
  #        "\u2713 Filtrado por distrito: ",
  #        nrow(datos_final),
  #        " servicios educativos"
  #      )
  #    }
  #  } else {
  #    warning(
  #      "No se encontr\u00f3 columna de distrito para filtrar. ",
  #      "Los datos solo contienen: departamen, provincia",
  #      call. = FALSE
  #    )
  #  }
  # }

  # ==========================================================================
  # 7. MENSAJE FINAL
  # ==========================================================================

  if (show_progress) {
    message(
      "\u2713 Data loaded successfully: ",
      nrow(datos_final), " educational services"
    )
  }

  return(datos_final)
}
