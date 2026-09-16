#' Get Population Centers with Population Growth Rate
#'
#' Downloads and loads information on population centers with their average annual
#' intercensal population growth rate (2007-2017) from the DEMARCA OSF repository.
#' Data includes POINT type geometry and allows identifying changes in population
#' volume for each population center. Allows stepwise filtering by department,
#' province, and district.
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
#' @return An \code{sf} (simple feature) object with POINT type geometry containing
#'   information on population centers and their growth rate, including:
#'   \itemize{
#'     \item POINT geometry (coordinates of the population center)
#'     \item \code{nro}: Identification number
#'     \item \code{codccpp}: Population center code
#'     \item \code{ubigeo}: UBIGEO code of the district
#'     \item \code{dep}: Department name
#'     \item \code{prov}: Province name
#'     \item \code{distrito}: District name
#'     \item \code{centro_pob}: Population center name
#'     \item \code{pob_2007}: Population in the 2007 census
#'     \item \code{pob_2017}: Population in the 2017 census
#'     \item \code{tasa}: Average annual growth rate (%)
#'     \item \code{capital}: Capital type (Departmental, Provincial, Distrital, Not a capital)
#'     \item \code{region}: Natural region (Coast, Highland, Jungle)
#'     \item \code{urb_rural}: Urban/rural classification
#'     \item \code{tc_catg}: Growth rate category (POSITIVE, NEGATIVE)
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
#'   \item Source: INEI Population Censuses 2007 and 2017
#'   \item Level: Population center
#'   \item Variable: Average annual intercensal growth rate
#'   \item Application: Territorial analysis for district creations
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
#' Cache is stored in: \code{tools::R_user_dir("rsdot", which = "cache")/centros_poblados/}
#'
#' **NOTE:** Geometries are POINT type and represent the location of each population center.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_centros_poblados_crecimiento()
#'
#' # Load population centers of a full department
#' ccpp_cusco <- get_centros_poblados_crecimiento(departamento = "CUSCO")
#'
#' # Filter by specific province
#' ccpp_prov_cusco <- get_centros_poblados_crecimiento(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Filter by specific district
#' ccpp_wanchaq <- get_centros_poblados_crecimiento(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "WANCHAQ"
#' )
#'
#' # Load multiple departments
#' ccpp_sur <- get_centros_poblados_crecimiento(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Map of population centers by growth category
#' ggplot(ccpp_cusco) +
#'   geom_sf(aes(color = tc_catg, size = pob_2017), alpha = 0.6) +
#'   scale_color_manual(
#'     values = c("POSITIVO" = "darkgreen", "NEGATIVO" = "darkred"),
#'     name = "Growth"
#'   ) +
#'   scale_size_continuous(
#'     name = "Population 2017",
#'     range = c(0.5, 5)
#'   ) +
#'   labs(
#'     title = "Population Centers of the Department of Cusco",
#'     subtitle = "Population Growth Rate 2007-2017",
#'     caption = "Source: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Filter population centers by characteristics
#' ccpp_urbanos <- ccpp_cusco |>
#'   filter(urb_rural == "URBANA")
#'
#' ccpp_capitales <- ccpp_cusco |>
#'   filter(capital != "No es capital")
#'
#' # Population centers with highest growth
#' top_crecimiento <- ccpp_cusco |>
#'   arrange(desc(tasa)) |>
#'   head(10)
#'
#' ggplot(top_crecimiento) +
#'   geom_sf(aes(size = tasa, color = tasa)) +
#'   scale_color_gradient(low = "yellow", high = "red", name = "Rate (%)") +
#'   scale_size_continuous(range = c(3, 10), name = "Rate (%)") +
#'   geom_sf_text(
#'     aes(label = centro_pob),
#'     size = 2.5,
#'     nudge_y = 0.05
#'   ) +
#'   labs(
#'     title = "Top 10 Population Centers with Highest Growth",
#'     subtitle = "Department of Cusco (2007-2017)",
#'     caption = "Source: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Analysis by natural region
#' ccpp_cusco |>
#'   group_by(region, tc_catg) |>
#'   summarise(
#'     n_centros = n(),
#'     pob_total_2017 = sum(pob_2017, na.rm = TRUE),
#'     tasa_promedio = mean(tasa, na.rm = TRUE),
#'     .groups = "drop"
#'   )
#'
#' # Urban vs rural comparison
#' ggplot(ccpp_cusco) +
#'   geom_boxplot(aes(x = urb_rural, y = tasa, fill = urb_rural)) +
#'   scale_fill_brewer(palette = "Set2") +
#'   labs(
#'     title = "Distribution of Growth Rates",
#'     subtitle = "By urban and rural area - Cusco",
#'     x = "Classification",
#'     y = "Growth Rate (%)",
#'     caption = "Source: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal() +
#'   theme(legend.position = "none")
#' }
#'
#' @seealso
#' \code{\link{get_densidad_poblacional}}, \code{\link{get_departamentos}},
#' \code{\link{get_provincias}}, \code{\link{get_distritos}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' INEI. National Population and Housing Censuses 2007 and 2017.
#'
#' DEMARCA OSF Repository: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom utils head
get_centros_poblados_crecimiento <- function(departamento = NULL,
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
      "densidad_crecim_pobla_ccpp_amazonas.gpkg",
      "densidad_crecim_pobla_ccpp_ancash.gpkg",
      "densidad_crecim_pobla_ccpp_apurimac.gpkg",
      "densidad_crecim_pobla_ccpp_arequipa.gpkg",
      "densidad_crecim_pobla_ccpp_ayacucho.gpkg",
      "densidad_crecim_pobla_ccpp_cajamarca.gpkg",
      "densidad_crecim_pobla_ccpp_callao.gpkg",
      "densidad_crecim_pobla_ccpp_cusco.gpkg",
      "densidad_crecim_pobla_ccpp_huancavelica.gpkg",
      "densidad_crecim_pobla_ccpp_huanuco.gpkg",
      "densidad_crecim_pobla_ccpp_ica.gpkg",
      "densidad_crecim_pobla_ccpp_junin.gpkg",
      "densidad_crecim_pobla_ccpp_la_libertad.gpkg",
      "densidad_crecim_pobla_ccpp_lambayeque.gpkg",
      "densidad_crecim_pobla_ccpp_lima.gpkg",
      "densidad_crecim_pobla_ccpp_loreto.gpkg",
      "densidad_crecim_pobla_ccpp_madre_de_dios.gpkg",
      "densidad_crecim_pobla_ccpp_moquegua.gpkg",
      "densidad_crecim_pobla_ccpp_pasco.gpkg",
      "densidad_crecim_pobla_ccpp_piura.gpkg",
      "densidad_crecim_pobla_ccpp_puno.gpkg",
      "densidad_crecim_pobla_ccpp_san_martin.gpkg",
      "densidad_crecim_pobla_ccpp_tacna.gpkg",
      "densidad_crecim_pobla_ccpp_tumbes.gpkg",
      "densidad_crecim_pobla_ccpp_ucayali.gpkg"
    ),
    id_osf = c(
      "5pr47", "k63gh", "ux7ze", "53fkg", "puv8d",
      "vytuw", "8mkaw", "jpdtv", "8dmk6", "cb576",
      "gy8zp", "t76jr", "43gnq", "s3c9h", "7tpgu",
      "sdtzm", "unxy7", "dtmjc", "qfpdc", "fjmv4",
      "sfgwb", "a7jpx", "kuad6", "7hdcq", "g5fx8"
    ),
    stringsAsFactors = FALSE
  )

  # ==========================================================================
  # 2. PARAMETER VALIDATION
  # ==========================================================================

  if (!is.logical(show_progress) || length(show_progress) != 1) {
    stop("The 'show_progress' parameter must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.logical(force_update) || length(force_update) != 1) {
    stop("The 'force_update' parameter must be TRUE or FALSE", call. = FALSE)
  }

  # If no department is specified, show list
  if (is.null(departamento)) {
    message("Available departments:")
    message(paste0("  - ", tabla_departamentos$departamento, collapse = "\n"))
    message("\nUsage: get_centros_poblados_crecimiento(departamento = 'CUSCO')")
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
      "Use get_centros_poblados_crecimiento() to see the full list.",
      call. = FALSE
    )
  }

  # Filter table
  tabla_filtrada <- tabla_departamentos[
    tabla_departamentos$departamento %in% departamento_input,
  ]

  # ==========================================================================
  # 3. DIRECTORY SETUP
  # ==========================================================================

  directorio_cache <- file.path(get_cache_dir(), "centros_poblados")
  if (!dir.exists(directorio_cache)) {
    dir.create(directorio_cache, recursive = TRUE)
  }

  lista_datos <- list()

  # ==========================================================================
  # 4. DOWNLOAD AND LOAD DATA
  # ==========================================================================

  for (i in seq_len(nrow(tabla_filtrada))) {
    dep <- tabla_filtrada$departamento[i]
    file_name <- tabla_filtrada$file_name[i]
    id_osf <- tabla_filtrada$id_osf[i]

    url_descarga <- paste0("https://osf.io/download/", id_osf, "/")
    archivo_local <- file.path(directorio_cache, file_name)

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

          # Add department column if it doesn't exist
          if (!"dep" %in% names(datos_dep) && !"departamento" %in% names(datos_dep)) {
            datos_dep$dep <- dep
          }

          lista_datos[[dep]] <- datos_dep

          if (show_progress) {
            message("\u2713 Loaded: ", dep, " (", nrow(datos_dep), " population centers)")
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
  # 5. COMBINE DATA
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
        # Attempt with sf::st_as_sf if rbind fails
        stop(
          "Error combining data from multiple departments: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  # ==========================================================================
  # 6. FILTERS AFTER LOADING
  # ==========================================================================

  # Identify the correct department column name
  col_dep <- if ("dep" %in% names(datos_final)) "dep" else "departamento"

  # Filter by PROVINCE
  if (!is.null(provincia)) {
    if (!is.character(provincia)) {
      stop("The 'provincia' parameter must be a character vector or NULL",
        call. = FALSE
      )
    }
    if ("prov" %in% names(datos_final)) {
      prov_input <- toupper(trimws(provincia))
      datos_final <- datos_final[toupper(datos_final$prov) %in% prov_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No population centers found for the province(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtered by province: ",
          nrow(datos_final),
          " population centers"
        )
      }
    } else {
      warning("Column 'prov' not found for filtering", call. = FALSE)
    }
  }

  # Filter by DISTRICT
  if (!is.null(distrito)) {
    if (!is.character(distrito)) {
      stop("The 'distrito' parameter must be a character vector or NULL",
        call. = FALSE
      )
    }
    if ("distrito" %in% names(datos_final)) {
      dist_input <- toupper(trimws(distrito))
      datos_final <- datos_final[toupper(datos_final$distrito) %in% dist_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No population centers found for the district(s): ",
          paste(distrito, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtered by district: ",
          nrow(datos_final),
          " population centers"
        )
      }
    } else {
      warning("Column 'distrito' not found for filtering", call. = FALSE)
    }
  }

  # ==========================================================================
  # 7. FINAL RESULTS
  # ==========================================================================

  if (show_progress) {
    message(
      "\u2713 Data loaded successfully: ",
      nrow(datos_final), " population centers"
    )
  }

  return(datos_final)
}
