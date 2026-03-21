#' Get District Capitals of Peru
#'
#' Downloads and loads information on district capitals identified in the
#' 2017 Population and Housing Census of the National Institute of Statistics and
#' Informatics (INEI). Data includes demographic and geographic information of
#' the capitals at the departmental, provincial, and district levels, with POINT type
#' geometry.
#'
#' @param departamento Character vector. Name(s) of the department(s) to download.
#'   Valid options: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
#'   "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA",
#'   "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
#'   Case-insensitive. If \code{NULL}, shows the list of available departments.
#' @param provincia Character vector. Name(s) of province(s) to filter the
#'   results (optional). It is applied after loading the data.
#' @param distrito Character vector. Name(s) of district(s) to filter the
#'   results (optional). It is applied after loading the data.
#' @param show_progress Logical. If \code{TRUE} (default), shows informative
#'   messages about download progress. If \code{FALSE}, executes silently.
#' @param force_update Logical. If \code{TRUE}, forces a new download of the
#'   file even if it exists in cache. Default \code{FALSE}.
#'
#' @return An \code{sf} (simple feature) object with POINT type geometry
#'   containing district capital information, including:
#'   \itemize{
#'     \item POINT geometry (location of the capital)
#'     \item \code{gid}: Unique record identifier
#'     \item \code{ubigeo}: Geographic location code (6 digits)
#'     \item \code{nombdep}: Department name
#'     \item \code{nombprov}: Province name
#'     \item \code{nombdist}: District name
#'     \item \code{codccpp}: Population center code (10 digits)
#'     \item \code{cen_pob}: Population center name
#'     \item \code{pob}: Total population
#'     \item \code{capital}: Capital type (1=departmental, 2=provincial, 3=distrital)
#'   }
#'
#'   If multiple departments are requested, it returns a combined sf object.
#'
#' @details
#' The function downloads data from OSF (Open Science Framework) and stores it
#' in cache during the R session. Data is in GeoPackage (.gpkg) format.
#'
#' Cache is stored in: \code{tempdir()/DEMARCA_cache/capitales/}
#'
#' **NOTE:** Geometries are POINT type and represent the approximate geographic
#' location of the capitals according to the 2017 Census. Population data
#' correspond to the census year.
#'
#' @examples
#' \dontrun{
#' # See available departments
#' get_capitales()
#'
#' # Load capitals of a department
#' capitales_cusco <- get_capitales(departamento = "CUSCO")
#'
#' # Load multiple departments
#' capitales_sur <- get_capitales(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Filter by province
#' capitales_lima_prov <- get_capitales(
#'   departamento = "LIMA",
#'   provincia = "LIMA"
#' )
#'
#' # Filter by district
#' capital_cusco_dist <- get_capitales(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "CUSCO"
#' )
#'
#' # Visualization with ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Map of capitals by hierarchy
#' ggplot(capitales_cusco) +
#'   geom_sf(aes(color = factor(capital), size = pob), alpha = 0.7) +
#'   scale_color_manual(
#'     values = c("1" = "red", "2" = "blue", "3" = "green"),
#'     labels = c("Departmental", "Provincial", "Distrital"),
#'     name = "Capital Type"
#'   ) +
#'   scale_size_continuous(range = c(1, 10), name = "Population") +
#'   labs(
#'     title = "Capitals of Cusco by Hierarchy",
#'     subtitle = "2017 Population and Housing Census",
#'     caption = "Source: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Population analysis by capital type
#' capitales_cusco |>
#'   st_drop_geometry() |>
#'   group_by(capital) |>
#'   summarise(
#'     n = n(),
#'     pob_total = sum(pob, na.rm = TRUE),
#'     pob_promedio = mean(pob, na.rm = TRUE),
#'     pob_mediana = median(pob, na.rm = TRUE)
#'   ) |>
#'   arrange(capital)
#'
#' # Most populated capitals
#' capitales_cusco |>
#'   st_drop_geometry() |>
#'   arrange(desc(pob)) |>
#'   select(nombdist, cen_pob, pob, capital) |>
#'   head(10)
#'
#' # Filter only provincial capitals
#' caps_prov <- capitales_cusco |>
#'   filter(capital == 2)
#'
#' # Analysis by province
#' capitales_cusco |>
#'   st_drop_geometry() |>
#'   group_by(nombprov) |>
#'   summarise(
#'     n_capitales = n(),
#'     pob_total = sum(pob, na.rm = TRUE)
#'   ) |>
#'   arrange(desc(pob_total))
#' }
#'
#' @seealso
#' \code{\link{get_centros_poblados}}, \code{\link{get_departamentos}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' INEI. 2017 Population and Housing Census. National Institute of Statistics
#' and Informatics.
#'
#' SDOT Viewer: \url{https://geosdot.servicios.gob.pe/visor/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf st_sf st_sfc st_drop_geometry
#' @importFrom utils head
get_capitales <- function(departamento = NULL,
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
      "v_capitales_amazonas.gpkg",
      "v_capitales_ancash.gpkg",
      "v_capitales_apurimac.gpkg",
      "v_capitales_arequipa.gpkg",
      "v_capitales_ayacucho.gpkg",
      "v_capitales_cajamarca.gpkg",
      "v_capitales_callao.gpkg",
      "v_capitales_cusco.gpkg",
      "v_capitales_huancavelica.gpkg",
      "v_capitales_huanuco.gpkg",
      "v_capitales_ica.gpkg",
      "v_capitales_junin.gpkg",
      "v_capitales_la_libertad.gpkg",
      "v_capitales_lambayeque.gpkg",
      "v_capitales_lima.gpkg",
      "v_capitales_loreto.gpkg",
      "v_capitales_madre_de_dios.gpkg",
      "v_capitales_moquegua.gpkg",
      "v_capitales_pasco.gpkg",
      "v_capitales_piura.gpkg",
      "v_capitales_puno.gpkg",
      "v_capitales_san_martin.gpkg",
      "v_capitales_tacna.gpkg",
      "v_capitales_tumbes.gpkg",
      "v_capitales_ucayali.gpkg"
    ),
    id_osf = c(
      "tb4mf", "w6tcu", "9puye", "4vyg3", "kfjvr",
      "v53gu", "s8vbf", "23ba9", "zwb8y", "ugz9t",
      "d59q8", "a8squ", "ptd5g", "87cxu", "vugjf",
      "gfpuj", "rv7sh", "gyh2p", "cga8q", "mfekp",
      "m8xh3", "fye5t", "pjmce", "sy4na", "n3uzm"
    ),
    stringsAsFactors = FALSE
  )

  # ==========================================================================
  # 2. VALIDACIÓN DE PARÁMETROS
  # ==========================================================================

  if (!is.logical(show_progress) || length(show_progress) != 1) {
    stop("El par\u00e1metro 'show_progress' debe ser TRUE o FALSE", call. = FALSE)
  }

  if (!is.logical(force_update) || length(force_update) != 1) {
    stop("El par\u00e1metro 'force_update' debe ser TRUE o FALSE", call. = FALSE)
  }

  # Si no se especifica departamento, mostrar lista
  if (is.null(departamento)) {
    message("Departamentos disponibles:")
    message(paste0("  - ", tabla_departamentos$departamento, collapse = "\n"))
    message("\nUso: get_capitales(departamento = 'CUSCO')")
    return(invisible(tabla_departamentos$departamento))
  }

  if (!is.character(departamento)) {
    stop("El par\u00e1metro 'departamento' debe ser un vector de caracteres",
      call. = FALSE
    )
  }

  # Normalizar nombres (manejar "LA LIBERTAD", "SAN MARTIN", "MADRE DE DIOS")
  departamento_input <- toupper(trimws(departamento))
  departamento_input <- gsub("_", " ", departamento_input)

  # Validar departamentos
  departamentos_validos <- departamento_input %in% tabla_departamentos$departamento

  if (!all(departamentos_validos)) {
    departamentos_invalidos <- departamento_input[!departamentos_validos]
    stop(
      "Departamento(s) no v\u00e1lido(s): ",
      paste(departamentos_invalidos, collapse = ", "), "\n",
      "Use get_capitales() para ver la lista completa.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "capitales")
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

    # Descargar si no existe o si se fuerza
    if (!file.exists(archivo_local) || force_update) {
      if (show_progress) message("Descargando: ", dep, "...")

      # Intentar descarga con timeout extendido
      timeout_original <- getOption("timeout")
      options(timeout = 300) # 5 minutos

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
            "Error al descargar ", dep, ": ", e$message, "\n",
            "URL: ", url_descarga, "\n",
            "Puede que el archivo no est\u00e9 disponible temporalmente.",
            call. = FALSE,
            immediate. = TRUE
          )
          FALSE
        }
      )

      options(timeout = timeout_original)

      if (descarga_exitosa && show_progress) {
        message("\u2713 Descarga completada: ", dep)
      } else if (!descarga_exitosa) {
        next # Saltar a siguiente departamento
      }
    } else {
      if (show_progress) message("\u2713 Usando cach\u00e9: ", dep)
    }

    # Leer GeoPackage
    if (file.exists(archivo_local)) {
      if (show_progress) message("Cargando datos: ", dep, "...")

      tryCatch(
        {
          datos_dep <- sf::read_sf(archivo_local, quiet = TRUE)

          # Aplicar corrección de codificación UTF-8
          cols_char <- names(datos_dep)[sapply(datos_dep, is.character)]
          for (col in cols_char) {
            if (col != "geom" && col != "geometry") {
              datos_dep[[col]] <- fix_double_utf8(datos_dep[[col]])
            }
          }

          # Normalizar nombres de columnas
          colnames(datos_dep) <- tolower(colnames(datos_dep))

          lista_datos[[dep]] <- datos_dep

          if (show_progress) {
            n_capitales <- nrow(datos_dep)
            pob_total <- sum(datos_dep$pob, na.rm = TRUE)
            message(
              "\u2713 Cargado: ", dep,
              " (", n_capitales, " capitales, ",
              format(pob_total, big.mark = ","), " habitantes)"
            )
          }
        },
        error = function(e) {
          warning(
            "Error al leer archivo para ", dep, ": ", e$message,
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
    stop("No se pudo cargar ning\u00fan departamento", call. = FALSE)
  }

  if (length(lista_datos) == 1) {
    datos_final <- lista_datos[[1]]
  } else {
    if (show_progress) message("Combinando datos de m\u00faltiples departamentos...")

    # Usar rbind con manejo de errores
    tryCatch(
      {
        datos_final <- do.call(rbind, lista_datos)
      },
      error = function(e) {
        stop(
          "Error al combinar datos de m\u00faltiples departamentos: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  # ==========================================================================
  # 6. FILTROS POST-CARGA
  # ==========================================================================

  # Filtrar por provincia si se especifica
  if (!is.null(provincia)) {
    if (!is.character(provincia)) {
      stop("El par\u00e1metro 'provincia' debe ser un vector de caracteres",
        call. = FALSE
      )
    }

    provincia_input <- toupper(trimws(provincia))
    provincia_input <- gsub("_", " ", provincia_input)

    datos_final <- datos_final[
      toupper(datos_final$nombprov) %in% provincia_input,
    ]

    if (nrow(datos_final) == 0) {
      warning(
        "No se encontraron datos para la(s) provincia(s) especificada(s): ",
        paste(provincia_input, collapse = ", "),
        call. = FALSE,
        immediate. = TRUE
      )
    }
  }

  # Filtrar por distrito si se especifica
  if (!is.null(distrito)) {
    if (!is.character(distrito)) {
      stop("El par\u00e1metro 'distrito' debe ser un vector de caracteres",
        call. = FALSE
      )
    }

    distrito_input <- toupper(trimws(distrito))
    distrito_input <- gsub("_", " ", distrito_input)

    datos_final <- datos_final[
      toupper(datos_final$nombdist) %in% distrito_input,
    ]

    if (nrow(datos_final) == 0) {
      warning(
        "No se encontraron datos para el/los distrito(s) especificado(s): ",
        paste(distrito_input, collapse = ", "),
        call. = FALSE,
        immediate. = TRUE
      )
    }
  }

  # ==========================================================================
  # 7. MENSAJE FINAL
  # ==========================================================================

  if (show_progress) {
    n_capitales <- nrow(datos_final)
    pob_total <- sum(datos_final$pob, na.rm = TRUE)
    n_tipo <- table(datos_final$capital)

    message(
      "\u2713 Datos cargados exitosamente: ",
      n_capitales, " capitales (",
      format(pob_total, big.mark = ","), " habitantes totales)"
    )

    if (length(n_tipo) > 0) {
      tipos_msg <- paste(
        sprintf("%d tipo %d", n_tipo, as.numeric(names(n_tipo))),
        collapse = ", "
      )
      message("  Tipos: ", tipos_msg)
    }
  }

  return(datos_final)
}
