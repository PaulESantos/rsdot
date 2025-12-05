#' Obtener Centros Poblados con Tasa de Crecimiento Poblacional
#'
#' Descarga y carga la información de centros poblados con su tasa media de
#' crecimiento anual poblacional intercensal (2007-2017) desde el repositorio
#' OSF de DEMARCA. Los datos incluyen geometría tipo POINT y permiten identificar
#' los cambios de volumen poblacional de cada centro poblado. Permite filtrado
#' escalonado por departamento, provincia y distrito.
#'
#' @param departamento Character vector. Nombre(s) del/los departamento(s) a descargar.
#'   Opciones válidas: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
#'   "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA",
#'   "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
#'   No distingue entre mayúsculas y minúsculas. Si es \code{NULL}, muestra la
#'   lista de departamentos disponibles.
#' @param provincia Character vector. Nombre(s) de la(s) provincia(s) para filtrar
#'   dentro del/los departamento(s) especificado(s). Opcional.
#' @param distrito Character vector. Nombre(s) del/los distrito(s) para filtrar
#'   dentro de la(s) provincia(s) especificada(s). Opcional.
#' @param show_progress Logical. Si \code{TRUE} (por defecto), muestra mensajes
#'   informativos sobre el progreso de la descarga. Si \code{FALSE}, ejecuta de
#'   forma silenciosa.
#' @param force_update Logical. Si \code{TRUE}, fuerza una nueva descarga del
#'   archivo incluso si existe en caché. Por defecto \code{FALSE}.
#'
#' @return Un objeto \code{sf} (simple feature) con geometría tipo POINT que contiene
#'   información de los centros poblados y su tasa de crecimiento, incluyendo:
#'   \itemize{
#'     \item Geometría POINT (coordenadas del centro poblado)
#'     \item \code{nro}: Número de identificación
#'     \item \code{codccpp}: Código de centro poblado
#'     \item \code{ubigeo}: Código UBIGEO del distrito
#'     \item \code{dep}: Nombre del departamento
#'     \item \code{prov}: Nombre de la provincia
#'     \item \code{distrito}: Nombre del distrito
#'     \item \code{centro_pob}: Nombre del centro poblado
#'     \item \code{pob_2007}: Población en el censo 2007
#'     \item \code{pob_2017}: Población en el censo 2017
#'     \item \code{tasa}: Tasa media de crecimiento anual (%)
#'     \item \code{capital}: Tipo de capital (Departamental, Provincial, Distrital, No es capital)
#'     \item \code{region}: Región natural (Costa, Sierra, Selva)
#'     \item \code{urb_rural}: Clasificación urbano/rural
#'     \item \code{tc_catg}: Categoría de tasa de crecimiento (POSITIVO, NEGATIVO)
#'   }
#'
#'   Si se solicitan múltiples departamentos, retorna un objeto sf combinado.
#'
#' @details
#' La función descarga datos desde OSF (Open Science Framework) y los almacena
#' en caché durante la sesión de R. Los datos están en formato GeoPackage (.gpkg).
#'
#' **Metodología de los datos:**
#' \itemize{
#'   \item Fuente: Censos de Población INEI 2007 y 2017
#'   \item Nivel: Centro poblado
#'   \item Variable: Tasa media de crecimiento anual intercensal
#'   \item Aplicación: Análisis territorial para creaciones distritales
#' }
#'
#' **Filtrado jerárquico:**
#' Los filtros se aplican en cascada:
#' \enumerate{
#'   \item Primero se cargan los departamentos especificados
#'   \item Luego se filtran las provincias (si se especifican)
#'   \item Finalmente se filtran los distritos (si se especifican)
#' }
#'
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/centros_poblados/}
#'
#' **NOTA:** Las geometrías son tipo POINT (puntos) y representan la ubicación
#' de cada centro poblado.
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_centros_poblados_crecimiento()
#'
#' # Cargar centros poblados de un departamento completo
#' ccpp_cusco <- get_centros_poblados_crecimiento(departamento = "CUSCO")
#'
#' # Filtrar por provincia específica
#' ccpp_prov_cusco <- get_centros_poblados_crecimiento(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Filtrar por distrito específico
#' ccpp_wanchaq <- get_centros_poblados_crecimiento(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "WANCHAQ"
#' )
#'
#' # Cargar múltiples departamentos
#' ccpp_sur <- get_centros_poblados_crecimiento(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Mapa de centros poblados por categoría de crecimiento
#' ggplot(ccpp_cusco) +
#'   geom_sf(aes(color = tc_catg, size = pob_2017), alpha = 0.6) +
#'   scale_color_manual(
#'     values = c("POSITIVO" = "darkgreen", "NEGATIVO" = "darkred"),
#'     name = "Crecimiento"
#'   ) +
#'   scale_size_continuous(
#'     name = "Población 2017",
#'     range = c(0.5, 5)
#'   ) +
#'   labs(
#'     title = "Centros Poblados del Departamento de Cusco",
#'     subtitle = "Tasa de Crecimiento Poblacional 2007-2017",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Filtrar centros poblados por características
#' ccpp_urbanos <- ccpp_cusco |>
#'   filter(urb_rural == "URBANA")
#'
#' ccpp_capitales <- ccpp_cusco |>
#'   filter(capital != "No es capital")
#'
#' # Centros poblados con mayor crecimiento
#' top_crecimiento <- ccpp_cusco |>
#'   arrange(desc(tasa)) |>
#'   head(10)
#'
#' ggplot(top_crecimiento) +
#'   geom_sf(aes(size = tasa, color = tasa)) +
#'   scale_color_gradient(low = "yellow", high = "red", name = "Tasa (%)") +
#'   scale_size_continuous(range = c(3, 10), name = "Tasa (%)") +
#'   geom_sf_text(
#'     aes(label = centro_pob),
#'     size = 2.5,
#'     nudge_y = 0.05
#'   ) +
#'   labs(
#'     title = "Top 10 Centros Poblados con Mayor Crecimiento",
#'     subtitle = "Departamento de Cusco (2007-2017)",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Análisis por región natural
#' ccpp_cusco |>
#'   group_by(region, tc_catg) |>
#'   summarise(
#'     n_centros = n(),
#'     pob_total_2017 = sum(pob_2017, na.rm = TRUE),
#'     tasa_promedio = mean(tasa, na.rm = TRUE),
#'     .groups = "drop"
#'   )
#'
#' # Comparación urbano vs rural
#' ggplot(ccpp_cusco) +
#'   geom_boxplot(aes(x = urb_rural, y = tasa, fill = urb_rural)) +
#'   scale_fill_brewer(palette = "Set2") +
#'   labs(
#'     title = "Distribución de Tasas de Crecimiento",
#'     subtitle = "Por área urbana y rural - Cusco",
#'     x = "Clasificación",
#'     y = "Tasa de Crecimiento (%)",
#'     caption = "Fuente: INEI | Visor - SDOT"
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
#' INEI. Censos Nacionales de Población y Vivienda 2007 y 2017.
#'
#' Repositorio DEMARCA en OSF: \url{https://osf.io/qy4j6/}
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
  # 2. VALIDACIÓN DE PARÁMETROS
  # ==========================================================================

  if (!is.logical(show_progress) || length(show_progress) != 1) {
    stop("El par\u00e1metro 'show_progress' debe ser TRUE o FALSE", call. = FALSE)
  }

  if (!is.logical(force_update) || length(force_update) != 1) {
    stop("El par\u00e1metro 'force_update' debe ser TRUE o FALSE", call. = FALSE)
  }

  if (!is.null(provincia) && !is.character(provincia)) {
    stop("El par\u00e1metro 'provincia' debe ser un vector de caracteres o NULL",
         call. = FALSE)
  }

  if (!is.null(distrito) && !is.character(distrito)) {
    stop("El par\u00e1metro 'distrito' debe ser un vector de caracteres o NULL",
         call. = FALSE)
  }

  # Si no se especifica departamento, mostrar lista
  if (is.null(departamento)) {
    message("Departamentos disponibles:")
    message(paste0("  - ", tabla_departamentos$departamento, collapse = "\n"))
    message("\nUso: get_centros_poblados_crecimiento(departamento = 'CUSCO')")
    return(invisible(tabla_departamentos$departamento))
  }

  if (!is.character(departamento)) {
    stop("El par\u00e1metro 'departamento' debe ser un vector de caracteres",
         call. = FALSE)
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
      "Use get_centros_poblados_crecimiento() para ver la lista completa.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "centros_poblados")
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
      options(timeout = 300)  # 5 minutos

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
        next  # Saltar a siguiente departamento
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

          # Agregar columna departamento si no existe
          if (!"dep" %in% names(datos_dep) && !"departamento" %in% names(datos_dep)) {
            datos_dep$dep <- dep
          }

          lista_datos[[dep]] <- datos_dep

          if (show_progress) {
            message("\u2713 Cargado: ", dep, " (", nrow(datos_dep), " centros poblados)")
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
        # Intentar con sf::st_as_sf si rbind falla
        stop(
          "Error al combinar datos de m\u00faltiples departamentos: ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  # ==========================================================================
  # 6. FILTRADO POR PROVINCIA Y DISTRITO
  # ==========================================================================

  # Identificar el nombre correcto de la columna de departamento
  col_dep <- if ("dep" %in% names(datos_final)) "dep" else "departamento"

  # Filtro por PROVINCIA
  if (!is.null(provincia)) {
    if ("prov" %in% names(datos_final)) {
      prov_input <- toupper(trimws(provincia))
      datos_final <- datos_final[toupper(datos_final$prov) %in% prov_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No se encontraron centros poblados para la(s) provincia(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por provincia: ",
          nrow(datos_final),
          " centros poblados"
        )
      }
    } else {
      warning("No se encontr\u00f3 la columna 'prov' para filtrar", call. = FALSE)
    }
  }

  # Filtro por DISTRITO
  if (!is.null(distrito)) {
    if ("distrito" %in% names(datos_final)) {
      dist_input <- toupper(trimws(distrito))
      datos_final <- datos_final[toupper(datos_final$distrito) %in% dist_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No se encontraron centros poblados para el/los distrito(s): ",
          paste(distrito, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por distrito: ",
          nrow(datos_final),
          " centros poblados"
        )
      }
    } else {
      warning("No se encontr\u00f3 la columna 'distrito' para filtrar", call. = FALSE)
    }
  }

  # ==========================================================================
  # 7. MENSAJE FINAL
  # ==========================================================================

  if (show_progress) {
    message(
      "\u2713 Datos cargados exitosamente: ",
      nrow(datos_final), " centros poblados"
    )
  }

  return(datos_final)
}
