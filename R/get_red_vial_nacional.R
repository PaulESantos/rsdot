#' Obtener Red Vial Nacional del Perú
#'
#' Descarga y carga la información de la Red Vial Nacional actualizada por
#' PROVÍAS NACIONAL a julio 2022, según Clasificador de Rutas aprobado mediante
#' Decreto Supremo 011-2016-MTC y sus modificatorias. Los datos incluyen geometría
#' tipo MULTILINESTRING e información detallada sobre trayectorias, clasificación,
#' estado, superficie y concesiones de las carreteras nacionales.
#'
#' @param departamento Character vector. Nombre(s) del/los departamento(s) a descargar.
#'   Opciones válidas: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
#'   "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA",
#'   "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
#'   No distingue entre mayúsculas y minúsculas. Si es \code{NULL}, muestra la
#'   lista de departamentos disponibles.
#' @param show_progress Logical. Si \code{TRUE} (por defecto), muestra mensajes
#'   informativos sobre el progreso de la descarga. Si \code{FALSE}, ejecuta de
#'   forma silenciosa.
#' @param force_update Logical. Si \code{TRUE}, fuerza una nueva descarga del
#'   archivo incluso si existe en caché. Por defecto \code{FALSE}.
#'
#' @return Un objeto \code{sf} (simple feature) con geometría tipo MULTILINESTRING
#'   que contiene información de la red vial nacional, incluyendo:
#'   \itemize{
#'     \item Geometría MULTILINESTRING (trayectoria de la vía)
#'     \item \code{inicio}: Kilómetro de inicio del tramo
#'     \item \code{fin}: Kilómetro de fin del tramo
#'     \item \code{trayectori}: Descripción completa de la trayectoria de la ruta
#'     \item \code{nrocarril}: Número de carriles
#'     \item \code{ejeclas}: Eje de clasificación (Longitudinal de la Costa, Transversal, etc.)
#'     \item \code{iddpto}: Código del departamento
#'     \item \code{nombdep}: Nombre del departamento
#'     \item \code{codruta}: Código de la ruta (ej. PE-1S, PE-3N, PE-22A)
#'     \item \code{jerarq}: Jerarquía de la ruta (RN = Red Nacional)
#'     \item \code{superfic}: Código de tipo de superficie (1-11)
#'     \item \code{longitud}: Longitud del tramo en kilómetros
#'     \item \code{codconces}: Código de concesión (si aplica)
#'     \item \code{codclog}: Código de corredor logístico
#'     \item \code{superfic_l}: Tipo de superficie descriptivo (Pavimentado, Afirmado, etc.)
#'     \item \code{codclog_l}: Descripción del corredor logístico
#'     \item \code{estado}: Estado numérico de la vía (1-3)
#'     \item \code{estado_l}: Estado descriptivo (Bueno, Regular, Malo)
#'   }
#'
#'   Si se solicitan múltiples departamentos, retorna un objeto sf combinado.
#'
#' @details
#' La función descarga datos desde OSF (Open Science Framework) y los almacena
#' en caché durante la sesión de R. Los datos están en formato GeoPackage (.gpkg).
#'
#' **Fuente de los datos:**
#' \itemize{
#'   \item Fuente: PROVÍAS NACIONAL - Ministerio de Transportes y Comunicaciones
#'   \item Actualización: Julio 2022
#'   \item Base legal: Decreto Supremo 011-2016-MTC y modificatorias
#'   \item Nivel: Red Vial Nacional (Sistema Nacional de Carreteras - SINAC)
#'   \item Aplicación: Planificación vial nacional y análisis de conectividad
#' }
#'
#' **Clasificación de superficie:**
#' \itemize{
#'   \item 1 = Pavimentado
#'   \item 2 = Afirmado
#'   \item 11 = Asfaltado económico
#'   \item Otros códigos según especificaciones técnicas de PROVÍAS
#' }
#'
#' **Clasificación de estado:**
#' \itemize{
#'   \item 1 = Bueno
#'   \item 2 = Regular
#'   \item 3 = Malo
#' }
#'
#' **Ejes de clasificación (ejeclas):**
#' \itemize{
#'   \item Longitudinal de la Costa (PE-1N, PE-1S)
#'   \item Longitudinal de la Sierra
#'   \item Longitudinal de la Selva
#'   \item Transversal
#' }
#'
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/red_vial_nacional/}
#'
#' **NOTA:** Las geometrías son tipo MULTILINESTRING (multilíneas) y representan
#' las trayectorias de las carreteras nacionales. Algunos tramos pueden estar
#' bajo concesión (campo \code{codconces}).
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_red_vial_nacional()
#'
#' # Cargar red vial nacional de un departamento
#' vias_cusco <- get_red_vial_nacional(departamento = "CUSCO")
#'
#' # Cargar múltiples departamentos
#' vias_sur <- get_red_vial_nacional(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Mapa de red vial nacional por tipo de superficie
#' ggplot(vias_cusco) +
#'   geom_sf(aes(color = superfic_l), linewidth = 1) +
#'   scale_color_manual(
#'     values = c(
#'       "Pavimentado" = "darkblue",
#'       "Afirmado" = "darkgreen",
#'       "Asfaltado económico" = "purple"
#'     ),
#'     name = "Superficie"
#'   ) +
#'   labs(
#'     title = "Red Vial Nacional de Cusco",
#'     subtitle = "Por Tipo de Superficie - PROVÍAS 2022",
#'     caption = "Fuente: PROVÍAS NACIONAL | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Mapa por estado de conservación
#' ggplot(vias_cusco) +
#'   geom_sf(aes(color = estado_l, linewidth = estado_l)) +
#'   scale_color_manual(
#'     values = c(
#'       "Bueno" = "darkgreen",
#'       "Regular" = "orange",
#'       "Malo" = "red"
#'     ),
#'     name = "Estado"
#'   ) +
#'   scale_linewidth_manual(
#'     values = c("Bueno" = 1.2, "Regular" = 0.8, "Malo" = 0.6),
#'     name = "Estado"
#'   ) +
#'   labs(
#'     title = "Estado de Conservación - Red Vial Nacional",
#'     subtitle = "Departamento de Cusco",
#'     caption = "Fuente: PROVÍAS NACIONAL | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Mapa por código de ruta
#' ggplot(vias_cusco) +
#'   geom_sf(aes(color = codruta), linewidth = 1.2) +
#'   labs(
#'     title = "Red Vial Nacional - Códigos de Ruta",
#'     subtitle = "Cusco",
#'     color = "Código de Ruta",
#'     caption = "Fuente: PROVÍAS NACIONAL"
#'   ) +
#'   theme_minimal()
#'
#' # Análisis de longitud por tipo de superficie
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
#' # Análisis por eje de clasificación
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   group_by(ejeclas, estado_l) |>
#'   summarise(
#'     longitud_total = sum(longitud, na.rm = TRUE),
#'     .groups = "drop"
#'   ) |>
#'   arrange(ejeclas, desc(longitud_total))
#'
#' # Filtrar vías concesionadas
#' vias_concesion <- vias_cusco |>
#'   filter(!is.na(codconces))
#'
#' # Análisis de corredores logísticos
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   filter(!is.na(codclog_l)) |>
#'   group_by(codclog_l) |>
#'   summarise(
#'     longitud_total = sum(longitud, na.rm = TRUE),
#'     n_tramos = n(),
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(longitud_total))
#'
#' # Análisis por número de carriles
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   group_by(nrocarril) |>
#'   summarise(
#'     longitud_total = sum(longitud, na.rm = TRUE),
#'     pct = (longitud_total / sum(vias_cusco$longitud, na.rm = TRUE)) * 100,
#'     .groups = "drop"
#'   )
#'
#' # Principales rutas nacionales
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   group_by(codruta) |>
#'   summarise(
#'     longitud_total = sum(longitud, na.rm = TRUE),
#'     km_inicio = min(inicio, na.rm = TRUE),
#'     km_fin = max(fin, na.rm = TRUE),
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(longitud_total))
#'
#' # Visualizar solo carreteras longitudinales
#' vias_long <- vias_cusco |>
#'   filter(grepl("Longitudinal", ejeclas))
#'
#' ggplot(vias_long) +
#'   geom_sf(aes(color = ejeclas), linewidth = 1.5) +
#'   labs(
#'     title = "Carreteras Longitudinales",
#'     subtitle = "Red Vial Nacional - Cusco",
#'     color = "Eje de Clasificación"
#'   ) +
#'   theme_minimal()
#'
#' # Estadísticas generales
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   summarise(
#'     longitud_total = sum(longitud, na.rm = TRUE),
#'     n_rutas = n_distinct(codruta),
#'     km_pavimentado = sum(longitud[superfic_l == "Pavimentado"], na.rm = TRUE),
#'     km_afirmado = sum(longitud[superfic_l == "Afirmado"], na.rm = TRUE),
#'     pct_pavimentado = (km_pavimentado / longitud_total) * 100,
#'     km_buen_estado = sum(longitud[estado_l == "Bueno"], na.rm = TRUE),
#'     pct_buen_estado = (km_buen_estado / longitud_total) * 100
#'   )
#' }
#'
#' @seealso
#' \code{\link{get_red_vial_departamental}}, \code{\link{get_departamentos}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' PROVÍAS NACIONAL - MTC. Base Cartográfica de la Red Vial Nacional 2022.
#'
#' Decreto Supremo 011-2016-MTC - Clasificador de Rutas del Sistema Nacional de
#' Carreteras (SINAC).
#'
#' Repositorio DEMARCA en OSF: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf st_sf st_sfc st_drop_geometry
#' @importFrom utils head
get_red_vial_nacional <- function(departamento = NULL,
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
      "red_vial_nacional_amazonas.gpkg",
      "red_vial_nacional_ancash.gpkg",
      "red_vial_nacional_apurimac.gpkg",
      "red_vial_nacional_arequipa.gpkg",
      "red_vial_nacional_ayacucho.gpkg",
      "red_vial_nacional_cajamarca.gpkg",
      "red_vial_nacional_callao.gpkg",
      "red_vial_nacional_cusco.gpkg",
      "red_vial_nacional_huancavelica.gpkg",
      "red_vial_nacional_huanuco.gpkg",
      "red_vial_nacional_ica.gpkg",
      "red_vial_nacional_junin.gpkg",
      "red_vial_nacional_la_libertad.gpkg",
      "red_vial_nacional_lambayeque.gpkg",
      "red_vial_nacional_lima.gpkg",
      "red_vial_nacional_loreto.gpkg",
      "red_vial_nacional_madre_de_dios.gpkg",
      "red_vial_nacional_moquegua.gpkg",
      "red_vial_nacional_pasco.gpkg",
      "red_vial_nacional_piura.gpkg",
      "red_vial_nacional_puno.gpkg",
      "red_vial_nacional_san_martin.gpkg",
      "red_vial_nacional_tacna.gpkg",
      "red_vial_nacional_tumbes.gpkg",
      "red_vial_nacional_ucayali.gpkg"
    ),
    id_osf = c(
      "w5jt2", "n2j4d", "ngqva", "8b6nh", "wjv57",
      "j72un", "rtxk6", "8qh4b", "3bgpf", "knzjs",
      "ycdez", "mya95", "9hcsg", "ud4fk", "t6j9a",
      "qr675", "92pag", "c6f5b", "2kwsy", "54dnw",
      "9fnqp", "7w5tn", "f4bj7", "msbj5", "5ch8q"
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
    message("\nUso: get_red_vial_nacional(departamento = 'CUSCO')")
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
      "Use get_red_vial_nacional() para ver la lista completa.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "red_vial_nacional")
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

          lista_datos[[dep]] <- datos_dep

          if (show_progress) {
            longitud_total <- sum(datos_dep$longitud, na.rm = TRUE)
            n_rutas <- length(unique(datos_dep$codruta))
            message(
              "\u2713 Cargado: ", dep,
              " (", nrow(datos_dep), " tramos, ",
              n_rutas, " rutas, ",
              round(longitud_total, 1), " km)"
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
  # 6. MENSAJE FINAL
  # ==========================================================================

  if (show_progress) {
    longitud_total <- sum(datos_final$longitud, na.rm = TRUE)
    n_rutas <- length(unique(datos_final$codruta))
    message(
      "\u2713 Datos cargados exitosamente: ",
      nrow(datos_final), " tramos (",
      n_rutas, " rutas nacionales, ",
      round(longitud_total, 1), " km totales)"
    )
  }

  return(datos_final)
}
