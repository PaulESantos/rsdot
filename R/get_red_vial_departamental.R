#' Obtener Red Vial Departamental del Perú
#'
#' Descarga y carga la información de la Red Vial Departamental actualizada por
#' PROVÍAS NACIONAL a julio 2022, según Clasificador de Rutas aprobado mediante
#' Decreto Supremo 011-2016-MTC y sus modificatorias. Los datos incluyen geometría
#' tipo MULTILINESTRING e información sobre trayectorias, estado y superficie de
#' las carreteras departamentales. Permite filtrado escalonado por departamento
#' y provincia.
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
#' @param show_progress Logical. Si \code{TRUE} (por defecto), muestra mensajes
#'   informativos sobre el progreso de la descarga. Si \code{FALSE}, ejecuta de
#'   forma silenciosa.
#' @param force_update Logical. Si \code{TRUE}, fuerza una nueva descarga del
#'   archivo incluso si existe en caché. Por defecto \code{FALSE}.
#'
#' @return Un objeto \code{sf} (simple feature) con geometría tipo MULTILINESTRING
#'   que contiene información de la red vial departamental, incluyendo:
#'   \itemize{
#'     \item Geometría MULTILINESTRING (trayectoria de la vía)
#'     \item \code{nombdep}: Nombre del departamento
#'     \item \code{nombprov}: Nombre de la provincia
#'     \item \code{trayectori}: Descripción de la trayectoria de la ruta
#'     \item \code{iddpto}: Código del departamento
#'     \item \code{jerarq}: Jerarquía de la ruta (RD = Red Departamental)
#'     \item \code{estado}: Estado numérico de la vía (1, 2, 3, 4)
#'     \item \code{codruta}: Código de la ruta (ej. PI-102, CU-105)
#'     \item \code{superfic}: Código de tipo de superficie (1-4)
#'     \item \code{longitud}: Longitud del tramo en kilómetros
#'     \item \code{estado_l}: Estado descriptivo (BUENO, REGULAR, MALO)
#'     \item \code{superfic_l}: Tipo de superficie (PAVIMENTADO, AFIRMADO, SIN AFIRMAR, TROCHA)
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
#'   \item Nivel: Red Vial Departamental
#'   \item Aplicación: Planificación vial y análisis de conectividad territorial
#' }
#'
#' **Clasificación de superficie:**
#' \itemize{
#'   \item 1 = PAVIMENTADO
#'   \item 2 = AFIRMADO
#'   \item 3 = SIN AFIRMAR
#'   \item 4 = TROCHA
#' }
#'
#' **Clasificación de estado:**
#' \itemize{
#'   \item 1 = BUENO
#'   \item 2 = REGULAR
#'   \item 3 = MALO
#'   \item 4 = MUY MALO
#' }
#'
#' **Filtrado jerárquico:**
#' Los filtros se aplican en cascada:
#' \enumerate{
#'   \item Primero se cargan los departamentos especificados
#'   \item Luego se filtran las provincias (si se especifican)
#' }
#'
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/red_vial_departamental/}
#'
#' **NOTA:** Las geometrías son tipo MULTILINESTRING (multilíneas) y representan
#' las trayectorias de las carreteras departamentales.
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_red_vial_departamental()
#'
#' # Cargar red vial de un departamento completo
#' vias_cusco <- get_red_vial_departamental(departamento = "CUSCO")
#'
#' # Filtrar por provincia específica
#' vias_cusco_prov <- get_red_vial_departamental(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Cargar múltiples departamentos
#' vias_sur <- get_red_vial_departamental(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Mapa de red vial por tipo de superficie
#' ggplot(vias_cusco) +
#'   geom_sf(aes(color = superfic_l), linewidth = 0.8) +
#'   scale_color_manual(
#'     values = c(
#'       "PAVIMENTADO" = "darkblue",
#'       "AFIRMADO" = "darkgreen",
#'       "SIN AFIRMAR" = "orange",
#'       "TROCHA" = "brown"
#'     ),
#'     name = "Tipo de Superficie"
#'   ) +
#'   labs(
#'     title = "Red Vial Departamental de Cusco",
#'     subtitle = "Por Tipo de Superficie - PROVÍAS 2022",
#'     caption = "Fuente: PROVÍAS NACIONAL | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Mapa por estado de conservación
#' ggplot(vias_cusco) +
#'   geom_sf(aes(color = estado_l), linewidth = 1) +
#'   scale_color_manual(
#'     values = c(
#'       "BUENO" = "darkgreen",
#'       "REGULAR" = "orange",
#'       "MALO" = "red"
#'     ),
#'     name = "Estado"
#'   ) +
#'   labs(
#'     title = "Estado de Conservación de la Red Vial",
#'     subtitle = "Departamento de Cusco",
#'     caption = "Fuente: PROVÍAS NACIONAL | Visor - SDOT"
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
#' # Análisis por estado y superficie
#' vias_cusco |>
#'   st_drop_geometry() |>
#'   group_by(estado_l, superfic_l) |>
#'   summarise(
#'     longitud_total = sum(longitud, na.rm = TRUE),
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(longitud_total))
#'
#' # Filtrar vías pavimentadas en buen estado
#' vias_buenas <- vias_cusco |>
#'   filter(superfic_l == "PAVIMENTADO", estado_l == "BUENO")
#'
#' # Análisis por provincia
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
#' # Visualizar una provincia específica
#' vias_prov <- vias_cusco |>
#'   filter(nombprov == "CUSCO")
#'
#' ggplot(vias_prov) +
#'   geom_sf(aes(color = codruta), linewidth = 1.2) +
#'   labs(
#'     title = "Red Vial de la Provincia de Cusco",
#'     subtitle = "Códigos de Ruta",
#'     caption = "Fuente: PROVÍAS NACIONAL"
#'   ) +
#'   theme_minimal() +
#'   theme(legend.position = "right")
#'
#' # Estadísticas generales
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
#' PROVÍAS NACIONAL - MTC. Base Cartográfica de la Red Vial Nacional 2022.
#'
#' Decreto Supremo 011-2016-MTC - Clasificador de Rutas del Sistema Nacional de
#' Carreteras (SINAC).
#'
#' Repositorio DEMARCA en OSF: \url{https://osf.io/qy4j6/}
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
    stop("El par\u00e1metro 'show_progress' debe ser TRUE o FALSE", call. = FALSE)
  }

  if (!is.logical(force_update) || length(force_update) != 1) {
    stop("El par\u00e1metro 'force_update' debe ser TRUE o FALSE", call. = FALSE)
  }

  if (!is.null(provincia) && !is.character(provincia)) {
    stop("El par\u00e1metro 'provincia' debe ser un vector de caracteres o NULL",
         call. = FALSE)
  }

  # Si no se especifica departamento, mostrar lista
  if (is.null(departamento)) {
    message("Departamentos disponibles:")
    message(paste0("  - ", tabla_departamentos$departamento, collapse = "\n"))
    message("\nUso: get_red_vial_departamental(departamento = 'CUSCO')")
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
      "Use get_red_vial_departamental() para ver la lista completa.",
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
            message(
              "\u2713 Cargado: ", dep,
              " (", nrow(datos_dep), " tramos, ",
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
  # 6. FILTRADO POR PROVINCIA
  # ==========================================================================

  # Filtro por PROVINCIA
  if (!is.null(provincia)) {
    if ("nombprov" %in% names(datos_final)) {
      prov_input <- toupper(trimws(provincia))
      datos_final <- datos_final[toupper(datos_final$nombprov) %in% prov_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No se encontraron v\u00edas para la(s) provincia(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        longitud_filtrada <- sum(datos_final$longitud, na.rm = TRUE)
        message(
          "\u2713 Filtrado por provincia: ",
          nrow(datos_final), " tramos (",
          round(longitud_filtrada, 1), " km)"
        )
      }
    } else {
      warning("No se encontr\u00f3 la columna 'nombprov' para filtrar", call. = FALSE)
    }
  }

  # ==========================================================================
  # 7. MENSAJE FINAL
  # ==========================================================================

  if (show_progress) {
    longitud_total <- sum(datos_final$longitud, na.rm = TRUE)
    message(
      "\u2713 Datos cargados exitosamente: ",
      nrow(datos_final), " tramos (",
      round(longitud_total, 1), " km totales)"
    )
  }

  return(datos_final)
}
