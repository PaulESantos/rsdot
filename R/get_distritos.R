#' Obtener Límites Censales Distritales del Perú
#'
#' Descarga y carga los límites distritales oficiales del INEI (2023) desde el
#' repositorio OSF de DEMARCA. Implementa caché de sesión para optimizar descargas
#' repetidas y mantiene la consola limpia durante la ejecución.
#'
#' @param distrito Character vector. Nombre(s) del/los distrito(s) a filtrar.
#'   Puede ser un único distrito (ej: "WANCHAQ") o múltiples distritos
#'   (ej: c("CUSCO", "WANCHAQ", "SANTIAGO")). No distingue entre mayúsculas y
#'   minúsculas. Si es \code{NULL}, no filtra por distrito.
#' @param provincia Character vector. Nombre de la provincia para filtrar
#'   todos sus distritos (ej: "CUSCO"). Si se especifica, retorna todos los
#'   distritos de la provincia indicada. No distingue entre mayúsculas y minúsculas.
#' @param departamento Character vector. Nombre del departamento para filtrar
#'   todos sus distritos (ej: "CUSCO"). Si se especifica, retorna todos los
#'   distritos del departamento indicado. No distingue entre mayúsculas y minúsculas.
#' @param show_progress Logical. Si \code{TRUE} (por defecto), muestra mensajes
#'   informativos sobre el progreso de la descarga. Si \code{FALSE}, ejecuta de
#'   forma silenciosa.
#' @param force_update Logical. Si \code{TRUE}, fuerza una nueva descarga del
#'   archivo incluso si existe en caché. Por defecto \code{FALSE} para aprovechar
#'   datos en caché.
#'
#' @return Un objeto \code{sf} (simple feature) con la geometría de los distritos
#'   solicitados, incluyendo sus atributos geográficos y administrativos.
#'
#' @details
#' La función descarga los datos directamente desde OSF (Open Science Framework)
#' y los almacena en un directorio temporal de caché durante la sesión de R.
#' Los datos corresponden a los límites censales oficiales del INEI 2023.
#'
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/}
#'
#' La función maneja automáticamente la codificación de caracteres especiales
#' (tildes, ñ) para asegurar compatibilidad entre diferentes sistemas operativos.
#'
#' Los filtros se pueden combinar jerárquicamente:
#' \itemize{
#'   \item Solo departamento: retorna todos los distritos del departamento
#'   \item Departamento + provincia: retorna todos los distritos de esa provincia
#'   \item Departamento + provincia + distrito: retorna distritos específicos
#'   \item Solo distrito: retorna ese distrito de cualquier parte del Perú
#' }
#'
#' Fuente de datos: Repositorio DEMARCA en OSF (\url{https://osf.io/j9ax8/})
#'
#' @examples
#' \dontrun{
#' # Cargar todos los distritos del Perú
#' distritos_peru <- get_distritos()
#'
#' # Cargar un distrito específico
#' wanchaq <- get_distritos(distrito = "Wanchaq")
#'
#' # Cargar todos los distritos de una provincia
#' distritos_cusco <- get_distritos(provincia = "Cusco")
#'
#' # Cargar todos los distritos de un departamento
#' distritos_puno <- get_distritos(departamento = "Puno")
#'
#' # Cargar múltiples distritos específicos
#' distritos_centrales <- get_distritos(
#'   distrito = c("CUSCO", "WANCHAQ", "SANTIAGO")
#' )
#'
#' # Combinar filtros: distritos de una provincia específica
#' costa_lima <- get_distritos(
#'   provincia = "CAÑETE",
#'   departamento = "LIMA"
#' )
#'
#' # Filtro jerárquico completo
#' distrito_especifico <- get_distritos(
#'   distrito = "SAN SEBASTIÁN",
#'   provincia = "CUSCO",
#'   departamento = "CUSCO"
#' )
#'
#' # Forzar actualización de datos
#' distritos_actualizados <- get_distritos(force_update = TRUE)
#'
#' # Ejecución silenciosa
#' callao <- get_distritos(provincia = "CALLAO", show_progress = FALSE)
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#'
#' # Mapa de todos los distritos de la provincia de Cusco
#' ggplot(distritos_cusco) +
#'   geom_sf(aes(fill = nombdist), color = "white", linewidth = 0.2) +
#'   labs(
#'     title = "Distritos de la Provincia de Cusco",
#'     subtitle = "Límites Censales INEI 2023",
#'     fill = "Distrito",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   scale_fill_brewer(palette = "Paired") +
#'   theme_minimal() +
#'   theme(
#'     legend.position = "right",
#'     legend.key.size = unit(0.4, "cm")
#'   )
#'
#' # Mapa de un distrito específico
#' ggplot(wanchaq) +
#'   geom_sf(fill = "#FFA07A", color = "darkred", linewidth = 1.5) +
#'   labs(
#'     title = "Distrito de Wanchaq",
#'     subtitle = "Provincia y Departamento de Cusco",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Mapa de distritos de Cañete
#' ggplot(costa_lima) +
#'   geom_sf(aes(fill = nombdist), color = "white", linewidth = 0.3) +
#'   labs(
#'     title = "Distritos de la Provincia de Cañete",
#'     subtitle = "Departamento de Lima",
#'     fill = "Distrito",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal() +
#'   theme(legend.position = "bottom")
#'
#' # Mapa de distritos centrales con etiquetas
#' ggplot(distritos_centrales) +
#'   geom_sf(aes(fill = nombdist), color = "white", linewidth = 0.5) +
#'   geom_sf_text(aes(label = nombdist), size = 3, fontface = "bold") +
#'   labs(
#'     title = "Distritos Centrales de Cusco",
#'     fill = "Distrito",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   scale_fill_manual(
#'     values = c("CUSCO" = "#FF6B6B", "WANCHAQ" = "#4ECDC4",
#'                "SANTIAGO" = "#45B7D1")
#'   ) +
#'   theme_minimal() +
#'   theme(legend.position = "bottom")
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link[sf]{read_sf}}, \code{\link[sf]{st_geometry}}
#'
#' @references
#' INEI (2023). Límites Censales Distritales del Perú.
#' Repositorio DEMARCA: \url{https://osf.io/j9ax8/}
#'
#' @export
#' @importFrom utils download.file unzip
#' @importFrom sf read_sf
get_distritos <- function(distrito = NULL,
                          provincia = NULL,
                          departamento = NULL,
                          show_progress = TRUE,
                          force_update = FALSE) {

  # ==========================================================================
  # 1. CONFIGURACIÓN Y VALIDACIÓN DE PARÁMETROS
  # ==========================================================================

  # Validar tipos de parámetros
  if (!is.logical(show_progress) || length(show_progress) != 1) {
    stop("El parámetro 'show_progress' debe ser TRUE o FALSE",
         call. = FALSE)
  }

  if (!is.logical(force_update) || length(force_update) != 1) {
    stop("El parámetro 'force_update' debe ser TRUE o FALSE",
         call. = FALSE)
  }

  if (!is.null(distrito) && !is.character(distrito)) {
    stop("El parámetro 'distrito' debe ser un vector de caracteres o NULL",
         call. = FALSE)
  }

  if (!is.null(provincia) && !is.character(provincia)) {
    stop("El parámetro 'provincia' debe ser un vector de caracteres o NULL",
         call. = FALSE)
  }

  if (!is.null(departamento) && !is.character(departamento)) {
    stop("El parámetro 'departamento' debe ser un vector de caracteres o NULL",
         call. = FALSE)
  }

  # Configuración de URLs y rutas
  id_archivo_osf <- "j9ax8"
  url_descarga   <- paste0("https://osf.io/", id_archivo_osf, "/download")

  # Configurar directorio de caché
  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache")
  if (!dir.exists(ruta_cache_dir)) {
    dir.create(ruta_cache_dir, recursive = TRUE)
  }

  archivo_zip <- file.path(ruta_cache_dir, "v_distritos_2023.zip")

  # ==========================================================================
  # 2. GESTIÓN DE DESCARGA CON CACHÉ
  # ==========================================================================

  if (!file.exists(archivo_zip) || force_update) {
    if (show_progress) {
      message("Descargando: Límites Censales Distritales (INEI 2023)...")
      message("Fuente: OSF - Repositorio DEMARCA")
      message("Nota: Este es un archivo grande, puede tardar varios minutos...")
    }

    tryCatch(
      {
        utils::download.file(
          url      = url_descarga,
          destfile = archivo_zip,
          mode     = "wb",
          quiet    = !show_progress
        )

        if (show_progress) message("✓ Descarga completada")
      },
      error = function(e) {
        stop(
          "Error al descargar desde OSF. Verifique su conexión a internet.\n",
          "URL: ", url_descarga, "\n",
          "Detalles: ", e$message,
          call. = FALSE
        )
      }
    )
  } else {
    if (show_progress) message("✓ Usando datos distritales en caché local")
  }

  # ==========================================================================
  # 3. GESTIÓN DE DESCOMPRESIÓN
  # ==========================================================================

  archivo_shp_existente <- list.files(
    path       = ruta_cache_dir,
    pattern    = ".*distrito.*\\.shp$",
    full.names = TRUE,
    recursive  = TRUE,
    ignore.case = TRUE
  )

  if (length(archivo_shp_existente) == 0 || force_update) {
    if (show_progress) message("Descomprimiendo archivos...")

    suppressWarnings({
      utils::unzip(
        zipfile   = archivo_zip,
        exdir     = ruta_cache_dir,
        overwrite = force_update
      )
    })

    archivo_shp_existente <- list.files(
      path       = ruta_cache_dir,
      pattern    = ".*distrito.*\\.shp$",
      full.names = TRUE,
      recursive  = TRUE,
      ignore.case = TRUE
    )
  }

  if (length(archivo_shp_existente) == 0) {
    stop(
      "Error: No se encontró archivo .shp válido en el ZIP descargado.\n",
      "Ruta buscada: ", ruta_cache_dir,
      call. = FALSE
    )
  }

  # ==========================================================================
  # 4. LECTURA Y CORRECCIÓN DE CODIFICACIÓN
  # ==========================================================================

  if (show_progress) message("Cargando geometrías distritales...")

  # Lectura estándar
  datos_sf <- tryCatch(
    {
      sf::read_sf(archivo_shp_existente[1], quiet = TRUE)
    },
    error = function(e) {
      stop(
        "Error al leer el shapefile de distritos.\n",
        "Archivo: ", archivo_shp_existente[1], "\n",
        "Detalles: ", e$message,
        call. = FALSE
      )
    }
  )

  # Normalizar nombres de columnas a minúsculas
  colnames(datos_sf) <- tolower(colnames(datos_sf))

  # ---- CORRECCIÓN DE CODIFICACIÓN DE TEXTO (MISMA ESTRATEGIA QUE PROVINCIAS) ----
  # Nota: el DBF original suele venir en latin1 y la Ñ aparece como 'ÃÑ' / 'Ãñ'

  cols_char <- names(datos_sf)[sapply(datos_sf, is.character)]

  for (col in cols_char) {
    x <- datos_sf[[col]]

    # Paso 1: forzar a UTF-8 asumiendo origen latin1
    x <- iconv(x, from = "latin1", to = "UTF-8", sub = "")

    # Paso 2: corregir Ñ mal representada
    x <- gsub("ÃÑ", "Ñ", x, fixed = TRUE)
    x <- gsub("Ãñ", "ñ", x, fixed = TRUE)

    datos_sf[[col]] <- x
  }
  # ---- FIN CORRECCIÓN CODIFICACIÓN ----

  # Guardar copia para mensajes de ayuda en caso de filtros vacíos
  datos_originales <- datos_sf

  # ==========================================================================
  # 5. FILTRADO JERÁRQUICO: DEPARTAMENTO > PROVINCIA > DISTRITO
  # ==========================================================================

  # Filtro por DEPARTAMENTO
  if (!is.null(departamento)) {
    if ("nombdep" %in% names(datos_sf)) {
      dep_input <- toupper(trimws(departamento))
      datos_sf  <- datos_sf[toupper(datos_sf$nombdep) %in% dep_input, ]

      if (nrow(datos_sf) == 0) {
        departamentos_disponibles <- sort(unique(datos_originales$nombdep))
        warning(
          "No se encontraron distritos para el departamento: ",
          paste(departamento, collapse = ", "), "\n",
          "Departamentos disponibles:\n  ",
          paste(departamentos_disponibles, collapse = "\n  "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "✓ Filtrado por departamento: ",
          nrow(datos_sf),
          " distrito(s) en ",
          paste(departamento, collapse = ", ")
        )
      }
    } else {
      warning(
        "No se encontró la columna 'nombdep' para filtrar por departamento.",
        call. = FALSE
      )
    }
  }

  # Filtro por PROVINCIA
  if (!is.null(provincia)) {
    if ("nombprov" %in% names(datos_sf)) {
      prov_input      <- toupper(trimws(provincia))
      datos_sf_temp   <- datos_sf[toupper(datos_sf$nombprov) %in% prov_input, ]

      if (nrow(datos_sf_temp) == 0) {
        provincias_disponibles <- sort(unique(datos_sf$nombprov))
        warning(
          "No se encontraron distritos para la provincia: ",
          paste(provincia, collapse = ", "), "\n",
          "Provincias disponibles",
          if (!is.null(departamento)) paste0(" en ", departamento) else "",
          ":\n  ",
          paste(provincias_disponibles, collapse = "\n  "),
          call. = FALSE
        )
        return(datos_sf)
      }

      datos_sf <- datos_sf_temp

      if (show_progress) {
        message(
          "✓ Filtrado por provincia: ",
          nrow(datos_sf),
          " distrito(s) en ",
          paste(provincia, collapse = ", ")
        )
      }
    } else {
      warning(
        "No se encontró la columna 'nombprov' para filtrar por provincia.",
        call. = FALSE
      )
    }
  }

  # Filtro por DISTRITO
  if (!is.null(distrito)) {
    if ("nombdist" %in% names(datos_sf)) {
      dist_input        <- toupper(trimws(distrito))
      datos_sf_filtrado <- datos_sf[toupper(datos_sf$nombdist) %in% dist_input, ]

      if (nrow(datos_sf_filtrado) == 0) {
        distritos_disponibles <- sort(unique(datos_sf$nombdist))
        warning(
          "No se encontraron coincidencias para: ",
          paste(distrito, collapse = ", "), "\n",
          "Distritos disponibles",
          if (!is.null(provincia)) paste0(" en ", provincia) else "",
          if (!is.null(departamento) && is.null(provincia)) {
            paste0(" en ", departamento)
          } else "",
          " (mostrando primeros 20):\n  ",
          paste(head(distritos_disponibles, 20), collapse = "\n  "),
          if (length(distritos_disponibles) > 20) {
            paste0("\n  ... y ", length(distritos_disponibles) - 20, " más")
          } else "",
          call. = FALSE
        )
        return(datos_sf)
      }

      datos_sf <- datos_sf_filtrado
    } else {
      warning(
        "No se encontró la columna 'nombdist' para filtrar por distrito.",
        call. = FALSE
      )
    }
  }

  # Mensaje de éxito final
  if (show_progress) {
    filtros_aplicados <- c()
    if (!is.null(departamento)) filtros_aplicados <- c(filtros_aplicados, "departamento")
    if (!is.null(provincia))    filtros_aplicados <- c(filtros_aplicados, "provincia")
    if (!is.null(distrito))     filtros_aplicados <- c(filtros_aplicados, "distrito")

    if (length(filtros_aplicados) > 0) {
      message(
        "✓ Filtrado completado por ",
        paste(filtros_aplicados, collapse = " > "),
        ": ",
        nrow(datos_sf),
        " distrito(s)"
      )
    } else {
      message("✓ Datos cargados: ", nrow(datos_sf), " distritos")
    }
  }

  return(datos_sf)
}
