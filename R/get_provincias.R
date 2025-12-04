#' Obtener Límites Censales Provinciales del Perú
#'
#' Descarga y carga los límites provinciales oficiales del INEI (2023) desde el
#' repositorio OSF de DEMARCA. Implementa caché de sesión para optimizar descargas
#' repetidas y mantiene la consola limpia durante la ejecución.
#'
#' @param provincia Character vector. Nombre(s) de la(s) provincia(s) a filtrar.
#'   Puede ser una única provincia (ej: "URUBAMBA") o múltiples provincias
#'   (ej: c("CAÑETE", "CHINCHA")). No distingue entre mayúsculas y minúsculas.
#'   Si es \code{NULL}, no filtra por provincia.
#' @param departamento Character vector. Nombre del departamento para filtrar
#'   todas sus provincias (ej: "CUSCO"). Si se especifica, retorna todas las
#'   provincias del departamento indicado. No distingue entre mayúsculas y minúsculas.
#' @param show_progress Logical. Si \code{TRUE} (por defecto), muestra mensajes
#'   informativos sobre el progreso de la descarga. Si \code{FALSE}, ejecuta de
#'   forma silenciosa.
#' @param force_update Logical. Si \code{TRUE}, fuerza una nueva descarga del
#'   archivo incluso si existe en caché. Por defecto \code{FALSE} para aprovechar
#'   datos en caché.
#'
#' @return Un objeto \code{sf} (simple feature) con la geometría de las provincias
#'   solicitadas, incluyendo sus atributos geográficos y administrativos.
#'
#' @details
#' La función descarga los datos directamente desde OSF (Open Science Framework)
#' y los almacena en un directorio temporal de caché durante la sesión de R.
#' Los datos corresponden a los límites censales oficiales del INEI 2023.
#'
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/}
#'
#' La función corrige automáticamente problemas de codificación de caracteres
#' (especialmente Ñ y tildes) en los datos originales para asegurar compatibilidad
#' entre diferentes sistemas operativos.
#'
#' Fuente de datos: Repositorio DEMARCA en OSF (\url{https://osf.io/t36aj/})
#'
#' @examples
#' \dontrun{
#' # Cargar todas las provincias del Perú
#' provincias_peru <- get_provincias()
#'
#' # Cargar una provincia específica
#' urubamba <- get_provincias(provincia = "Urubamba")
#'
#' # Cargar todas las provincias de un departamento
#' provincias_cusco <- get_provincias(departamento = "Cusco")
#'
#' # Cargar múltiples provincias específicas
#' sur_chico <- get_provincias(
#'   provincia = c("CAÑETE", "CHINCHA", "PISCO")
#' )
#'
#' # Combinar filtros: provincias específicas de un departamento
#' costa_lima <- get_provincias(
#'   provincia = c("CAÑETE", "YAUYOS"),
#'   departamento = "LIMA"
#' )
#'
#' # Forzar actualización de datos
#' provincias_actualizadas <- get_provincias(force_update = TRUE)
#'
#' # Ejecución silenciosa
#' pasco <- get_provincias(departamento = "PASCO", show_progress = FALSE)
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#'
#' # Mapa de todas las provincias de Cusco
#' ggplot(provincias_cusco) +
#'   geom_sf(aes(fill = nombprov), color = "white", linewidth = 0.3) +
#'   labs(
#'     title = "Provincias del Departamento de Cusco",
#'     subtitle = "Límites Censales INEI 2023",
#'     fill = "Provincia",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   scale_fill_viridis_d(option = "plasma") +
#'   theme_minimal() +
#'   theme(legend.position = "right")
#'
#' # Mapa de una provincia específica
#' ggplot(urubamba) +
#'   geom_sf(fill = "lightblue", color = "darkblue", linewidth = 1) +
#'   labs(
#'     title = "Provincia de Urubamba",
#'     subtitle = "Departamento de Cusco - INEI 2023",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Mapa de provincias de la costa sur
#' ggplot(sur_chico) +
#'   geom_sf(aes(fill = nombprov), color = "white", linewidth = 0.5) +
#'   labs(
#'     title = "Provincias de la Costa Sur",
#'     fill = "Provincia",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   scale_fill_manual(values = c("CHINCHA" = "#FF6B6B",
#'                                  "PISCO" = "#4ECDC4",
#'                                  "CAÑETE" = "#45B7D1")) +
#'   theme_minimal() +
#'   theme(legend.position = "bottom")
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link[sf]{read_sf}},
#' \code{\link[sf]{st_geometry}}
#'
#' @references
#' INEI (2023). Límites Censales Provinciales del Perú.
#' Repositorio DEMARCA: \url{https://osf.io/t36aj/}
#'
#' @export
#' @importFrom utils download.file unzip
#' @importFrom sf read_sf
#' @importFrom utils head
get_provincias <- function(provincia = NULL,
                           departamento = NULL,
                           show_progress = TRUE,
                           force_update = FALSE) {

  # ==========================================================================
  # 1. CONFIGURACI\u00d3N Y VALIDACI\u00d3N DE PAR\u00c1METROS
  # ==========================================================================

  # Validar tipos de par\u00e1metros
  if (!is.logical(show_progress) || length(show_progress) != 1) {
    stop("El par\u00e1metro 'show_progress' debe ser TRUE o FALSE",
         call. = FALSE)
  }

  if (!is.logical(force_update) || length(force_update) != 1) {
    stop("El par\u00e1metro 'force_update' debe ser TRUE o FALSE",
         call. = FALSE)
  }

  if (!is.null(provincia) && !is.character(provincia)) {
    stop("El par\u00e1metro 'provincia' debe ser un vector de caracteres o NULL",
         call. = FALSE)
  }

  if (!is.null(departamento) && !is.character(departamento)) {
    stop("El par\u00e1metro 'departamento' debe ser un vector de caracteres o NULL",
         call. = FALSE)
  }

  # Configuraci\u00f3n de URLs y rutas
  id_archivo_osf <- "t36aj"
  url_descarga   <- paste0("https://osf.io/", id_archivo_osf, "/download")

  # Configurar directorio de cach\u00e9
  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache")
  if (!dir.exists(ruta_cache_dir)) {
    dir.create(ruta_cache_dir, recursive = TRUE)
  }

  archivo_zip <- file.path(ruta_cache_dir, "v_provincias_2023.zip")

  # ==========================================================================
  # 2. GESTI\u00d3N DE DESCARGA CON CACH\u00c9
  # ==========================================================================

  if (!file.exists(archivo_zip) || force_update) {
    if (show_progress) {
      message("Descargando: L\u00edmites Censales Provinciales (INEI 2023)...")
      message("Fuente: OSF - Repositorio DEMARCA")
    }

    tryCatch(
      {
        utils::download.file(
          url      = url_descarga,
          destfile = archivo_zip,
          mode     = "wb",
          quiet    = !show_progress
        )

        if (show_progress) message("\u2713 Descarga completada")
      },
      error = function(e) {
        stop(
          "Error al descargar desde OSF. Verifique su conexi\u00f3n a internet.\n",
          "URL: ", url_descarga, "\n",
          "Detalles: ", e$message,
          call. = FALSE
        )
      }
    )
  } else {
    if (show_progress) message("\u2713 Usando datos provinciales en cach\u00e9 local")
  }

  # ==========================================================================
  # 3. GESTI\u00d3N DE DESCOMPRESI\u00d3N
  # ==========================================================================

  archivo_shp_existente <- list.files(
    path       = ruta_cache_dir,
    pattern    = ".*provincia.*\\.shp$",
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
      pattern    = ".*provincia.*\\.shp$",
      full.names = TRUE,
      recursive  = TRUE,
      ignore.case = TRUE
    )
  }

  if (length(archivo_shp_existente) == 0) {
    stop(
      "Error: No se encontr\u00f3 archivo .shp v\u00e1lido en el ZIP descargado.\n",
      "Ruta buscada: ", ruta_cache_dir,
      call. = FALSE
    )
  }

  # ==========================================================================
  # 4. LECTURA Y CORRECCI\u00d3N DE CODIFICACI\u00d3N
  # ==========================================================================

  if (show_progress) message("Cargando geometr\u00edas...")

  datos_sf <- tryCatch(
    {
      sf::read_sf(archivo_shp_existente[1], quiet = TRUE)
    },
    error = function(e) {
      stop(
        "Error al leer el shapefile.\n",
        "Archivo: ", archivo_shp_existente[1], "\n",
        "Detalles: ", e$message,
        call. = FALSE
      )
    }
  )

  # Normalizar nombres de columnas a min\u00fasculas
  colnames(datos_sf) <- tolower(colnames(datos_sf))

  # ---- NUEVA CORRECCI\u00d3N DE CODIFICACI\u00d3N ----
  # El shapefile original trae la \u00d1 como '\u00c3\u00d1' / '\u00c3\u00f1' u otros artefactos.
  # 1) Forzamos todas las columnas de texto a UTF-8 desde latin1 (donde suele venir el DBF)
  # 2) Corregimos patrones t\u00edpicos de mala representaci\u00f3n de la \u00d1.

  cols_char <- names(datos_sf)[sapply(datos_sf, is.character)]

  for (col in cols_char) {
    x <- datos_sf[[col]]

    # Paso 1: asegurar UTF-8 desde latin1 (DBF del INEI suele venir en latin1/CP1252)
    x <- iconv(x, from = "latin1", to = "UTF-8", sub = "")

    # Paso 2: corregir \u00d1 mal representada como '\u00c3\u00d1' / '\u00c3\u00f1'
    # (esto se ve muchas veces como CA\u00c3\u00d1ETE en lugar de CA\u00d1ETE)
    x <- gsub("\u00c3\u00d1", "\u00d1", x, fixed = TRUE)
    x <- gsub("\u00c3\u00f1", "\u00f1", x, fixed = TRUE)

    # Si quisieras ser a\u00fan m\u00e1s agresivo, podr\u00edas a\u00f1adir patrones extras aqu\u00ed.

    datos_sf[[col]] <- x
  }
  # ---- FIN DE CORRECCI\u00d3N DE CODIFICACI\u00d3N ----

  # ==========================================================================
  # 5. FILTRADO POR DEPARTAMENTO Y PROVINCIA
  # ==========================================================================

  # Filtro por DEPARTAMENTO
  if (!is.null(departamento)) {
    if ("nombdep" %in% names(datos_sf)) {
      dep_input <- toupper(trimws(departamento))
      datos_sf  <- datos_sf[toupper(datos_sf$nombdep) %in% dep_input, ]

      if (nrow(datos_sf) == 0) {
        warning(
          "No se encontraron provincias para el departamento: ",
          paste(departamento, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por departamento: ",
          nrow(datos_sf),
          " provincia(s) en ",
          paste(departamento, collapse = ", ")
        )
      }
    } else {
      warning(
        "No se encontr\u00f3 la columna 'nombdep' para filtrar por departamento.",
        call. = FALSE
      )
    }
  }

  # Filtro por PROVINCIA
  if (!is.null(provincia)) {
    if ("nombprov" %in% names(datos_sf)) {
      prov_input <- toupper(trimws(provincia))
      datos_sf_filtrado <- datos_sf[toupper(datos_sf$nombprov) %in% prov_input, ]

      if (nrow(datos_sf_filtrado) == 0) {
        provincias_disponibles <- unique(datos_sf$nombprov)
        warning(
          "No se encontraron coincidencias para: ",
          paste(provincia, collapse = ", "), "\n",
          "Provincias disponibles",
          if (!is.null(departamento)) paste0(" en ", departamento) else "",
          " (primeras 20):\n  ",
          paste(head(sort(provincias_disponibles), 20), collapse = "\n  "),
          call. = FALSE
        )
        return(datos_sf)
      }

      datos_sf <- datos_sf_filtrado
    } else {
      warning(
        "No se encontr\u00f3 la columna 'nombprov' para filtrar por provincia.",
        call. = FALSE
      )
    }
  }

  if (show_progress) {
    message(
      "\u2713 Datos cargados: ",
      nrow(datos_sf),
      " provincia(s)"
    )
  }

  return(datos_sf)
}
