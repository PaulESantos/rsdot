#' Obtener Límites Censales Departamentales del Perú
#'
#' Descarga y carga los límites departamentales oficiales del INEI (2023) desde el
#' repositorio OSF de DEMARCA. Implementa caché de sesión para optimizar descargas
#' repetidas y mantiene la consola limpia durante la ejecución.
#'
#' @param departamento Character vector. Nombre(s) del/los departamento(s) a filtrar.
#'   Puede ser un único departamento (ej: "CUSCO") o múltiples departamentos
#'   (ej: c("CUSCO", "PUNO")). No distingue entre mayúsculas y minúsculas.
#'   Si es \code{NULL} (por defecto), retorna todos los departamentos del Perú.
#' @param show_progress Logical. Si \code{TRUE} (por defecto), muestra mensajes
#'   informativos sobre el progreso de la descarga. Si \code{FALSE}, ejecuta de
#'   forma silenciosa.
#' @param force_update Logical. Si \code{TRUE}, fuerza una nueva descarga del
#'   archivo incluso si existe en caché. Por defecto \code{FALSE} para aprovechar
#'   datos en caché.
#'
#' @return Un objeto \code{sf} (simple feature) con la geometría de los departamentos
#'   solicitados, incluyendo sus atributos geográficos y administrativos.
#'
#' @details
#' La función descarga los datos directamente desde OSF (Open Science Framework)
#' y los almacena en un directorio temporal de caché durante la sesión de R.
#' Los datos corresponden a los límites censales oficiales del INEI 2023.
#'
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/}
#'
#' Fuente de datos: Repositorio DEMARCA en OSF (\url{https://osf.io/grhe3/})
#'
#' @examples
#' \dontrun{
#' # Cargar todos los departamentos del Perú
#' peru <- get_departamentos()
#'
#' # Cargar un departamento específico (no distingue mayúsculas/minúsculas)
#' cusco <- get_departamentos(departamento = "Cusco")
#'
#' # Cargar múltiples departamentos (región sur)
#' sur <- get_departamentos(
#'   departamento = c("PUNO", "TACNA", "MOQUEGUA", "AREQUIPA")
#' )
#'
#' # Forzar actualización de datos
#' peru_actualizado <- get_departamentos(force_update = TRUE)
#'
#' # Ejecución silenciosa
#' lima <- get_departamentos(departamento = "LIMA", show_progress = FALSE)
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#'
#' ggplot(peru) +
#'   geom_sf(fill = "#ff9999", color = "#e60000", linewidth = 1) +
#'   labs(
#'     title = "Límites Censales Departamentales del Perú",
#'     subtitle = "Límites Censales INEI 2023",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' ggplot(cusco) +
#'   geom_sf(fill = "lightgreen", color = "darkgreen", linewidth = 1) +
#'   labs(
#'     title = "Departamento de Cusco",
#'     subtitle = "Límites Censales INEI 2023",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' ggplot(sur) +
#'   geom_sf(aes(fill = nombdep), color = "white", linewidth = 0.5) +
#'   labs(
#'     title = "Macro Región Sur del Perú",
#'     fill = "Departamento",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   scale_fill_brewer(palette = "Set3") +
#'   theme_minimal() +
#'   theme(legend.position = "bottom")
#' }
#'
#' @seealso
#' \code{\link[sf]{read_sf}}, \code{\link[sf]{st_geometry}}
#'
#' @references
#' INEI (2023). Límites Censales Departamentales del Perú.
#' Repositorio VISOR SDOT DEMARCA PERU: \url{https://osf.io/qy4j6/overview}
#'
#' @export
#' @importFrom utils download.file unzip
#' @importFrom sf read_sf
#' @importFrom utils head
get_departamentos <- function(departamento = NULL,
                              show_progress = TRUE,
                              force_update = FALSE) {

  # ==========================================================================
  # 1. CONFIGURACI\u00d3N Y VALIDACI\u00d3N DE PAR\u00c1METROS
  # ==========================================================================

  # Validar tipos de par\u00e1metros
  if (!is.logical(show_progress) || length(show_progress) != 1) {
    stop("El par\u00e1metro 'show_progress' debe ser TRUE o FALSE", call. = FALSE)
  }

  if (!is.logical(force_update) || length(force_update) != 1) {
    stop("El par\u00e1metro 'force_update' debe ser TRUE o FALSE", call. = FALSE)
  }

  if (!is.null(departamento) && !is.character(departamento)) {
    stop("El par\u00e1metro 'departamento' debe ser un vector de caracteres o NULL",
         call. = FALSE)
  }

  # Configuraci\u00f3n de URLs y rutas
  id_archivo_osf <- "grhe3"
  url_descarga <- paste0("https://osf.io/", id_archivo_osf, "/download")

  # Configurar directorio de cach\u00e9
  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache")
  if (!dir.exists(ruta_cache_dir)) {
    dir.create(ruta_cache_dir, recursive = TRUE)
  }

  archivo_zip <- file.path(ruta_cache_dir, "v_departamentos_2023.zip")

  # ==========================================================================
  # 2. GESTI\u00d3N DE DESCARGA CON CACH\u00c9
  # ==========================================================================

  # Descargar solo si no existe o si se fuerza actualizaci\u00f3n
  if (!file.exists(archivo_zip) || force_update) {
    if (show_progress) {
      message("Descargando: L\u00edmites Censales Departamentales (INEI 2023)...")
      message("Fuente: OSF - Repositorio DEMARCA")
    }

    tryCatch(
      {
        utils::download.file(
          url = url_descarga,
          destfile = archivo_zip,
          mode = "wb",
          quiet = !show_progress
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
    if (show_progress) message("\u2713 Usando datos en cach\u00e9 local")
  }

  # ==========================================================================
  # 3. GESTI\u00d3N DE DESCOMPRESI\u00d3N
  # ==========================================================================

  # Buscar shapefile ya descomprimido
  archivo_shp_existente <- list.files(
    path = ruta_cache_dir,
    pattern = ".*departamento.*\\.shp$",
    full.names = TRUE,
    recursive = TRUE,
    ignore.case = TRUE
  )

  # Descomprimir solo si es necesario
  if (length(archivo_shp_existente) == 0 || force_update) {
    if (show_progress) message("Descomprimiendo archivos...")

    suppressWarnings({
      utils::unzip(
        zipfile = archivo_zip,
        exdir = ruta_cache_dir,
        overwrite = force_update
      )
    })

    # Buscar nuevamente despu\u00e9s de descomprimir
    archivo_shp_existente <- list.files(
      path = ruta_cache_dir,
      pattern = ".*departamento.*\\.shp$",
      full.names = TRUE,
      recursive = TRUE,
      ignore.case = TRUE
    )
  }

  # Validar que existe el shapefile
  if (length(archivo_shp_existente) == 0) {
    stop(
      "Error: No se encontr\u00f3 archivo .shp v\u00e1lido en el ZIP descargado.\n",
      "Ruta buscada: ", ruta_cache_dir,
      call. = FALSE
    )
  }

  # ==========================================================================
  # 4. LECTURA DE DATOS ESPACIALES
  # ==========================================================================

  if (show_progress) message("Cargando geometr\u00edas...")

  # Leer shapefile de forma silenciosa
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

  # ==========================================================================
  # 5. FILTRADO POR DEPARTAMENTO
  # ==========================================================================

  # Si no se especifica departamento, retornar todos
  if (is.null(departamento)) {
    if (show_progress) {
      message("\u2713 Datos cargados: ", nrow(datos_sf), " departamentos")
    }
    return(datos_sf)
  }

  # Normalizar nombres de columnas a min\u00fasculas
  colnames(datos_sf) <- tolower(colnames(datos_sf))

  # Verificar que existe la columna de nombres
  if (!"nombdep" %in% names(datos_sf)) {
    warning(
      "No se encontr\u00f3 la columna 'nombdep'. Retornando datos sin filtrar.",
      call. = FALSE
    )
    return(datos_sf)
  }

  # Normalizar nombres de departamentos (a may\u00fasculas para comparaci\u00f3n)
  dep_input <- toupper(trimws(departamento))

  # Filtrar departamentos
  datos_sf_filtrado <- datos_sf[toupper(datos_sf$nombdep) %in% dep_input, ]

  # Validar resultados del filtrado
  if (nrow(datos_sf_filtrado) == 0) {
    departamentos_disponibles <- unique(datos_sf$nombdep)
    warning(
      "No se encontraron coincidencias para: ",
      paste(departamento, collapse = ", "), "\n",
      "Departamentos disponibles:\n  ",
      paste(sort(departamentos_disponibles), collapse = "\n  "),
      call. = FALSE
    )
    return(datos_sf) # Retornar datos completos como fallback
  }

  if (show_progress) {
    message(
      "\u2713 Filtrado completado: ",
      nrow(datos_sf_filtrado),
      " departamento(s)"
    )
  }

  return(datos_sf_filtrado)
}
