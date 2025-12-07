#' Obtener Aeródromos del Perú
#'
#' Descarga y carga la información de aeródromos del Perú actualizado al año 2022,
#' elaborado por el Ministerio de Transportes y Comunicaciones. Los datos incluyen
#' geometría tipo POINT e información sobre aeropuertos, aeródromos y helipuertos.
#' Permite filtrado escalonado por departamento, provincia y distrito.
#'
#' @param departamento Character vector. Nombre(s) del/los departamento(s) a descargar.
#'   Opciones válidas: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CUSCO", "HUANUCO", "ICA", "JUNIN", "LA LIBERTAD", "LAMBAYEQUE",
#'   "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA", "PASCO", "PIURA", "PUNO",
#'   "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
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
#'   información de los aeródromos, incluyendo:
#'   \itemize{
#'     \item Geometría POINT (coordenadas del aeródromo)
#'     \item \code{nombre}: Nombre del aeródromo
#'     \item \code{label}: Etiqueta descriptiva completa
#'     \item \code{tipo}: Tipo de instalación (AEROPUERTO INTERNACIONAL, AERODROMO, HELIPUERTO)
#'     \item \code{codidep}: Código del departamento
#'     \item \code{nombdep}: Nombre del departamento
#'     \item \code{nombprov}: Nombre de la provincia
#'     \item \code{nombdist}: Nombre del distrito
#'     \item \code{escala}: Escala del aeródromo
#'     \item \code{lat}: Latitud
#'     \item \code{lon}: Longitud
#'     \item \code{estado}: Estado operativo (OPERATIVO, etc.)
#'     \item \code{administ}: Entidad administradora
#'     \item \code{jerarquia}: Jerarquía (NACIONAL, etc.)
#'     \item \code{titular}: Tipo de titularidad (PUBLICA, PRIVADA)
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
#'   \item Fuente: Ministerio de Transportes y Comunicaciones (MTC)
#'   \item Año: 2022
#'   \item Nivel: Aeródromo/Aeropuerto/Helipuerto
#'   \item Aplicación: Planificación territorial y análisis de conectividad
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
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/aerodromos/}
#'
#' **NOTA:** Las geometrías son tipo POINT (puntos) y representan la ubicación
#' de cada aeródromo.
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_aerodromos()
#'
#' # Cargar aeródromos de un departamento completo
#' aero_cusco <- get_aerodromos(departamento = "CUSCO")
#'
#' # Filtrar por provincia específica
#' aero_prov_cusco <- get_aerodromos(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Filtrar por distrito específico
#' aero_san_sebastian <- get_aerodromos(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "SAN SEBASTIAN"
#' )
#'
#' # Cargar múltiples departamentos
#' aero_sur <- get_aerodromos(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Mapa de aeródromos por tipo
#' ggplot(aero_cusco) +
#'   geom_sf(aes(color = tipo, shape = tipo), size = 3) +
#'   scale_color_manual(
#'     values = c(
#'       "AEROPUERTO INTERNACIONAL" = "darkred",
#'       "AERODROMO" = "darkblue",
#'       "HELIPUERTO" = "darkgreen"
#'     ),
#'     name = "Tipo"
#'   ) +
#'   labs(
#'     title = "Aeródromos del Departamento de Cusco",
#'     subtitle = "Actualizado 2022 - MTC",
#'     caption = "Fuente: MTC | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Aeródromos por tipo de titularidad
#' aero_cusco |>
#'   group_by(tipo, titular) |>
#'   summarise(n = n(), .groups = "drop") |>
#'   arrange(desc(n))
#'
#' # Filtrar aeródromos operativos públicos
#' aero_publicos <- aero_cusco |>
#'   filter(titular == "PUBLICA", estado == "OPERATIVO")
#'
#' # Visualizar aeródromos por administrador
#' aero_cusco |>
#'   filter(!is.na(administ)) |>
#'   group_by(administ) |>
#'   summarise(n = n(), .groups = "drop") |>
#'   arrange(desc(n))
#'
#' # Mapa de aeródromos con etiquetas
#' ggplot(aero_cusco) +
#'   geom_sf(aes(color = tipo), size = 4) +
#'   geom_sf_text(
#'     aes(label = nombre),
#'     size = 2.5,
#'     nudge_y = 0.1,
#'     check_overlap = TRUE
#'   ) +
#'   scale_color_brewer(palette = "Set1", name = "Tipo") +
#'   labs(
#'     title = "Aeródromos de Cusco",
#'     subtitle = "MTC 2022",
#'     caption = "Fuente: MTC | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link{get_distritos}}, \code{\link[sf]{read_sf}}
#'
#' @references
#' Ministerio de Transportes y Comunicaciones (MTC). Aeródromos del Perú 2022.
#'
#' Repositorio DEMARCA en OSF: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom utils head
get_aerodromos <- function(departamento = NULL,
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
      "CAJAMARCA", "CUSCO", "HUANUCO", "ICA", "JUNIN",
      "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS",
      "MOQUEGUA", "PASCO", "PIURA", "PUNO", "SAN MARTIN",
      "TACNA", "TUMBES", "UCAYALI"
    ),
    file_name = c(
      "aerodromo_amazonas.gpkg",
      "aerodromo_ancash.gpkg",
      "aerodromo_apurimac.gpkg",
      "aerodromo_arequipa.gpkg",
      "aerodromo_ayacucho.gpkg",
      "aerodromo_cajamarca.gpkg",
      "aerodromo_cusco.gpkg",
      "aerodromo_huanuco.gpkg",
      "aerodromo_ica.gpkg",
      "aerodromo_junin.gpkg",
      "aerodromo_la_libertad.gpkg",
      "aerodromo_lambayeque.gpkg",
      "aerodromo_lima.gpkg",
      "aerodromo_loreto.gpkg",
      "aerodromo_madre_de_dios.gpkg",
      "aerodromo_moquegua.gpkg",
      "aerodromo_pasco.gpkg",
      "aerodromo_piura.gpkg",
      "aerodromo_puno.gpkg",
      "aerodromo_san_martin.gpkg",
      "aerodromo_tacna.gpkg",
      "aerodromo_tumbes.gpkg",
      "aerodromo_ucayali.gpkg"
    ),
    id_osf = c(
      "3hp9y", "krb5x", "xsvdy", "cs73x", "csp9y",
      "wrfvt", "7g8sj", "8tj6v", "7cfmh", "7up4r",
      "rcpyu", "jxug3", "wrh2d", "nsb96", "bwzvm",
      "zwg8x", "57zfg", "35yrb", "etb39", "3cehq",
      "6tms5", "udskr", "gf5xt"
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
    message("\nUso: get_aerodromos(departamento = 'CUSCO')")
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
      "Use get_aerodromos() para ver la lista completa.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "aerodromos")
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
            message("\u2713 Cargado: ", dep, " (", nrow(datos_dep), " aer\u00f3dromos)")
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
  # 6. FILTRADO POR PROVINCIA Y DISTRITO
  # ==========================================================================

  # Filtro por PROVINCIA
  if (!is.null(provincia)) {
    if ("nombprov" %in% names(datos_final)) {
      prov_input <- toupper(trimws(provincia))
      datos_final <- datos_final[toupper(datos_final$nombprov) %in% prov_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No se encontraron aer\u00f3dromos para la(s) provincia(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por provincia: ",
          nrow(datos_final),
          " aer\u00f3dromos"
        )
      }
    } else {
      warning("No se encontr\u00f3 la columna 'nombprov' para filtrar", call. = FALSE)
    }
  }

  # Filtro por DISTRITO
  if (!is.null(distrito)) {
    if ("nombdist" %in% names(datos_final)) {
      dist_input <- toupper(trimws(distrito))
      datos_final <- datos_final[toupper(datos_final$nombdist) %in% dist_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No se encontraron aer\u00f3dromos para el/los distrito(s): ",
          paste(distrito, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por distrito: ",
          nrow(datos_final),
          " aer\u00f3dromos"
        )
      }
    } else {
      warning("No se encontr\u00f3 la columna 'nombdist' para filtrar", call. = FALSE)
    }
  }

  # ==========================================================================
  # 7. MENSAJE FINAL
  # ==========================================================================

  if (show_progress) {
    message(
      "\u2713 Datos cargados exitosamente: ",
      nrow(datos_final), " aer\u00f3dromos"
    )
  }

  return(datos_final)
}
