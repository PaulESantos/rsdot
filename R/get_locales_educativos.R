#' Obtener Locales Educativos del Perú
#'
#' Descarga y carga la información de locales educativos del Perú, elaborado por
#' el Ministerio de Educación (MINEDU). Un local educativo es un inmueble en el
#' cual funciona uno o más establecimientos educativos (servicios educativos).
#' Los datos incluyen geometría tipo POINT e información sobre los servicios
#' educativos que operan en cada local, ubicación y área censal.
#' Permite filtrado escalonado por departamento, provincia y distrito.
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
#'   información de los locales educativos, incluyendo:
#'
#'   \strong{Identificación del local:}
#'   \itemize{
#'     \item Geometría POINT (coordenadas del local educativo)
#'     \item \code{codlocal}: Código único del local educativo asignado por MINEDU
#'     \item \code{servicios}: Lista de servicios educativos (instituciones educativas)
#'       que funcionan en el local, incluyendo nombre, nivel y modalidad. Un mismo
#'       local puede albergar múltiples servicios educativos (ej: inicial, primaria
#'       y secundaria)
#'     \item \code{dir_cen}: Dirección del local educativo
#'   }
#'
#'   \strong{Ubicación geográfica:}
#'   \itemize{
#'     \item \code{localidad}: Nombre de la localidad donde se ubica el local
#'     \item \code{codcp_inei}: Código de centro poblado según INEI
#'     \item \code{codccpp}: Código de centro poblado (alternativo)
#'     \item \code{centro_pob}: Nombre del centro poblado
#'     \item \code{departamen}: Nombre del departamento
#'     \item \code{provincia}: Nombre de la provincia
#'     \item \code{distrito}: Nombre del distrito
#'   }
#'
#'   \strong{Características del local:}
#'   \itemize{
#'     \item \code{area_censo}: Área censal donde se ubica el local:
#'       \itemize{
#'         \item URBANA: Zona urbana
#'         \item RURAL: Zona rural
#'       }
#'   }
#'
#'   \strong{Coordenadas:}
#'   \itemize{
#'     \item \code{nlat_ie}: Latitud del local educativo (coordenada Y)
#'     \item \code{nlong_ie}: Longitud del local educativo (coordenada X)
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
#'   \item Fuente: Ministerio de Educación (MINEDU)
#'   \item Registro: Padrón de Instituciones Educativas
#'   \item Nivel: Local educativo
#'   \item Aplicación: Análisis de cobertura educativa, planificación de
#'     infraestructura escolar y estudios de accesibilidad
#' }
#'
#' **Conceptos clave:**
#'
#' \strong{Local educativo vs Institución educativa:}
#' \itemize{
#'   \item \strong{Local educativo}: Infraestructura física (inmueble) donde
#'     funcionan los servicios educativos. Un local puede albergar múltiples
#'     instituciones educativas.
#'   \item \strong{Servicio/Institución educativa}: Unidad operativa del sistema
#'     educativo que brinda un tipo específico de educación (inicial, primaria,
#'     secundaria, CETPRO, etc.)
#' }
#'
#' **Niveles y modalidades educativas en Perú:**
#' \itemize{
#'   \item \strong{Educación Básica Regular (EBR)}:
#'     \itemize{
#'       \item Inicial (0-5 años): Cuna, Jardín
#'       \item Primaria (6-11 años)
#'       \item Secundaria (12-16 años)
#'     }
#'   \item \strong{Educación Básica Alternativa (EBA)}: Para personas que no
#'     accedieron oportunamente a EBR
#'   \item \strong{Educación Básica Especial (EBE)}: Para personas con
#'     discapacidad o talento superdotado
#'   \item \strong{Educación Técnico-Productiva (CETPRO)}: Formación técnica
#'     y capacitación laboral
#'   \item \strong{Educación Superior}: Institutos y escuelas de educación
#'     superior pedagógica, tecnológica y artística
#' }
#'
#' **Interpretación del campo servicios:**
#'
#' El campo \code{servicios} contiene una cadena de texto con todos los servicios
#' educativos que operan en el local, separados por comas. Cada servicio incluye
#' el nombre de la institución y su nivel/modalidad entre paréntesis.
#'
#' Ejemplo: "501330 (PRIMARIA), 501330 (SECUNDARIA)" indica que en ese local
#' funcionan los niveles de primaria y secundaria de la IE 501330.
#'
#' **Filtrado jerárquico:**
#' Los filtros se aplican en cascada:
#' \enumerate{
#'   \item Primero se cargan los departamentos especificados
#'   \item Luego se filtran las provincias (si se especifican)
#'   \item Finalmente se filtran los distritos (si se especifican)
#' }
#'
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/locales_educativos/}
#'
#' **NOTA:** Las geometrías son tipo POINT (puntos) y representan la ubicación
#' de cada local educativo.
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_locales_educativos()
#'
#' # Cargar locales educativos de un departamento completo
#' locales_cusco <- get_locales_educativos(departamento = "CUSCO")
#'
#' # Filtrar por provincia específica
#' locales_prov_cusco <- get_locales_educativos(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Filtrar por distrito específico
#' locales_wanchaq <- get_locales_educativos(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "WANCHAQ"
#' )
#'
#' # Cargar múltiples departamentos
#' locales_sur <- get_locales_educativos(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Mapa de locales educativos por área censal
#' ggplot(locales_cusco) +
#'   geom_sf(aes(color = area_censo), size = 1, alpha = 0.6) +
#'   scale_color_manual(
#'     values = c("URBANA" = "darkblue", "RURAL" = "darkgreen"),
#'     name = "Área"
#'   ) +
#'   labs(
#'     title = "Locales Educativos del Departamento de Cusco",
#'     subtitle = "Ministerio de Educación",
#'     caption = "Fuente: MINEDU - Padrón de Instituciones Educativas"
#'   ) +
#'   theme_minimal()
#'
#' # Resumen por área censal
#' locales_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(area_censo)
#'
#' # Distribución por distrito
#' locales_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(distrito, sort = TRUE) |>
#'   head(10)
#'
#' # Filtrar locales con nivel inicial
#' locales_inicial <- locales_cusco |>
#'   filter(grepl("INICIAL", servicios, ignore.case = TRUE))
#'
#' # Filtrar locales con secundaria
#' locales_secundaria <- locales_cusco |>
#'   filter(grepl("SECUNDARIA", servicios, ignore.case = TRUE))
#'
#' # Locales con múltiples niveles (inicial + primaria + secundaria)
#' locales_completos <- locales_cusco |>
#'   filter(
#'     grepl("INICIAL", servicios) &
#'     grepl("PRIMARIA", servicios) &
#'     grepl("SECUNDARIA", servicios)
#'   )
#'
#' # Análisis de cobertura por provincia
#' cobertura_provincia <- locales_cusco |>
#'   sf::st_drop_geometry() |>
#'   group_by(provincia) |>
#'   summarise(
#'     total_locales = n(),
#'     locales_urbanos = sum(area_censo == "URBANA"),
#'     locales_rurales = sum(area_censo == "RURAL"),
#'     pct_rural = round(sum(area_censo == "RURAL") / n() * 100, 1),
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(total_locales))
#'
#' # Mapa de locales rurales
#' locales_cusco |>
#'   filter(area_censo == "RURAL") |>
#'   ggplot() +
#'   geom_sf(color = "darkgreen", size = 0.8, alpha = 0.5) +
#'   labs(
#'     title = "Locales Educativos Rurales - Cusco",
#'     subtitle = "MINEDU",
#'     caption = "Fuente: MINEDU - Padrón de Instituciones Educativas"
#'   ) +
#'   theme_minimal()
#'
#' # Contar servicios educativos por local
#' locales_cusco |>
#'   sf::st_drop_geometry() |>
#'   mutate(
#'     n_servicios = stringr::str_count(servicios, "\\(")
#'   ) |>
#'   count(n_servicios, name = "n_locales")
#'
#' # Buscar locales por nombre de institución
#' locales_emblematicos <- locales_cusco |>
#'   filter(grepl("INCA GARCILASO|CLORINDA MATTO", servicios, ignore.case = TRUE))
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link{get_distritos}}, \code{\link{get_centros_poblados}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' Ministerio de Educación del Perú (MINEDU). Padrón de Instituciones Educativas
#' y Programas Educativos.
#'
#' ESCALE - Estadística de la Calidad Educativa: \url{https://escale.minedu.gob.pe/}
#'
#' Repositorio DEMARCA en OSF: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom utils head
get_locales_educativos <- function(departamento = NULL,
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
      "locales_educativos_amazonas.gpkg",
      "locales_educativos_ancash.gpkg",
      "locales_educativos_apurimac.gpkg",
      "locales_educativos_arequipa.gpkg",
      "locales_educativos_ayacucho.gpkg",
      "locales_educativos_cajamarca.gpkg",
      "locales_educativos_callao.gpkg",
      "locales_educativos_cusco.gpkg",
      "locales_educativos_huancavelica.gpkg",
      "locales_educativos_huanuco.gpkg",
      "locales_educativos_ica.gpkg",
      "locales_educativos_junin.gpkg",
      "locales_educativos_la_libertad.gpkg",
      "locales_educativos_lambayeque.gpkg",
      "locales_educativos_lima.gpkg",
      "locales_educativos_loreto.gpkg",
      "locales_educativos_madre_de_dios.gpkg",
      "locales_educativos_moquegua.gpkg",
      "locales_educativos_pasco.gpkg",
      "locales_educativos_piura.gpkg",
      "locales_educativos_puno.gpkg",
      "locales_educativos_san_martin.gpkg",
      "locales_educativos_tacna.gpkg",
      "locales_educativos_tumbes.gpkg",
      "locales_educativos_ucayali.gpkg"
    ),
    id_osf = c(
      "k9rsb", "5zams", "2p96k", "syq4r", "cqnb8",
      "rxmyz", "suh8t", "zx47g", "pv4ms", "afkgz",
      "6fhsn", "2ejaz", "xvpcw", "cresx", "cpz9t",
      "d5b8e", "n2cmg", "phkda", "paqdv", "2mh5y",
      "5zg68", "7625d", "zurp3", "exuty", "wr3cj"
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
    message("\nUso: get_locales_educativos(departamento = 'CUSCO')")
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
      "Use get_locales_educativos() para ver la lista completa.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "locales_educativos")
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

          # Normalizar nombres de columnas a minúsculas
          colnames(datos_dep) <- tolower(colnames(datos_dep))

          lista_datos[[dep]] <- datos_dep

          if (show_progress) {
            message("\u2713 Cargado: ", dep, " (", nrow(datos_dep), " locales educativos)")
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
    if ("provincia" %in% names(datos_final)) {
      prov_input <- toupper(trimws(provincia))
      datos_final <- datos_final[toupper(datos_final$provincia) %in% prov_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No se encontraron locales educativos para la(s) provincia(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por provincia: ",
          nrow(datos_final),
          " locales educativos"
        )
      }
    } else {
      warning("No se encontr\u00f3 la columna 'provincia' para filtrar", call. = FALSE)
    }
  }

  # Filtro por DISTRITO
  if (!is.null(distrito)) {
    if ("distrito" %in% names(datos_final)) {
      dist_input <- toupper(trimws(distrito))
      datos_final <- datos_final[toupper(datos_final$distrito) %in% dist_input, ]

      if (nrow(datos_final) == 0) {
        warning(
          "No se encontraron locales educativos para el/los distrito(s): ",
          paste(distrito, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por distrito: ",
          nrow(datos_final),
          " locales educativos"
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
      nrow(datos_final), " locales educativos"
    )
  }

  return(datos_final)
}
