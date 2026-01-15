#' Obtener Cobertura de Servicio Móvil por Centros Poblados del Perú
#'
#' Descarga y carga la información de cobertura de servicio móvil por centros
#' poblados del Perú, elaborado por el Ministerio de Transportes y Comunicaciones
#' (MTC) y el Organismo Supervisor de Inversión Privada en Telecomunicaciones (OSIPTEL).
#' Los datos incluyen geometría tipo POINT e información sobre disponibilidad de
#' tecnologías 2G, 3G, 4G y 5G, así como servicios de voz, SMS y datos.
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
#'   información de cobertura móvil por centro poblado, incluyendo:
#'
#'   \strong{Identificación geográfica:}
#'   \itemize{
#'     \item Geometría POINT (coordenadas del centro poblado)
#'     \item \code{n}: Número de registro
#'     \item \code{ubigeo_ccp}: Código de ubicación geográfica del centro poblado
#'     \item \code{departamen}: Nombre del departamento
#'     \item \code{provincia}: Nombre de la provincia
#'     \item \code{distrito}: Nombre del distrito
#'     \item \code{centro_pob}: Nombre del centro poblado
#'     \item \code{y_latitud}: Latitud del centro poblado
#'     \item \code{x_longitud}: Longitud del centro poblado
#'     \item \code{emoperador}: Indicador de presencia de operador
#'     \item \code{layer}: Capa de origen
#'   }
#'
#'   \strong{Tecnologías de red móvil} (valores: 1 = disponible, 0 = no disponible):
#'   \itemize{
#'     \item \code{2g}: Tecnología 2G declarada por la empresa operadora a nivel
#'       de centro poblado
#'     \item \code{3g}: Tecnología 3G declarada por la empresa operadora a nivel
#'       de centro poblado
#'     \item \code{4g}: Tecnología 4G declarada por la empresa operadora a nivel
#'       de centro poblado
#'     \item \code{5g}: Tecnología 5G declarada por la empresa operadora a nivel
#'       de centro poblado
#'   }
#'
#'   \strong{Servicios de comunicación} (valores: 1 = disponible, 0 = no disponible):
#'   \itemize{
#'     \item \code{voz}: Servicio de voz brindado por la empresa operadora a nivel
#'       de centro poblado
#'     \item \code{sms}: Servicio de mensaje de texto corto (SMS) brindado por la
#'       empresa operadora a nivel de centro poblado
#'     \item \code{mms}: Servicio de mensaje multimedia (MMS) brindado por la
#'       empresa operadora a nivel de centro poblado
#'   }
#'
#'   \strong{Velocidad de internet móvil} (valores: 1 = disponible, 0 = no disponible):
#'   \itemize{
#'     \item \code{hasta1mbps}: Servicio de acceso a internet móvil con velocidad
#'       de hasta 1 Mbps brindado por la empresa operadora
#'     \item \code{masde1mbps}: Servicio de acceso a internet móvil con velocidad
#'       de más de 1 Mbps brindado por la empresa operadora
#'   }
#'
#'   \strong{Infraestructura - Estaciones base:}
#'   \itemize{
#'     \item \code{canteb2g}: Cantidad de estaciones base que pueden brindar
#'       servicio de cobertura móvil con tecnología 2G al centro poblado
#'     \item \code{canteb3g}: Cantidad de estaciones base que pueden brindar
#'       servicio de cobertura móvil con tecnología 3G al centro poblado
#'     \item \code{canteb4g}: Cantidad de estaciones base que pueden brindar
#'       servicio de cobertura móvil con tecnología 4G al centro poblado
#'     \item \code{canteb5g}: Cantidad de estaciones base que pueden brindar
#'       servicio de cobertura móvil con tecnología 5G al centro poblado
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
#'   \item Fuente: Ministerio de Transportes y Comunicaciones (MTC) / OSIPTEL
#'   \item Nivel: Centro poblado
#'   \item Aplicación: Análisis de brecha digital y planificación de telecomunicaciones
#' }
#'
#' **Interpretación de variables binarias:**
#'
#' Las variables de tecnología (2G, 3G, 4G, 5G), servicios (VOZ, SMS, MMS) y
#' velocidad (hasta1mbps, masde1mbps) son declaradas por las empresas operadoras
#' y toman únicamente valores de 0 o 1:
#' \itemize{
#'   \item \strong{Valor 1}: El centro poblado cuenta con la tecnología/servicio
#'   \item \strong{Valor 0}: El centro poblado NO cuenta con la tecnología/servicio
#' }
#'
#' **Tecnologías de red móvil:**
#' \itemize{
#'   \item \strong{2G (GSM)}: Segunda generación - servicios básicos de voz y SMS
#'   \item \strong{3G (UMTS/HSPA)}: Tercera generación - datos móviles básicos,
#'     navegación web limitada
#'   \item \strong{4G (LTE)}: Cuarta generación - banda ancha móvil, streaming
#'     de video, aplicaciones en tiempo real
#'   \item \strong{5G}: Quinta generación - ultra banda ancha, baja latencia,
#'     IoT masivo
#' }
#'
#' **Estaciones base:**
#'
#' Las variables canteb2g, canteb3g, canteb4g y canteb5g indican la cantidad
#' de estaciones base (antenas) que pueden brindar servicio de cobertura móvil
#' con cada tecnología al centro poblado. Un mayor número de estaciones base
#' generalmente indica mejor calidad de señal y capacidad de red.
#'
#' **Filtrado jerárquico:**
#' Los filtros se aplican en cascada:
#' \enumerate{
#'   \item Primero se cargan los departamentos especificados
#'   \item Luego se filtran las provincias (si se especifican)
#'   \item Finalmente se filtran los distritos (si se especifican)
#' }
#'
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/cobertura_movil/}
#'
#' **NOTA:** Las geometrías son tipo POINT (puntos) y representan la ubicación
#' de cada centro poblado con información de cobertura.
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_cobertura_movil_c()
#'
#' # Cargar cobertura de un departamento completo
#' cob_cusco <- get_cobertura_movil_c(departamento = "CUSCO")
#'
#' # Filtrar por provincia específica
#' cob_prov_cusco <- get_cobertura_movil_c(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Filtrar por distrito específico
#' cob_san_sebastian <- get_cobertura_movil_c(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "SAN SEBASTIAN"
#' )
#'
#' # Cargar múltiples departamentos
#' cob_sur <- get_cobertura_movil_c(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Mapa de cobertura 4G
#' ggplot(cob_cusco) +
#'   geom_sf(aes(color = factor(`4g`)), size = 1, alpha = 0.6) +
#'   scale_color_manual(
#'     values = c("0" = "red", "1" = "darkgreen"),
#'     labels = c("0" = "Sin cobertura", "1" = "Con cobertura"),
#'     name = "Cobertura 4G"
#'   ) +
#'   labs(
#'     title = "Cobertura 4G en Centros Poblados de Cusco",
#'     subtitle = "MTC / OSIPTEL",
#'     caption = "Fuente: MTC | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Resumen de cobertura por tecnología
#' cob_cusco |>
#'   sf::st_drop_geometry() |>
#'   summarise(
#'     total_ccpp = n(),
#'     con_2g = sum(`2g`),
#'     con_3g = sum(`3g`),
#'     con_4g = sum(`4g`),
#'     con_5g = sum(`5g`),
#'     pct_4g = round(sum(`4g`) / n() * 100, 1)
#'   )
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link{get_distritos}}, \code{\link{get_centros_poblados}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' Ministerio de Transportes y Comunicaciones (MTC). Cobertura de Servicio Móvil
#' por Centros Poblados.
#'
#' OSIPTEL - Organismo Supervisor de Inversión Privada en Telecomunicaciones.
#'
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom utils head
get_cobertura_movil_c <- function(departamento = NULL,
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
      "cobertura_movil_oper_amazonas.gpkg",
      "cobertura_movil_oper_ancash.gpkg",
      "cobertura_movil_oper_apurimac.gpkg",
      "cobertura_movil_oper_arequipa.gpkg",
      "cobertura_movil_oper_ayacucho.gpkg",
      "cobertura_movil_oper_cajamarca.gpkg",
      "cobertura_movil_oper_callao.gpkg",
      "cobertura_movil_oper_cusco.gpkg",
      "cobertura_movil_oper_huancavelica.gpkg",
      "cobertura_movil_oper_huanuco.gpkg",
      "cobertura_movil_oper_ica.gpkg",
      "cobertura_movil_oper_junin.gpkg",
      "cobertura_movil_oper_la_libertad.gpkg",
      "cobertura_movil_oper_lambayeque.gpkg",
      "cobertura_movil_oper_lima.gpkg",
      "cobertura_movil_oper_loreto.gpkg",
      "cobertura_movil_oper_madre_de_dios.gpkg",
      "cobertura_movil_oper_moquegua.gpkg",
      "cobertura_movil_oper_pasco.gpkg",
      "cobertura_movil_oper_piura.gpkg",
      "cobertura_movil_oper_puno.gpkg",
      "cobertura_movil_oper_san_martin.gpkg",
      "cobertura_movil_oper_tacna.gpkg",
      "cobertura_movil_oper_tumbes.gpkg",
      "cobertura_movil_oper_ucayali.gpkg"
    ),
    id_osf = c(
      "hzkc6", "rekvf", "ga2wx", "dpw8j", "3mvy4",
      "35p9r", "ak9h2", "q5xpd", "m6s74", "h7xy4",
      "jyc82", "cpgwx", "p9382", "2hy3f", "scbvw",
      "us9xq", "byqc9", "gh5fs", "dxv9c", "sz3p4",
      "d2tpj", "fbp7a", "p2mvj", "2jbkp", "vkxjf"
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
    message("\nUso: get_cobertura_movil(departamento = 'CUSCO')")
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
      "Use get_cobertura_movil() para ver la lista completa.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "cobertura_movil")
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
      nrow(datos_final), " centros poblados con informaci\u00f3n de cobertura"
    )
  }

  return(datos_final)
}
