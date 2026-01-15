#' Obtener Instituciones Prestadoras de Servicios de Salud (IPRESS) del Perú
#'
#' Descarga y carga la información de Instituciones Prestadoras de Servicios de
#' Salud (IPRESS) del Perú, elaborado por la Superintendencia Nacional de Salud
#' (SUSALUD). Los datos incluyen geometría tipo POINT e información sobre
#' establecimientos de salud, servicios médicos de apoyo, categorización,
#' horarios de atención y redes de salud. Actualizado al año 2024.
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
#'   información de las IPRESS, incluyendo:
#'
#'   \strong{Identificación del establecimiento:}
#'   \itemize{
#'     \item Geometría POINT (coordenadas del establecimiento)
#'     \item \code{cod_unic_e}: Código único del establecimiento (RENIPRESS)
#'     \item \code{nom_esta}: Nombre del establecimiento de salud
#'     \item \code{dir_esta}: Dirección completa del establecimiento
#'     \item \code{num_tele}: Número de teléfono de contacto
#'   }
#'
#'   \strong{Tipo y clasificación:}
#'   \itemize{
#'     \item \code{cod_tipo_e}: Código del tipo de establecimiento
#'     \item \code{des_tipo_e}: Descripción del tipo de establecimiento:
#'       \itemize{
#'         \item 1 = Establecimiento de salud sin internamiento
#'         \item 2 = Establecimiento de salud con internamiento
#'         \item 3 = Servicio médico de apoyo
#'         \item 4 = Oferta flexible
#'         \item 5 = Comunidades terapeuticas
#'       }
#'     \item \code{cod_clas_e}: Código de clasificación del establecimiento
#'     \item \code{des_clas_e}: Descripción de la clasificación (ej: Puestos de
#'       salud, Centros de salud, Hospitales, Clínicas, Consultorios, etc.)
#'   }
#'
#'   \strong{Institución administradora:}
#'   \itemize{
#'     \item \code{cod_inst_d}: Código de la institución administradora
#'     \item \code{des_inst_d}: Descripción de la institución administradora
#'   }
#'
#'   \strong{Ubicación geográfica:}
#'   \itemize{
#'     \item \code{cod_ubig_e}: Código de ubigeo del establecimiento
#'     \item \code{nombdep}: Nombre del departamento
#'     \item \code{nombprov}: Nombre de la provincia
#'     \item \code{nombdist}: Nombre del distrito
#'     \item \code{val_lati}: Latitud del establecimiento
#'     \item \code{val_long}: Longitud del establecimiento
#'   }
#'
#'   \strong{Categorización y operación:}
#'   \itemize{
#'     \item \code{cod_cate}: Código de categoría del establecimiento (ej: I-1,
#'       I-2, I-3, I-4, II-1, II-2, II-E, III-1, III-2, III-E)
#'     \item \code{doc_cate}: Documento de resolución de categorización
#'     \item \code{fec_inic_o}: Fecha de inicio de operaciones
#'     \item \code{des_hora_e}: Horario de atención del establecimiento
#'     \item \code{cod_cond_e}: Condición del establecimiento (ej: ACTIVO, INACTIVO)
#'   }
#'
#'   \strong{Red de salud:}
#'   \itemize{
#'     \item \code{cod_auto_s}: Código de autoridad sanitaria
#'     \item \code{des_auto_s}: Descripción de autoridad sanitaria (región)
#'     \item \code{cod_reds}: Código de red de salud
#'     \item \code{des_reds}: Nombre de la red de salud
#'     \item \code{cod_micr_r}: Código de micro red
#'     \item \code{des_micr_r}: Nombre de la micro red
#'   }
#'
#'   \strong{Datos del representante:}
#'   \itemize{
#'     \item \code{cod_nruc}: Número de RUC de la institución
#'     \item \code{nom_repr_l}: Nombre del representante legal
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
#'   \item Fuente: Superintendencia Nacional de Salud (SUSALUD)
#'   \item Registro: RENIPRESS (Registro Nacional de IPRESS)
#'   \item Año de actualización: 2024
#'   \item Nivel: Establecimiento de salud
#'   \item Aplicación: Análisis de acceso a servicios de salud, planificación
#'     sanitaria y estudios epidemiológicos
#' }
#'
#' **Sistema de categorización de establecimientos de salud en Perú:**
#'
#' La categorización determina la capacidad resolutiva del establecimiento:
#' \itemize{
#'   \item \strong{Primer nivel (I-1 a I-4)}: Atención básica y preventiva
#'     \itemize{
#'       \item I-1: Puesto de salud (técnico de enfermería)
#'       \item I-2: Puesto de salud (profesional de salud)
#'       \item I-3: Centro de salud sin internamiento
#'       \item I-4: Centro de salud con internamiento
#'     }
#'   \item \strong{Segundo nivel (II-1, II-2, II-E)}: Atención especializada
#'     \itemize{
#'       \item II-1: Hospital I
#'       \item II-2: Hospital II
#'       \item II-E: Hospital especializado
#'     }
#'   \item \strong{Tercer nivel (III-1, III-2, III-E)}: Alta especialización
#'     \itemize{
#'       \item III-1: Hospital III
#'       \item III-2: Instituto especializado
#'       \item III-E: Instituto especializado de alta complejidad
#'     }
#' }
#'
#' **Tipos de IPRESS:**
#' \enumerate{
#'   \item \strong{Establecimientos sin internamiento}: Consultorios, puestos
#'     de salud, centros médicos, policlínicos
#'   \item \strong{Establecimientos con internamiento}: Hospitales, clínicas,
#'     centros de salud con camas
#'   \item \strong{Servicios médicos de apoyo}: Laboratorios, centros de
#'     diagnóstico por imágenes, bancos de sangre, patología clínica
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
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/ipress/}
#'
#' **NOTA:** Las geometrías son tipo POINT (puntos) y representan la ubicación
#' de cada establecimiento de salud.
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_ipress()
#'
#' # Cargar IPRESS de un departamento completo
#' ipress_cusco <- get_ipress(departamento = "CUSCO")
#'
#' # Filtrar por provincia específica
#' ipress_prov_cusco <- get_ipress(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Filtrar por distrito específico
#' ipress_wanchaq <- get_ipress(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "WANCHAQ"
#' )
#'
#' # Cargar múltiples departamentos
#' ipress_sur <- get_ipress(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Mapa de IPRESS por tipo de establecimiento
#' ggplot(ipress_cusco) +
#'   geom_sf(aes(color = des_tipo_e), size = 1.5, alpha = 0.7) +
#'   scale_color_brewer(palette = "Set1", name = "Tipo") +
#'   labs(
#'     title = "Instituciones Prestadoras de Servicios de Salud - Cusco",
#'     subtitle = "SUSALUD 2024",
#'     caption = "Fuente: SUSALUD - RENIPRESS"
#'   ) +
#'   theme_minimal()
#'
#' # Resumen por tipo de establecimiento
#' ipress_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(des_tipo_e, sort = TRUE)
#'
#' # Resumen por institución administradora
#' ipress_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(des_inst_d, sort = TRUE)
#'
#' # Filtrar establecimientos públicos activos
#' ipress_publicos <- ipress_cusco |>
#'   filter(
#'     des_inst_d == "GOBIERNO REGIONAL",
#'     cod_cond_e == "ACTIVO"
#'   )
#'
#' # Análisis por categoría
#' ipress_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(cod_cate, sort = TRUE) |>
#'   head(10)
#'
#' # Mapa de hospitales y clínicas (establecimientos con internamiento)
#' ipress_cusco |>
#'   filter(cod_tipo_e == "2") |>
#'   ggplot() +
#'   geom_sf(aes(color = des_inst_d), size = 3) +
#'   scale_color_brewer(palette = "Dark2", name = "Administrador") +
#'   labs(
#'     title = "Establecimientos de Salud con Internamiento - Cusco",
#'     subtitle = "Hospitales y Clínicas",
#'     caption = "Fuente: SUSALUD - RENIPRESS 2024"
#'   ) +
#'   theme_minimal()
#'
#' # Análisis de cobertura por distrito
#' cobertura_distrito <- ipress_cusco |>
#'   sf::st_drop_geometry() |>
#'   group_by(nombdist) |>
#'   summarise(
#'     total_ipress = n(),
#'     con_internamiento = sum(cod_tipo_e == "2"),
#'     sin_internamiento = sum(cod_tipo_e == "1"),
#'     servicios_apoyo = sum(cod_tipo_e == "3"),
#'     .groups = "drop"
#'   ) |>
#'   arrange(desc(total_ipress))
#'
#' # Distribución por red de salud
#' ipress_cusco |>
#'   sf::st_drop_geometry() |>
#'   filter(!is.na(des_reds)) |>
#'   count(des_reds, sort = TRUE)
#'
#' # Mapa de establecimientos por categoría (primer nivel)
#' ipress_cusco |>
#'   filter(grepl("^I-", cod_cate)) |>
#'   ggplot() +
#'   geom_sf(aes(color = cod_cate), size = 2, alpha = 0.7) +
#'   scale_color_viridis_d(name = "Categoría") +
#'   labs(
#'     title = "Establecimientos de Primer Nivel - Cusco",
#'     subtitle = "Categorías I-1 a I-4",
#'     caption = "Fuente: SUSALUD - RENIPRESS 2024"
#'   ) +
#'   theme_minimal()
#'
#' # Buscar establecimientos por nombre
#' hospitales_regionales <- ipress_cusco |>
#'   filter(grepl("REGIONAL|HOSPITAL", nom_esta, ignore.case = TRUE))
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link{get_distritos}}, \code{\link{get_centros_poblados}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' Superintendencia Nacional de Salud (SUSALUD). Registro Nacional de
#' Instituciones Prestadoras de Servicios de Salud (RENIPRESS) 2024.
#'
#' Ministerio de Salud del Perú. Norma Técnica de Categorización de
#' Establecimientos de Salud.
#'
#' Repositorio DEMARCA en OSF: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf
#' @importFrom utils head
get_ipress <- function(departamento = NULL,
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
      "instituciones_prestadoras_servicios_salud_amazonas.gpkg",
      "instituciones_prestadoras_servicios_salud_ancash.gpkg",
      "instituciones_prestadoras_servicios_salud_apurimac.gpkg",
      "instituciones_prestadoras_servicios_salud_arequipa.gpkg",
      "instituciones_prestadoras_servicios_salud_ayacucho.gpkg",
      "instituciones_prestadoras_servicios_salud_cajamarca.gpkg",
      "instituciones_prestadoras_servicios_salud_callao.gpkg",
      "instituciones_prestadoras_servicios_salud_cusco.gpkg",
      "instituciones_prestadoras_servicios_salud_huancavelica.gpkg",
      "instituciones_prestadoras_servicios_salud_huanuco.gpkg",
      "instituciones_prestadoras_servicios_salud_ica.gpkg",
      "instituciones_prestadoras_servicios_salud_junin.gpkg",
      "instituciones_prestadoras_servicios_salud_la_libertad.gpkg",
      "instituciones_prestadoras_servicios_salud_lambayeque.gpkg",
      "instituciones_prestadoras_servicios_salud_lima.gpkg",
      "instituciones_prestadoras_servicios_salud_loreto.gpkg",
      "instituciones_prestadoras_servicios_salud_madre_de_dios.gpkg",
      "instituciones_prestadoras_servicios_salud_moquegua.gpkg",
      "instituciones_prestadoras_servicios_salud_pasco.gpkg",
      "instituciones_prestadoras_servicios_salud_piura.gpkg",
      "instituciones_prestadoras_servicios_salud_puno.gpkg",
      "instituciones_prestadoras_servicios_salud_san_martin.gpkg",
      "instituciones_prestadoras_servicios_salud_tacna.gpkg",
      "instituciones_prestadoras_servicios_salud_tumbes.gpkg",
      "instituciones_prestadoras_servicios_salud_ucayali.gpkg"
    ),
    id_osf = c(
      "3b85s", "b9qap", "zvjwb", "8xydq", "zmjgx",
      "wk632", "ky3av", "pzfsx", "s768q", "c6w3j",
      "w9j8u", "gj95n", "28m7j", "4rt9x", "ngvu9",
      "2dy5w", "gya9x", "y5pma", "de8y3", "evxb5",
      "azmq2", "my4wp", "uenj3", "br4fv", "tg4vh"
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
    message("\nUso: get_ipress(departamento = 'CUSCO')")
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
      "Use get_ipress() para ver la lista completa.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "ipress")
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
            message("\u2713 Cargado: ", dep, " (", nrow(datos_dep), " establecimientos)")
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
          "No se encontraron establecimientos para la(s) provincia(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por provincia: ",
          nrow(datos_final),
          " establecimientos"
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
          "No se encontraron establecimientos para el/los distrito(s): ",
          paste(distrito, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por distrito: ",
          nrow(datos_final),
          " establecimientos"
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
      nrow(datos_final), " establecimientos de salud (IPRESS)"
    )
  }

  return(datos_final)
}
