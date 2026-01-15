#' Obtener Servicios Educativos del Perú
#'
#' Descarga y carga la información de servicios educativos del Perú, elaborado por
#' el Ministerio de Educación (MINEDU). Un servicio educativo es el conjunto de
#' actividades educativas diseñadas y organizadas para lograr un objetivo de
#' aprendizaje. Los datos incluyen geometría tipo POINT e información sobre
#' nivel educativo, modalidad, gestión, turno y características del servicio.
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
#' @param show_progress Logical. Si \code{TRUE} (por defecto), muestra mensajes
#'   informativos sobre el progreso de la descarga. Si \code{FALSE}, ejecuta de
#'   forma silenciosa.
#' @param force_update Logical. Si \code{TRUE}, fuerza una nueva descarga del
#'   archivo incluso si existe en caché. Por defecto \code{FALSE}.
#'
#' @return Un objeto \code{sf} (simple feature) con geometría tipo POINT que contiene
#'   información de los servicios educativos, incluyendo:
#'
#'   \strong{Identificación del servicio:}
#'   \itemize{
#'     \item Geometría POINT (coordenadas del servicio educativo)
#'     \item \code{cod_mod}: Código modular del servicio educativo (identificador
#'       único asignado por MINEDU)
#'     \item \code{anexo}: Número de anexo del servicio educativo (0 si es sede
#'       principal)
#'     \item \code{cen_edu}: Nombre del centro educativo o institución
#'     \item \code{codinst}: Código de la institución (RUC o código interno)
#'   }
#'
#'   \strong{Nivel y modalidad educativa:}
#'   \itemize{
#'     \item \code{d_niv_mod}: Nivel y modalidad del servicio educativo
#'     \item \code{d_forma}: Forma de atención:
#'       \itemize{
#'         \item ESCOLARIZADO: Educación presencial en aula
#'         \item NO ESCOLARIZADO: Programas no escolarizados (PRONOEI, etc.)
#'         \item NO APLICA:
#'       }
#'   }
#'
#'   \strong{Gestión y dependencia:}
#'   \itemize{
#'     \item \code{d_gestion}: Tipo de gestión:
#'       \itemize{
#'         \item PÚBLICA DE GESTIÓN DIRECTA: Administrada directamente por el Estado
#'         \item PÚBLICA DE GESTIÓN PRIVADA: Pública pero administrada por privados
#'           (convenios)
#'         \item PRIVADA: Administración particular
#'       }
#'     \item \code{d_ges_dep}: Dependencia específica de la gestión
#'     \item \code{d_dreugel}: DRE o UGEL de dependencia administrativa
#'   }
#'
#'   \strong{Características del servicio:}
#'   \itemize{
#'     \item \code{d_cod_car}: Característica del docente según número de grados
#'       a cargo:
#'       \itemize{
#'         \item POLIDOCENTE COMPLETO: Un docente por grado/sección
#'         \item POLIDOCENTE MULTIGRADO: Docentes atienden varios grados
#'         \item UNIDOCENTE: Un solo docente para todos los grados
#'         \item NO APLICA: No corresponde al nivel educativo
#'         \item NO DISPONIBLE:
#'       }
#'     \item \code{d_tipssexo}: Tipo de servicio según sexo de estudiantes:
#'       \itemize{
#'         \item MIXTO: Atiende a ambos sexos
#'         \item VARONES: Solo varones
#'         \item MUJERES: Solo mujeres
#'         \item NO APLICA
#'       }
#'     \item \code{d_tipoprog}: Tipo de programa (para servicios no escolarizados)
#'     \item \code{d_cod_tur}: Turno de atención
#'   }
#'
#'   \strong{Ubicación geográfica:}
#'   \itemize{
#'     \item \code{departamen}: Nombre del departamento
#'     \item \code{provincia}: Nombre de la provincia
#'     \item \code{nlat_ie}: Latitud del servicio educativo (coordenada Y)
#'     \item \code{nlong_ie}: Longitud del servicio educativo (coordenada X)
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
#'   \item Nivel: Servicio educativo
#'   \item Aplicación: Análisis de oferta educativa, planificación de política
#'     educativa y estudios de cobertura por nivel y modalidad
#' }
#'
#' **Diferencia entre local educativo y servicio educativo:**
#' \itemize{
#'   \item \strong{Local educativo}: Infraestructura física (inmueble) donde
#'     funcionan los servicios. Ver \code{\link{get_locales_educativos}}.
#'   \item \strong{Servicio educativo}: Unidad de gestión pedagógica que brinda
#'     un nivel o modalidad específica de educación. Un local puede albergar
#'     múltiples servicios educativos.
#' }
#'
#' **Sistema educativo peruano - Niveles y modalidades:**
#'
#' \strong{Educación Básica Regular (EBR):}
#' \itemize{
#'   \item \strong{Inicial}: Cuna (0-2 años), Jardín (3-5 años)
#'   \item \strong{Primaria}: 6 grados (6-11 años)
#'   \item \strong{Secundaria}: 5 grados (12-16 años)
#' }
#'
#'
#' **Clasificación por característica docente (d_cod_car):**
#' \itemize{
#'   \item \strong{Polidocente completo}: Cada sección tiene su propio docente.
#'     Típico en zonas urbanas.
#'   \item \strong{Polidocente multigrado}: Varios docentes que atienden más de
#'     un grado cada uno.
#'   \item \strong{Unidocente}: Un solo docente atiende todos los grados.
#'     Frecuente en zonas rurales con poca matrícula.
#' }
#'
#' **Filtrado jerárquico:**
#' Los filtros se aplican en cascada:
#' \enumerate{
#'   \item Primero se cargan los departamentos especificados
#'   \item Luego se filtran las provincias (si se especifican)
#' }
#'
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/servicios_educativos/}
#'
#' **NOTA:** Las geometrías son tipo POINT (puntos) y representan la ubicación
#' de cada servicio educativo.
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_servicios_educativos()
#'
#' # Cargar servicios educativos de un departamento completo
#' serv_cusco <- get_servicios_educativos(departamento = "CUSCO")
#'
#' # Filtrar por provincia específica
#' serv_prov_cusco <- get_servicios_educativos(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Filtrar por distrito específico
#' serv_prov_urubamba <- get_servicios_educativos(
#'   departamento = "CUSCO",
#'   provincia = "URUBAMBA"
#' )
#'
#' # Cargar múltiples departamentos
#' serv_sur <- get_servicios_educativos(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Mapa de servicios educativos por nivel
#' serv_cusco |>
#'   filter(d_niv_mod %in% c("PRIMARIA", "SECUNDARIA", "INICIAL - JARDÍN")) |>
#'   ggplot() +
#'   geom_sf(aes(color = d_niv_mod), size = 1, alpha = 0.6) +
#'   scale_color_brewer(palette = "Set1", name = "Nivel") +
#'   labs(
#'     title = "Servicios Educativos por Nivel - Cusco",
#'     subtitle = "Educación Básica Regular",
#'     caption = "Fuente: MINEDU - Padrón de Instituciones Educativas"
#'   ) +
#'   theme_minimal()
#'
#' # Resumen por nivel y modalidad
#' serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(d_niv_mod, sort = TRUE)
#'
#' # Distribución por tipo de gestión
#' serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(d_gestion, sort = TRUE)
#'
#' # Servicios públicos vs privados
#' serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(d_ges_dep, sort = TRUE)
#'
#' # Filtrar solo educación inicial
#' serv_inicial <- serv_cusco |>
#'   filter(grepl("INICIAL", d_niv_mod))
#'
#' # Filtrar servicios de secundaria pública
#' serv_secund_publica <- serv_cusco |>
#'   filter(
#'     d_niv_mod == "SECUNDARIA",
#'     d_gestion == "PÚBLICA DE GESTIÓN DIRECTA"
#'   )
#'
#' # Análisis de escuelas unidocentes (rurales)
#' serv_unidocente <- serv_cusco |>
#'   filter(d_cod_car == "UNIDOCENTE")
#'
#' # Mapa de escuelas unidocentes
#' ggplot(serv_unidocente) +
#'   geom_sf(color = "darkred", size = 1.5, alpha = 0.7) +
#'   labs(
#'     title = "Servicios Educativos Unidocentes - Cusco",
#'     subtitle = "Un docente para todos los grados",
#'     caption = "Fuente: MINEDU"
#'   ) +
#'   theme_minimal()
#'
#' # Análisis por UGEL
#' serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(d_dreugel, sort = TRUE)
#'
#' # Distribución por turno de atención
#' serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   count(d_cod_tur, sort = TRUE)
#'
#' # Servicios no escolarizados (PRONOEI, etc.)
#' serv_no_escolarizado <- serv_cusco |>
#'   filter(d_forma == "NO ESCOLARIZADO")
#'
#' # Análisis de cobertura por provincia y nivel
#' cobertura <- serv_cusco |>
#'   sf::st_drop_geometry() |>
#'   filter(d_niv_mod %in% c("PRIMARIA", "SECUNDARIA")) |>
#'   group_by(provincia, d_niv_mod) |>
#'   summarise(
#'     total_servicios = n(),
#'     pct_publica = round(
#'       sum(d_gestion == "PÚBLICA DE GESTIÓN DIRECTA") / n() * 100, 1
#'     ),
#'     .groups = "drop"
#'   ) |>
#'   tidyr::pivot_wider(
#'     names_from = d_niv_mod,
#'     values_from = c(total_servicios, pct_publica)
#'   )
#'
#' # Mapa comparativo público vs privado
#' serv_cusco |>
#'   filter(d_niv_mod == "SECUNDARIA") |>
#'   mutate(
#'     tipo_gestion = ifelse(
#'       grepl("PÚBLICA", d_gestion), "Pública", "Privada"
#'     )
#'   ) |>
#'   ggplot() +
#'   geom_sf(aes(color = tipo_gestion), size = 2, alpha = 0.6) +
#'   scale_color_manual(
#'     values = c("Pública" = "darkblue", "Privada" = "darkred"),
#'     name = "Gestión"
#'   ) +
#'   labs(
#'     title = "Servicios de Secundaria por Tipo de Gestión - Cusco",
#'     caption = "Fuente: MINEDU"
#'   ) +
#'   theme_minimal()
#'
#' # CETPRO - Educación técnico productiva
#' cetpro <- serv_cusco |>
#'   filter(grepl("TÉCNICO PRODUCTIVA", d_niv_mod))
#'
#' # Institutos superiores
#' superiores <- serv_cusco |>
#'   filter(grepl("SUPERIOR", d_niv_mod))
#' }
#'
#' @seealso
#' \code{\link{get_locales_educativos}}, \code{\link{get_departamentos}},
#' \code{\link{get_provincias}}, \code{\link{get_distritos}},
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
get_servicios_educativos <- function(departamento = NULL,
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
      "servicios_educativos_amazonas.gpkg",
      "servicios_educativos_ancash.gpkg",
      "servicios_educativos_apurimac.gpkg",
      "servicios_educativos_arequipa.gpkg",
      "servicios_educativos_ayacucho.gpkg",
      "servicios_educativos_cajamarca.gpkg",
      "servicios_educativos_callao.gpkg",
      "servicios_educativos_cusco.gpkg",
      "servicios_educativos_huancavelica.gpkg",
      "servicios_educativos_huanuco.gpkg",
      "servicios_educativos_ica.gpkg",
      "servicios_educativos_junin.gpkg",
      "servicios_educativos_la_libertad.gpkg",
      "servicios_educativos_lambayeque.gpkg",
      "servicios_educativos_lima.gpkg",
      "servicios_educativos_loreto.gpkg",
      "servicios_educativos_madre_de_dios.gpkg",
      "servicios_educativos_moquegua.gpkg",
      "servicios_educativos_pasco.gpkg",
      "servicios_educativos_piura.gpkg",
      "servicios_educativos_puno.gpkg",
      "servicios_educativos_san_martin.gpkg",
      "servicios_educativos_tacna.gpkg",
      "servicios_educativos_tumbes.gpkg",
      "servicios_educativos_ucayali.gpkg"
    ),
    id_osf = c(
      "zv64n", "wy42d", "kgq2p", "sujwm", "y8rnk",
      "wfc8b", "ndcyf", "9nkeb", "kebgs", "nb3cu",
      "v9g8d", "q76fj", "54hek", "6b9tm", "csyxd",
      "bv4t7", "xe5sj", "vmzfg", "9w2ne", "ywdgc",
      "s5mvt", "tk842", "qrdfb", "mj5gf", "38qaw"
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
    message("\nUso: get_servicios_educativos(departamento = 'CUSCO')")
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
      "Use get_servicios_educativos() para ver la lista completa.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "servicios_educativos")
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
            message("\u2713 Cargado: ", dep, " (", nrow(datos_dep), " servicios educativos)")
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
          "No se encontraron servicios educativos para la(s) provincia(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por provincia: ",
          nrow(datos_final),
          " servicios educativos"
        )
      }
    } else {
      warning("No se encontr\u00f3 la columna 'provincia' para filtrar", call. = FALSE)
    }
  }

  # Filtro por DISTRITO
 #if (!is.null(distrito)) {
 #  # Nota: En la estructura proporcionada no se ve columna 'distrito'
 #  # Verificar si existe en los datos reales
 #  col_distrito <- intersect(c("distrito", "nombdist"), names(datos_final))

 #  if (length(col_distrito) > 0) {
 #    col_usar <- col_distrito[1]
 #    dist_input <- toupper(trimws(distrito))
 #    datos_final <- datos_final[toupper(datos_final[[col_usar]]) %in% dist_input, ]

 #    if (nrow(datos_final) == 0) {
 #      warning(
 #        "No se encontraron servicios educativos para el/los distrito(s): ",
 #        paste(distrito, collapse = ", "),
 #        call. = FALSE
 #      )
 #      return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
 #    }

 #    if (show_progress) {
 #      message(
 #        "\u2713 Filtrado por distrito: ",
 #        nrow(datos_final),
 #        " servicios educativos"
 #      )
 #    }
 #  } else {
 #    warning(
 #      "No se encontr\u00f3 columna de distrito para filtrar. ",
 #      "Los datos solo contienen: departamen, provincia",
 #      call. = FALSE
 #    )
 #  }
 #}

  # ==========================================================================
  # 7. MENSAJE FINAL
  # ==========================================================================

  if (show_progress) {
    message(
      "\u2713 Datos cargados exitosamente: ",
      nrow(datos_final), " servicios educativos"
    )
  }

  return(datos_final)
}
