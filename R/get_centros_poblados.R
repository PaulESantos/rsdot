#' Obtener Centros Poblados y Asentamientos Dispersos del Perú
#'
#' Descarga y carga la información de centros poblados y asentamientos dispersos
#' recogidos en el Censo de Población y Vivienda 2017 del Instituto Nacional de
#' Estadística e Informática (INEI). Comprende viviendas particulares con ocupantes
#' presentes. Los datos incluyen geometría tipo POINT e información detallada sobre
#' población, vivienda y ubicación geográfica.
#'
#' @param departamento Character vector. Nombre(s) del/los departamento(s) a descargar.
#'   Opciones válidas: "AMAZONAS", "ANCASH", "APURIMAC", "AREQUIPA", "AYACUCHO",
#'   "CAJAMARCA", "CALLAO", "CUSCO", "HUANCAVELICA", "HUANUCO", "ICA", "JUNIN",
#'   "LA LIBERTAD", "LAMBAYEQUE", "LIMA", "LORETO", "MADRE DE DIOS", "MOQUEGUA",
#'   "PASCO", "PIURA", "PUNO", "SAN MARTIN", "TACNA", "TUMBES", "UCAYALI".
#'   No distingue entre mayúsculas y minúsculas. Si es \code{NULL}, muestra la
#'   lista de departamentos disponibles.
#' @param provincia Character vector. Nombre(s) de provincia(s) para filtrar los
#'   resultados (opcional). Se aplica después de cargar los datos.
#' @param distrito Character vector. Nombre(s) de distrito(s) para filtrar los
#'   resultados (opcional). Se aplica después de cargar los datos.
#' @param show_progress Logical. Si \code{TRUE} (por defecto), muestra mensajes
#'   informativos sobre el progreso de la descarga. Si \code{FALSE}, ejecuta de
#'   forma silenciosa.
#' @param force_update Logical. Si \code{TRUE}, fuerza una nueva descarga del
#'   archivo incluso si existe en caché. Por defecto \code{FALSE}.
#'
#' @return Un objeto \code{sf} (simple feature) con geometría tipo POINT
#'   que contiene información de centros poblados, incluyendo:
#'   \itemize{
#'     \item Geometría POINT (ubicación del centro poblado)
#'     \item \code{ubigeo}: Código de ubicación geográfica (6 dígitos)
#'     \item \code{codccpp}: Código del centro poblado (10 dígitos)
#'     \item \code{nombdep}: Nombre del departamento
#'     \item \code{nombprov}: Nombre de la provincia
#'     \item \code{nombdist}: Nombre del distrito
#'     \item \code{cen_pob}: Nombre del centro poblado
#'     \item \code{pob}: Población total
#'     \item \code{viv}: Total de viviendas
#'     \item \code{viv_part}: Viviendas particulares
#'     \item \code{viv_part_o}: Viviendas particulares ocupadas
#'     \item \code{viv_part_1}: Viviendas particulares con 1 o más ocupantes
#'     \item \code{pob_viv_pa}: Población en viviendas particulares
#'     \item \code{y}: Latitud (grados decimales)
#'     \item \code{x}: Longitud (grados decimales)
#'     \item \code{fuente}: Fuente de la información
#'     \item \code{revision}: Información de revisión
#'     \item \code{cap}: Indicador de capital
#'     \item \code{capital}: Tipo de capital si aplica
#'     \item \code{iddpto}: ID del departamento
#'     \item \code{idprov}: ID de la provincia
#'   }
#'
#'   Si se solicitan múltiples departamentos, retorna un objeto sf combinado.
#'
#' @details
#' La función descarga datos desde OSF (Open Science Framework) y los almacena
#' en caché durante la sesión de R. Los datos están en formato GeoPackage (.gpkg).
#'
#' **IMPORTANTE - Ubicación Referencial:**
#' La ubicación de los centros poblados es referencial. Las coordenadas pueden
#' diferir de las registradas en otras bases de datos oficiales o levantamientos
#' de campo específicos. Se recomienda validar las ubicaciones para estudios que
#' requieran precisión espacial.
#'
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/centros_poblados/}
#'
#' **NOTA:** Las geometrías son tipo POINT y representan la ubicación aproximada
#' de los centros poblados según el Censo 2017. El sistema de coordenadas es
#' WGS 84 (EPSG:4326).
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_centros_poblados()
#'
#' # Cargar centros poblados de un departamento
#' cp_tacna <- get_centros_poblados(departamento = "TACNA")
#'
#' # Cargar múltiples departamentos
#' cp_sur <- get_centros_poblados(
#'   departamento = c("TACNA", "MOQUEGUA")
#' )
#'
#' # Filtrar por provincia
#' cp_cusco_prov <- get_centros_poblados(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Clasificar por tamaño poblacional
#' cp_tacna <- cp_tacna |>
#'   mutate(
#'     categoria = case_when(
#'       pob >= 5000 ~ "Grande (5000+)",
#'       pob >= 1000 ~ "Mediano (1000-4999)",
#'       pob >= 500 ~ "Pequeño (500-999)",
#'       TRUE ~ "Muy pequeño (<500)"
#'     )
#'   )
#'
#' # Mapa de centros poblados por tamaño
#' ggplot(cp_tacna) +
#'   geom_sf(aes(color = categoria, size = pob), alpha = 0.6) +
#'   scale_size_continuous(range = c(0.5, 5)) +
#'   scale_color_brewer(palette = "Spectral", direction = -1) +
#'   labs(
#'     title = "Centros Poblados de Tacna",
#'     subtitle = "Clasificados por Tamaño - Censo 2017",
#'     color = "Categoría",
#'     size = "Población",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Análisis de vivienda
#' cp_tacna |>
#'   st_drop_geometry() |>
#'   filter(viv > 0) |>
#'   summarise(
#'     centros = n(),
#'     pob_total = sum(pob, na.rm = TRUE),
#'     viv_total = sum(viv, na.rm = TRUE),
#'     pob_por_vivienda = pob_total / viv_total,
#'     tasa_ocupacion = sum(viv_part_o) / sum(viv_part) * 100
#'   )
#'
#' # Distribución por distrito
#' cp_tacna |>
#'   st_drop_geometry() |>
#'   group_by(nombdist) |>
#'   summarise(
#'     n_centros = n(),
#'     poblacion = sum(pob, na.rm = TRUE),
#'     viviendas = sum(viv, na.rm = TRUE)
#'   ) |>
#'   arrange(desc(poblacion))
#'
#' # Análisis de densidad de vivienda
#' cp_tacna |>
#'   st_drop_geometry() |>
#'   filter(viv > 0) |>
#'   mutate(pob_por_viv = pob / viv) |>
#'   summarise(
#'     min = min(pob_por_viv),
#'     q1 = quantile(pob_por_viv, 0.25),
#'     mediana = median(pob_por_viv),
#'     promedio = mean(pob_por_viv),
#'     q3 = quantile(pob_por_viv, 0.75),
#'     max = max(pob_por_viv)
#'   )
#' }
#'
#' @seealso
#' \code{\link{get_capitales}}, \code{\link{get_departamentos}},
#' \code{\link[sf]{read_sf}}
#'
#' @references
#' INEI. Censo de Población y Vivienda 2017. Instituto Nacional de Estadística e
#' Informática.
#'
#' Visor SDOT: \url{https://geosdot.servicios.gob.pe/visor/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf st_sf st_sfc st_drop_geometry
#' @importFrom utils head
get_centros_poblados <- function(departamento = NULL,
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
      "ccpp_asentamientos_dispersos_amazonas.gpkg",
      "ccpp_asentamientos_dispersos_ancash.gpkg",
      "ccpp_asentamientos_dispersos_apurimac.gpkg",
      "ccpp_asentamientos_dispersos_arequipa.gpkg",
      "ccpp_asentamientos_dispersos_ayacucho.gpkg",
      "ccpp_asentamientos_dispersos_cajamarca.gpkg",
      "ccpp_asentamientos_dispersos_callao.gpkg",
      "ccpp_asentamientos_dispersos_cusco.gpkg",
      "ccpp_asentamientos_dispersos_huancavelica.gpkg",
      "ccpp_asentamientos_dispersos_huanuco.gpkg",
      "ccpp_asentamientos_dispersos_ica.gpkg",
      "ccpp_asentamientos_dispersos_junin.gpkg",
      "ccpp_asentamientos_dispersos_la_libertad.gpkg",
      "ccpp_asentamientos_dispersos_lambayeque.gpkg",
      "ccpp_asentamientos_dispersos_lima.gpkg",
      "ccpp_asentamientos_dispersos_loreto.gpkg",
      "ccpp_asentamientos_dispersos_madre_de_dios.gpkg",
      "ccpp_asentamientos_dispersos_moquegua.gpkg",
      "ccpp_asentamientos_dispersos_pasco.gpkg",
      "ccpp_asentamientos_dispersos_piura.gpkg",
      "ccpp_asentamientos_dispersos_puno.gpkg",
      "ccpp_asentamientos_dispersos_san_martin.gpkg",
      "ccpp_asentamientos_dispersos_tacna.gpkg",
      "ccpp_asentamientos_dispersos_tumbes.gpkg",
      "ccpp_asentamientos_dispersos_ucayali.gpkg"
    ),
    id_osf = c(
      "wu5fq", "de456", "e89ha", "vhj7a", "xe647",
      "xrp3c", "x8b5k", "2xa5u", "btxwh", "xpw7u",
      "5wbd6", "hv8rk", "zja4k", "ftyq6", "tnhzg",
      "sufjw", "r25yv", "fsuy5", "s6bvk", "4rqcm",
      "9rp6m", "xad5r", "pbzxw", "qbx2m", "gd9ms"
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
    message("\nUso: get_centros_poblados(departamento = 'TACNA')")
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
      "Use get_centros_poblados() para ver la lista completa.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "centros_poblados")
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
            n_cp <- nrow(datos_dep)
            pob_total <- sum(datos_dep$pob, na.rm = TRUE)
            viv_total <- sum(datos_dep$viv, na.rm = TRUE)
            message(
              "\u2713 Cargado: ", dep,
              " (", format(n_cp, big.mark = ","), " centros poblados, ",
              format(pob_total, big.mark = ","), " habitantes, ",
              format(viv_total, big.mark = ","), " viviendas)"
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
  # 6. APLICAR FILTROS ADICIONALES
  # ==========================================================================

  if (!is.null(provincia)) {
    if (!is.character(provincia)) {
      stop("El par\u00e1metro 'provincia' debe ser un vector de caracteres",
           call. = FALSE)
    }

    provincia_input <- toupper(trimws(provincia))
    datos_final <- datos_final[toupper(datos_final$nombprov) %in% provincia_input, ]

    if (nrow(datos_final) == 0) {
      warning(
        "No se encontraron centros poblados para la(s) provincia(s): ",
        paste(provincia, collapse = ", "),
        call. = FALSE,
        immediate. = TRUE
      )
    } else if (show_progress) {
      message(
        "\u2713 Filtrado por provincia: ",
        format(nrow(datos_final), big.mark = ","), " centros poblados"
      )
    }
  }

  if (!is.null(distrito)) {
    if (!is.character(distrito)) {
      stop("El par\u00e1metro 'distrito' debe ser un vector de caracteres",
           call. = FALSE)
    }

    distrito_input <- toupper(trimws(distrito))
    datos_final <- datos_final[toupper(datos_final$nombdist) %in% distrito_input, ]

    if (nrow(datos_final) == 0) {
      warning(
        "No se encontraron centros poblados para el/los distrito(s): ",
        paste(distrito, collapse = ", "),
        call. = FALSE,
        immediate. = TRUE
      )
    } else if (show_progress) {
      message(
        "\u2713 Filtrado por distrito: ",
        format(nrow(datos_final), big.mark = ","), " centros poblados"
      )
    }
  }

  # ==========================================================================
  # 7. MENSAJE FINAL
  # ==========================================================================

  if (show_progress && nrow(datos_final) > 0) {
    pob_total <- sum(datos_final$pob, na.rm = TRUE)
    viv_total <- sum(datos_final$viv, na.rm = TRUE)
    n_deps <- length(unique(datos_final$nombdep))
    message(
      "\u2713 Datos cargados exitosamente: ",
      format(nrow(datos_final), big.mark = ","), " centros poblados (",
      n_deps, ifelse(n_deps == 1, " departamento, ", " departamentos, "),
      format(pob_total, big.mark = ","), " habitantes, ",
      format(viv_total, big.mark = ","), " viviendas)"
    )
  }

  return(datos_final)
}
