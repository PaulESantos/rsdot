#' Obtener Densidad y Crecimiento Poblacional por Departamento
#'
#' Descarga y carga la información de densidad y crecimiento poblacional intercensal
#' por departamento desde el repositorio OSF de DEMARCA. Los datos provienen de los
#' censos de población 2007 y 2017 del INEI, procesados mediante interpolación espacial.
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
#' @return Un objeto \code{sf} (simple feature) con información de densidad y
#'   crecimiento poblacional, incluyendo:
#'   \itemize{
#'     \item Geometría de los polígonos interpolados (con geometrías validadas)
#'     \item \code{ubigeo}: Código UBIGEO del distrito
#'     \item \code{nombdep}: Nombre del departamento
#'     \item \code{nombprov}: Nombre de la provincia
#'     \item \code{nombdist}: Nombre del distrito
#'     \item \code{nivel}: Nivel de crecimiento poblacional
#'     \item \code{rango}: Rango de tasa de crecimiento
#'     \item \code{descrip}: Descripción de la categoría
#'   }
#'
#'   Si se solicitan múltiples departamentos, retorna un objeto sf combinado.
#'
#' @details
#' La función descarga datos desde OSF (Open Science Framework) y los almacena
#' en caché durante la sesión de R. Los datos están en formato GeoPackage (.gpkg).
#'
#' **Metodología de los datos:**
#' \itemize{
#'   \item Fuente: Censos de Población INEI 2007 y 2017
#'   \item Cobertura: Centros poblados con población > 150 habitantes
#'   \item Técnica: Interpolación espacial para representación continua
#'   \item Variable: Tasa de crecimiento poblacional intercensal anual
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
#' El caché se almacena en: \code{tempdir()/DEMARCA_cache/densidad_poblacional/}
#'
#' **NOTA:** Las geometrías se validan automáticamente con \code{st_make_valid()}
#' para prevenir errores en operaciones espaciales posteriores.
#'
#' @examples
#' \dontrun{
#' # Ver departamentos disponibles
#' get_densidad_poblacional()
#'
#' # Cargar un departamento completo
#' densidad_cusco <- get_densidad_poblacional(departamento = "CUSCO")
#'
#' # Cargar una provincia específica
#' densidad_prov_cusco <- get_densidad_poblacional(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO"
#' )
#'
#' # Cargar un distrito específico
#' densidad_wanchaq <- get_densidad_poblacional(
#'   departamento = "CUSCO",
#'   provincia = "CUSCO",
#'   distrito = "WANCHAQ"
#' )
#'
#' # Cargar múltiples departamentos
#' densidad_sur <- get_densidad_poblacional(
#'   departamento = c("CUSCO", "PUNO", "AREQUIPA")
#' )
#'
#' # Filtrar múltiples provincias
#' densidad_valle <- get_densidad_poblacional(
#'   departamento = "CUSCO",
#'   provincia = c("CUSCO", "URUBAMBA", "CALCA")
#' )
#'
#' # Visualización con ggplot2
#' library(ggplot2)
#' library(dplyr)
#'
#' # Mapa de nivel de crecimiento
#' ggplot(densidad_cusco) +
#'   geom_sf(aes(fill = nivel), color = NA) +
#'   scale_fill_brewer(
#'     palette = "RdYlGn",
#'     name = "Nivel de\nCrecimiento"
#'   ) +
#'   labs(
#'     title = "Crecimiento Poblacional por Nivel",
#'     subtitle = "Departamento de Cusco (2007-2017)",
#'     caption = "Fuente: INEI | Visor - SDOT"
#'   ) +
#'   theme_minimal()
#'
#' # Capa agregada por distrito (un polígono por distrito)
#' distritos_anta <- prov_anta |>
#'  group_by(ubigeo, nombdist) |>
#'  summarise(.groups = "drop")
#' distritos_anta
#'
#' # Puntos para poner las etiquetas dentro de cada distrito
#' distritos_centroides <- st_point_on_surface(distritos_anta)
#' distritos_centroides
#'
#' ggplot() +
#'  # 1. Celdas / polígonos con el nivel de crecimiento
#'  geom_sf(
#'    data = prov_anta,
#'    aes(fill = nivel),
#'    color = NA
#'  ) +
#'  # 2. Contorno de distritos (para “hacerlos notar”)
#'  geom_sf(
#'    data = distritos_anta,
#'    fill  = NA,
#'    color = "grey20",
#'    linewidth = 0.4
#'  ) +
#'  # 3. Nombre de los distritos
#'  geom_sf_text(
#'    data = distritos_centroides,
#'    aes(label = nombdist),
#'    size = 3
#'  ) +
#'  scale_fill_brewer(
#'    palette = "RdYlGn",
#'    name    = "Nivel de\ncrecimiento"
#'  ) +
#'  labs(
#'    title    = "Crecimiento poblacional",
#'    subtitle = "Provincia de Anta (2007–2017)",
#'    caption  = "Fuente: INEI | Visor SDOT"
#'  ) +
#'  theme_minimal() +
#'  theme(
#'    legend.position = "right",
#'    panel.grid.major = element_line(linewidth = 0.2, colour = "grey90")
#'  )
#' }
#'
#' @seealso
#' \code{\link{get_departamentos}}, \code{\link{get_provincias}},
#' \code{\link{get_distritos}}, \code{\link[sf]{read_sf}},
#' \code{\link[sf]{st_make_valid}}
#'
#' @references
#' INEI. Censos Nacionales de Población y Vivienda 2007 y 2017.
#'
#' Repositorio DEMARCA en OSF: \url{https://osf.io/qy4j6/}
#'
#' @export
#' @importFrom utils download.file
#' @importFrom sf read_sf st_make_valid
#' @importFrom utils head
get_densidad_poblacional <- function(departamento = NULL,
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
      "densidad_crecimiento_poblacional_amazonas.gpkg",
      "densidad_crecimiento_poblacional_ancash.gpkg",
      "densidad_crecimiento_poblacional_apurimac.gpkg",
      "densidad_crecimiento_poblacional_arequipa.gpkg",
      "densidad_crecimiento_poblacional_ayacucho.gpkg",
      "densidad_crecimiento_poblacional_cajamarca.gpkg",
      "densidad_crecimiento_poblacional_callao.gpkg",
      "densidad_crecimiento_poblacional_cusco.gpkg",
      "densidad_crecimiento_poblacional_huancavelica.gpkg",
      "densidad_crecimiento_poblacional_huanuco.gpkg",
      "densidad_crecimiento_poblacional_ica.gpkg",
      "densidad_crecimiento_poblacional_junin.gpkg",
      "densidad_crecimiento_poblacional_la_libertad.gpkg",
      "densidad_crecimiento_poblacional_lambayeque.gpkg",
      "densidad_crecimiento_poblacional_lima.gpkg",
      "densidad_crecimiento_poblacional_loreto.gpkg",
      "densidad_crecimiento_poblacional_madre_de_dios.gpkg",
      "densidad_crecimiento_poblacional_moquegua.gpkg",
      "densidad_crecimiento_poblacional_pasco.gpkg",
      "densidad_crecimiento_poblacional_piura.gpkg",
      "densidad_crecimiento_poblacional_puno.gpkg",
      "densidad_crecimiento_poblacional_san_martin.gpkg",
      "densidad_crecimiento_poblacional_tacna.gpkg",
      "densidad_crecimiento_poblacional_tumbes.gpkg",
      "densidad_crecimiento_poblacional_ucayali.gpkg"
    ),
    id_osf = c(
      "tkur9", "ys832", "4h76w", "n36pk", "w2yhr",
      "86hrj", "wz3xs", "dkf83", "4yzxa", "4b2vm",
      "8cpqf", "qbez7", "nyjkc", "vmhyc", "rqdbg",
      "v3sqr", "j6k3z", "f7vn6", "em7au", "e8agr",
      "kp2d7", "k23rx", "rp4yf", "jdcnz", "74unj"
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
    message("\nUso: get_densidad_poblacional(departamento = 'CUSCO')")
    return(invisible(tabla_departamentos$departamento))
  }

  if (!is.character(departamento)) {
    stop("El par\u00e1metro 'departamento' debe ser un vector de caracteres",
         call. = FALSE)
  }

  # Normalizar nombres
  departamento_input <- toupper(trimws(departamento))
  departamento_input <- gsub("_", " ", departamento_input)

  # Validar departamentos
  departamentos_validos <- departamento_input %in% tabla_departamentos$departamento

  if (!all(departamentos_validos)) {
    departamentos_invalidos <- departamento_input[!departamentos_validos]
    stop(
      "Departamento(s) no v\u00e1lido(s): ",
      paste(departamentos_invalidos, collapse = ", "), "\n",
      "Use get_densidad_poblacional() para ver la lista completa.",
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

  ruta_cache_dir <- file.path(tempdir(), "DEMARCA_cache", "densidad_poblacional")
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

    url_descarga <- paste0("https://osf.io/", id_osf, "/download")
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
          datos_dep <- sf::read_sf(archivo_local, quiet = TRUE) |>
            dplyr::mutate(
              dplyr::across(
                dplyr::where(is.character),
                ~ fix_double_utf8(.x)
              ),
              dplyr::across(
                dplyr::where(is.factor),
                ~ factor(fix_double_utf8(as.character(.x)))
              )
            )

          # Normalizar nombres de columnas
          colnames(datos_dep) <- tolower(colnames(datos_dep))

          # Validar geometrías para prevenir errores
          if (show_progress) message("Validando geometr\u00edas...")
          datos_dep <- sf::st_make_valid(datos_dep)

          # Agregar columna departamento si no existe
          if (!"departamento" %in% names(datos_dep)) {
            datos_dep$departamento <- dep
          }

          # Corregir codificación en nombres
          if ("nombdist" %in% names(datos_dep)) {
            datos_dep$nombdist <- sub("A'", "\u00d1", datos_dep$nombdist, fixed = TRUE)
          }
          if ("nombprov" %in% names(datos_dep)) {
            datos_dep$nombprov <- sub("A'", "\u00d1", datos_dep$nombprov, fixed = TRUE)
          }

          lista_datos[[dep]] <- datos_dep

          if (show_progress) {
            message("\u2713 Cargado: ", dep, " (", nrow(datos_dep), " registros)")
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

    tryCatch(
      {
        datos_final <- do.call(rbind, lista_datos)
      },
      error = function(e) {
        # Fallback con sf
        datos_final <<- do.call(dplyr::bind_rows, lista_datos)
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
          "No se encontraron datos para la(s) provincia(s): ",
          paste(provincia, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por provincia: ",
          nrow(datos_final),
          " registros"
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
          "No se encontraron datos para el/los distrito(s): ",
          paste(distrito, collapse = ", "),
          call. = FALSE
        )
        return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))
      }

      if (show_progress) {
        message(
          "\u2713 Filtrado por distrito: ",
          nrow(datos_final),
          " registros"
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
      nrow(datos_final), " registros"
    )
  }

  return(datos_final)
}
