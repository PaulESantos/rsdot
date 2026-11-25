# Internal Utility Functions for rsdot
#
# A set of auxiliary functions for secure downloads, spatial file reading,
# and persistent cache management.
#
# @name utils_internal
NULL

# Get persistent cache directory
#
# @return Path to the cache directory.
# @importFrom tools R_user_dir
get_cache_dir <- function() {
    cache_dir <- tools::R_user_dir("rsdot", which = "cache")
    if (!dir.exists(cache_dir)) {
        dir.create(cache_dir, recursive = TRUE)
    }
    return(cache_dir)
}

# Check if local staging mode is enabled
is_staging_enabled <- function() {
    isTRUE(getOption("rsdot.use_staging", FALSE)) ||
        tolower(Sys.getenv("RSDOT_USE_STAGING", "false")) %in% c("true", "1", "yes")
}

# Find staging GeoPackage in local sdot_data directory
find_staging_file <- function(file_name) {
    custom_dir <- getOption("rsdot.staging_dir", NULL)
    candidatos_base <- c(
        custom_dir,
        "sdot_data",
        "../sdot_data",
        "../../sdot_data",
        "d:/sdot_project/sdot_data"
    )
    candidatos_base <- unique(candidatos_base[!is.null(candidatos_base) & dir.exists(candidatos_base)])
    if (length(candidatos_base) == 0) return(NULL)

    base_dir <- candidatos_base[1]

    subdirs_comunes <- c(
        "Distrito2025",
        "modelamiento_geografico",
        file.path("modelamiento_geografico", "tiempo_deplazamiento_capital_cercano", "gpkg_departamentos"),
        "analisis_geografico",
        file.path("peligros", "gpkg_departamentos"),
        file.path("peligros", "gpkg_departamentos", "areas_exposicion_inundacion"),
        file.path("peligros", "gpkg_departamentos", "areas_exposicion_movimientos_masa"),
        file.path("peligros", "gpkg_departamentos", "inventario_inundaciones"),
        file.path("peligros", "gpkg_departamentos", "puntos_criticos_inundaciones"),
        file.path("peligros", "gpkg_departamentos", "tramos_criticos_inundaciones"),
        file.path("peligros", "gpkg_departamentos", "zonas_criticas_movimientos_masa"),
        file.path("riesgos", "gpkg_departamentos"),
        file.path("riesgos", "gpkg_departamentos", "evaluacion_riesgos"),
        file.path("riesgos", "gpkg_departamentos", "zonas_riesgos_no_mitigable"),
        "aspect_socio_culturales"
    )

    for (sd in subdirs_comunes) {
        direct_path <- file.path(base_dir, sd, file_name)
        if (file.exists(direct_path)) return(normalizePath(direct_path))
    }

    found <- list.files(base_dir, pattern = paste0("^", file_name, "$"), recursive = TRUE, full.names = TRUE)
    if (length(found) > 0) return(normalizePath(found[1]))

    return(NULL)
}

#' Clear package cache
#'
#' Deletes all downloaded files in the rsdot cache directory.
#'
#' @return Invisible TRUE if deleted successfully.
#' @examples
#' \dontrun{
#' clear_rsdot_cache()
#' }
#' @export
clear_rsdot_cache <- function() {
    cache_dir <- get_cache_dir()
    if (dir.exists(cache_dir)) {
        unlink(cache_dir, recursive = TRUE)
        message("rsdot cache deleted: ", cache_dir)
    }
    return(invisible(TRUE))
}

# Secure download with retries and error handling
#
# @param url URL of the file to download.
# @param destfile Local destination path.
# @param show_progress Logical. Shows progress bar/messages.
# @param timeout Integer. Maximum time in seconds (default 300 = 5 min).
# @return Invisible TRUE if successful, FALSE if failed.
# @importFrom utils download.file
download_file_safe <- function(url, destfile, show_progress = TRUE, timeout = 300) {
    timeout_original <- getOption("timeout")
    on.exit(options(timeout = timeout_original))
    options(timeout = timeout)

    tryCatch(
        {
            utils::download.file(
                url = url,
                destfile = destfile,
                mode = "wb",
                quiet = !show_progress
            )
            TRUE
        },
        error = function(e) {
            warning(
                "Error downloading from: ", url, "\n",
                "Details: ", e$message, "\n",
                "The resource may be temporarily unavailable.",
                call. = FALSE,
                immediate. = TRUE
            )
            FALSE
        }
    )
}

# Secure reading of SF files with optional encoding correction
#
# @param path Path to the file (GPKG, SHP, etc).
# @param fix_encoding Logical. If TRUE, attempts to fix mojibake in character columns.
# @param valid_geom Logical. If TRUE, runs st_make_valid.
# @param quiet Logical. Silence sf output.
# @return sf object, or NULL in case of grave error.
# @importFrom sf read_sf st_make_valid st_sf st_sfc
read_sf_safe <- function(path, fix_encoding = TRUE, valid_geom = FALSE, quiet = TRUE) {
    if (!file.exists(path)) {
        warning("File does not exist: ", path, call. = FALSE)
        return(NULL)
    }

    tryCatch(
        {
            datos <- sf::read_sf(path, quiet = quiet)

            # Normalize column names to lowercase
            colnames(datos) <- tolower(colnames(datos))

            if (fix_encoding) {
                # Identify character columns
                cols_char <- names(datos)[sapply(datos, is.character)]

                # Exclude geometry columns if for some reason they are character
                cols_char <- setdiff(cols_char, c("geom", "geometry"))

                for (col in cols_char) {
                    datos[[col]] <- fix_encoding_text(datos[[col]])
                }

                # Also correct factors if any
                cols_fact <- names(datos)[sapply(datos, is.factor)]
                for (col in cols_fact) {
                    levels(datos[[col]]) <- fix_encoding_text(levels(datos[[col]]))
                }
            }

            if (valid_geom) {
                datos <- sf::st_make_valid(datos)
            }

            datos
        },
        error = function(e) {
            warning("Error reading SF file: ", path, "\n", e$message, call. = FALSE)
            NULL
        }
    )
}

# Corrects text with encoding problems
#
# Handles:
# 1. Conversion of Latin1 to UTF-8
# 2. Double-encoded UTF-8 mojibake
# 3. Typical Latin characters (tildes, Ñ)
#
# @param x Character vector.
# @return Corrected vector.
fix_encoding_text <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }

    # Detect which strings are NOT valid UTF-8
    is_valid <- tryCatch(validUTF8(x), error = \(e) rep(FALSE, length(x)))

    if (any(!is_valid)) {
        idx_bad <- which(!is_valid)
        # Attempt to repair only invalid ones assuming Latin1
        x[idx_bad] <- iconv(x[idx_bad], from = "latin1", to = "UTF-8", sub = "")
    }

    # Internal function to process a single string
    process_string <- function(s) {
        if (is.na(s)) {
            return(NA_character_)
        }

        # Common double UTF-8 pattern mappings
        s <- gsub("\u00c3\u0091", "\u00d1", s)
        s <- gsub("\u00c3\u0083\u00c2\u0091", "\u00d1", s)

        s <- gsub("\u00c3\u00b1", "\u00f1", s)
        s <- gsub("\u00c3\u0083\u00c2\u00b1", "\u00f1", s)

        # Uppercase vowels with tilde
        s <- gsub("\u00c3\u0081", "\u00c1", s)
        s <- gsub("\u00c3\u0089", "\u00c9", s)
        s <- gsub("\u00c3\u008d", "\u00cd", s)
        s <- gsub("\u00c3\u0093", "\u00d3", s)
        s <- gsub("\u00c3\u009a", "\u00da", s)

        # Lowercase vowels with tilde
        s <- gsub("\u00c3\u00a1", "\u00e1", s)
        s <- gsub("\u00c3\u00a9", "\u00e9", s)
        s <- gsub("\u00c3\u00ad", "\u00ed", s)
        s <- gsub("\u00c3\u00b3", "\u00f3", s)
        s <- gsub("\u00c3\u00ba", "\u00fa", s)

        return(s)
    }

    if (length(x) > 1) {
        return(sapply(x, process_string, USE.NAMES = FALSE))
    } else {
        return(process_string(x))
    }
}

# Creates an empty SF with CRS 4326
# @return Empty sf object
empty_sf <- function() {
    sf::st_sf(geometry = sf::st_sfc(crs = 4326))
}

# Generic function to combine a list of sf objects efficiently
#
# @param lista_sf List of sf objects.
# @param show_progress Logical. Show combining progress message.
# @return Combined sf object.
combine_sf_list <- function(lista_sf, show_progress = FALSE) {
    if (length(lista_sf) == 0) {
        return(empty_sf())
    }
    if (length(lista_sf) == 1) {
        return(lista_sf[[1]])
    }

    if (show_progress) {
        message("Combining data from multiple departments...")
    }

    tryCatch(
        {
            do.call(rbind, lista_sf)
        },
        error = function(e) {
            if (requireNamespace("dplyr", quietly = TRUE)) {
                dplyr::bind_rows(lista_sf)
            } else {
                stop("Error combining data from multiple departments: ", e$message, call. = FALSE)
            }
        }
    )
}

# Filter sf object by administrative attributes (departamento, provincia, distrito)
#
# @param datos sf object.
# @param departamento Character vector (optional).
# @param provincia Character vector (optional).
# @param distrito Character vector (optional).
# @param show_progress Logical.
# @param item_label Character label for progress message (e.g. "records").
# @return Filtered sf object.
filter_spatial_admin <- function(datos,
                                 departamento = NULL,
                                 provincia = NULL,
                                 distrito = NULL,
                                 show_progress = FALSE,
                                 item_label = "records") {
    if (nrow(datos) == 0) return(datos)

    find_col <- function(df, candidates) {
        cols <- names(df)
        matched <- cols[cols %in% candidates]
        if (length(matched) > 0) matched[1] else NULL
    }

    # 1. Filter by Department
    if (!is.null(departamento)) {
        col_dep <- find_col(datos, c("nombdep", "nom_dep", "nom_dpto", "departamento", "dep", "dpto"))
        if (!is.null(col_dep)) {
            dep_clean <- toupper(trimws(gsub("_", " ", departamento)))
            datos <- datos[toupper(trimws(datos[[col_dep]])) %in% dep_clean, ]
            if (nrow(datos) == 0) {
                warning("No data found for department(s): ", paste(departamento, collapse = ", "), call. = FALSE)
                return(empty_sf())
            }
        }
    }

    # 2. Filter by Province
    if (!is.null(provincia)) {
        col_prov <- find_col(datos, c("nombprov", "nom_prov", "nom_provincia", "provincia", "prov", "ubi_prov"))
        if (!is.null(col_prov)) {
            prov_clean <- toupper(trimws(gsub("_", " ", provincia)))
            datos <- datos[toupper(trimws(datos[[col_prov]])) %in% prov_clean, ]
            if (nrow(datos) == 0) {
                warning("No ", item_label, " found for province(s): ", paste(provincia, collapse = ", "), call. = FALSE)
                return(empty_sf())
            }
            if (show_progress) {
                message("\u2713 Filtered by province: ", nrow(datos), " ", item_label)
            }
        } else {
            warning("Column for province filtering not found in dataset", call. = FALSE)
        }
    }

    # 3. Filter by District
    if (!is.null(distrito)) {
        col_dist <- find_col(datos, c("nombdist", "nom_dist", "nom_distrito", "distrito", "dist", "ubi_dist"))
        if (!is.null(col_dist)) {
            dist_clean <- toupper(trimws(gsub("_", " ", distrito)))
            datos <- datos[toupper(trimws(datos[[col_dist]])) %in% dist_clean, ]
            if (nrow(datos) == 0) {
                warning("No ", item_label, " found for district(s): ", paste(distrito, collapse = ", "), call. = FALSE)
                return(empty_sf())
            }
            if (show_progress) {
                message("\u2713 Filtered by district: ", nrow(datos), " ", item_label)
            }
        } else {
            warning("Column for district filtering not found in dataset", call. = FALSE)
        }
    }

    return(datos)
}

# Universal Engine to load departmental spatial datasets
#
# @param dataset_key Dataset key in get_rsdot_catalog.
# @param departamento Character vector or NULL.
# @param provincia Character vector or NULL.
# @param distrito Character vector or NULL.
# @param show_progress Logical.
# @param force_update Logical.
# @param subfolder Subdirectory in cache (default dataset_key).
# @param item_label Descriptive label for records.
# @param usage_fn Custom function call name for help message.
# @param lang Language preference ("es" or "en").
# @return Combined and filtered sf object.
load_departmental_spatial_data <- function(dataset_key,
                                           departamento = NULL,
                                           provincia = NULL,
                                           distrito = NULL,
                                           show_progress = TRUE,
                                           force_update = FALSE,
                                           subfolder = dataset_key,
                                           item_label = "records",
                                           usage_fn = paste0("get_", dataset_key),
                                           lang = "en") {
    # 1. Parameter validations
    if (!is.logical(show_progress) || length(show_progress) != 1) {
        stop("The 'show_progress' parameter must be TRUE or FALSE / El par\u00e1metro 'show_progress' debe ser TRUE o FALSE", call. = FALSE)
    }

    if (!is.logical(force_update) || length(force_update) != 1) {
        stop("The 'force_update' parameter must be TRUE or FALSE / El par\u00e1metro 'force_update' debe ser TRUE o FALSE", call. = FALSE)
    }

    if (!is.null(provincia) && !is.character(provincia)) {
        stop("The 'provincia' parameter must be a character vector or NULL / El par\u00e1metro 'provincia' debe ser un vector de caracteres o NULL", call. = FALSE)
    }

    if (!is.null(distrito) && !is.character(distrito)) {
        stop("The 'distrito' parameter must be a character vector or NULL / El par\u00e1metro 'distrito' debe ser un vector de caracteres o NULL", call. = FALSE)
    }

    # 2. Reference catalog
    tabla_departamentos <- get_rsdot_catalog(dataset_key)

    # 3. Department listing if NULL
    if (is.null(departamento)) {
        if (lang == "es") {
            message("Departamentos disponibles:")
            message(paste0("  - ", tabla_departamentos$departamento, collapse = "\n"))
            message("\nUso: ", usage_fn, "(departamento = 'CUSCO')")
        } else {
            message("Available departments:")
            message(paste0("  - ", tabla_departamentos$departamento, collapse = "\n"))
            message("\nUsage: ", usage_fn, "(departamento = 'CUSCO')")
        }
        return(invisible(tabla_departamentos$departamento))
    }

    if (!is.character(departamento)) {
        stop("The 'departamento' parameter must be a character vector / El par\u00e1metro 'departamento' debe ser un vector de caracteres", call. = FALSE)
    }

    # 4. Normalize and validate department names
    departamento_input <- toupper(trimws(departamento))
    departamento_input <- gsub("_", " ", departamento_input)

    departamentos_validos <- departamento_input %in% tabla_departamentos$departamento

    if (!all(departamentos_validos)) {
        departamentos_invalidos <- departamento_input[!departamentos_validos]
        stop(
            "Invalid department(s) / Departamento(s) no v\u00e1lido(s): ",
            paste(departamentos_invalidos, collapse = ", "), "\n",
            "Use ", usage_fn, "() to see the full list.",
            call. = FALSE
        )
    }

    # 5. Filter catalog table
    tabla_filtrada <- tabla_departamentos[tabla_departamentos$departamento %in% departamento_input, ]

    # 6. Cache Directory
    ruta_cache_dir <- file.path(get_cache_dir(), subfolder)
    if (!dir.exists(ruta_cache_dir)) {
        dir.create(ruta_cache_dir, recursive = TRUE)
    }

    # 7. Download and read loop
    lista_datos <- list()

    for (i in seq_len(nrow(tabla_filtrada))) {
        dep <- tabla_filtrada$departamento[i]
        file_name <- tabla_filtrada$file_name[i]
        id_osf <- tabla_filtrada$id_osf[i]

        # Check staging
        if (grepl("^(TODO_|STAGING_)", id_osf)) {
            if (is_staging_enabled()) {
                staging_path <- find_staging_file(file_name)
                if (!is.null(staging_path) && file.exists(staging_path)) {
                    if (show_progress) {
                        message("\u2139 Staging local: Leyendo ", file_name, "...")
                    }
                    datos_dep <- read_sf_safe(staging_path, fix_encoding = TRUE)
                    if (!is.null(datos_dep)) {
                        lista_datos[[dep]] <- datos_dep
                        if (show_progress) {
                            message("\u2713 Loaded: ", dep, " (", nrow(datos_dep), " ", item_label, ")")
                        }
                        next
                    }
                }
            }
            stop(
                "Los datos de ", dataset_key, " para '", dep,
                "' se encuentran en preparaci\u00f3n en sdot_data antes de su publicaci\u00f3n oficial en OSF.\n",
                "Para cargarlos desde sdot_data local active: options(rsdot.use_staging = TRUE)",
                call. = FALSE
            )
        }

        url_descarga <- paste0("https://osf.io/download/", id_osf, "/")
        archivo_local <- file.path(ruta_cache_dir, file_name)

        if (!file.exists(archivo_local) || force_update) {
            if (show_progress) message("Downloading: ", dep, "...")

            descarga_exitosa <- download_file_safe(
                url = url_descarga,
                destfile = archivo_local,
                show_progress = show_progress
            )

            if (descarga_exitosa && show_progress) {
                message("\u2713 Download completed: ", dep)
            } else if (!descarga_exitosa) {
                next
            }
        } else {
            if (show_progress) message("\u2713 Using cache: ", dep)
        }

        if (file.exists(archivo_local)) {
            if (show_progress) message("Loading data: ", dep, "...")

            datos_dep <- read_sf_safe(archivo_local, fix_encoding = TRUE)

            if (!is.null(datos_dep)) {
                lista_datos[[dep]] <- datos_dep
                if (show_progress) {
                    message("\u2713 Loaded: ", dep, " (", nrow(datos_dep), " ", item_label, ")")
                }
            }
        }
    }

    if (length(lista_datos) == 0) {
        stop("Could not load any department data", call. = FALSE)
    }

    # 8. Combine data
    datos_final <- combine_sf_list(lista_datos, show_progress = show_progress)

    # 9. Filter by province and district
    datos_final <- filter_spatial_admin(
        datos = datos_final,
        provincia = provincia,
        distrito = distrito,
        show_progress = show_progress,
        item_label = item_label
    )

    return(datos_final)
}

# Generic function to download and read spatial data (Single/National File)
#
# @param osf_id OSF file ID.
# @param file_name File name (e.g. "file.zip" or "file.gpkg").
# @param show_progress Logical. Show progress messages.
# @param force_update Logical. Force download.
# @param pattern_shp Regex pattern to search for shapefile inside zip (optional).
# @return sf object.
# @importFrom utils unzip
download_and_read_spatial_data <- function(osf_id, file_name, show_progress = TRUE, force_update = FALSE, pattern_shp = NULL) {
    if (is_staging_enabled()) {
        staging_path <- find_staging_file(file_name)
        if (!is.null(staging_path) && file.exists(staging_path)) {
            if (show_progress) {
                message("\u2139 Staging local: Leyendo ", file_name, "...")
            }
            datos <- read_sf_safe(staging_path, fix_encoding = TRUE, quiet = !show_progress)
            if (!is.null(datos)) {
                if (show_progress) message("\u2713 Datos cargados exitosamente desde staging.")
                return(datos)
            }
        }
    }

    if (grepl("^(TODO_|STAGING_)", osf_id)) {
        stop(
            "Los datos para '", file_name,
            "' se encuentran en preparaci\u00f3n en sdot_data antes de su publicaci\u00f3n oficial en OSF.\n",
            "Para cargarlos desde sdot_data local active: options(rsdot.use_staging = TRUE)",
            call. = FALSE
        )
    }

    cache_dir <- get_cache_dir()
    archivo_local <- file.path(cache_dir, file_name)
    url_descarga <- paste0("https://osf.io/download/", osf_id, "/")

    # 1. Download
    if (!file.exists(archivo_local) || force_update) {
        if (show_progress) {
            message("Downloading file from OSF...")
            message("URL: ", url_descarga)
        }

        exito <- download_file_safe(url_descarga, archivo_local, show_progress = show_progress)

        if (!exito) {
            stop("Could not download file from OSF.", call. = FALSE)
        }

        if (show_progress) message("\u2713 Download completed.")
    } else {
        if (show_progress) message("\u2713 Using cached file: ", file_name)
    }

    # 2. Decompression (if ZIP)
    if (grepl("\\.zip$", file_name, ignore.case = TRUE)) {
        if (show_progress) message("Decompressing files...")

        suppressWarnings({
            utils::unzip(archivo_local, exdir = cache_dir, overwrite = force_update)
        })

        if (!is.null(pattern_shp)) {
            archivos_encontrados <- list.files(
                path = cache_dir,
                pattern = pattern_shp,
                full.names = TRUE,
                recursive = TRUE,
                ignore.case = TRUE
            )

            if (length(archivos_encontrados) == 0) {
                stop("Expected SHP file not found inside ZIP.", call. = FALSE)
            }
            archivo_a_leer <- archivos_encontrados[1]
        } else {
            stop("'pattern_shp' is required for ZIP files.", call. = FALSE)
        }
    } else {
        archivo_a_leer <- archivo_local
    }

    # 3. Reading
    if (show_progress) message("Reading spatial data...")
    datos <- read_sf_safe(archivo_a_leer, fix_encoding = TRUE, quiet = TRUE)

    if (is.null(datos)) {
        stop("Error reading spatial data.", call. = FALSE)
    }

    if (show_progress) message("\u2713 Data read successfully.")
    return(datos)
}
