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
    # Save original timeout option
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

# Generic function to download and read spatial data
#
# @param osf_id OSF file ID.
# @param file_name File name (e.g. "file.zip" or "file.gpkg").
# @param show_progress Logical. Show progress messages.
# @param force_update Logical. Force download.
# @param pattern_shp Regex pattern to search for shapefile inside zip (optional).
# @return sf object.
# @importFrom utils unzip
download_and_read_spatial_data <- function(osf_id, file_name, show_progress = TRUE, force_update = FALSE, pattern_shp = NULL) {
    cache_dir <- get_cache_dir()
    archivo_local <- file.path(cache_dir, file_name)
    url_descarga <- paste0("https://osf.io/", osf_id, "/download")

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

        # Look for the file to read (shp)
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
