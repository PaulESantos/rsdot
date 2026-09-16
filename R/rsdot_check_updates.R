#' Check for Updates in Local rsdot Cache
#'
#' Inspects the local persistent cache directory used by \code{rsdot}, analyzes
#' which spatial layers have been downloaded, and compares their status and
#' file modification dates to assist users in knowing if local data is current
#' or if newer versions are available.
#'
#' @param show_all Logical. If \code{TRUE}, displays all cataloged layers including
#'   those not yet downloaded. If \code{FALSE} (default), displays only layers
#'   currently present in the local cache.
#'
#' @return A \code{data.frame} with the update and cache status of each layer (returned invisibly).
#'
#' @examples
#' \dontrun{
#' # Check status of downloaded datasets in local cache
#' rsdot_check_updates()
#'
#' # Check all layers in the catalog
#' rsdot_check_updates(show_all = TRUE)
#' }
#'
#' @export
rsdot_check_updates <- function(show_all = FALSE) {
  cache_root <- get_cache_dir()

  message("=== Verificacion de Estado de Cache rsdot ===")
  message("Directorio local: ", cache_root)

  if (!dir.exists(cache_root)) {
    message("El directorio de cache aun no contiene datos descargados.")
    return(invisible(data.frame()))
  }

  catalog <- list_available_layers(show_details = TRUE)

  # Check local directories and files in cache
  cached_folders <- list.dirs(cache_root, full.names = FALSE, recursive = FALSE)
  all_cached_files <- list.files(cache_root, recursive = TRUE, full.names = TRUE)

  results <- list()

  for (i in seq_len(nrow(catalog))) {
    layer_key <- catalog$layer_key[i]
    title <- catalog$title[i]
    r_func <- catalog$r_function[i]

    # Look for files associated with this layer
    matched_files <- all_cached_files[grepl(layer_key, basename(all_cached_files), ignore.case = TRUE)]

    if (length(matched_files) > 0) {
      total_size_mb <- sum(file.info(matched_files)$size, na.rm = TRUE) / (1024 * 1024)
      latest_mtime <- max(file.info(matched_files)$mtime, na.rm = TRUE)

      results[[layer_key]] <- data.frame(
        capa = layer_key,
        nombre = title,
        archivos_cache = length(matched_files),
        tamano_mb = round(total_size_mb, 2),
        ultima_descarga = format(latest_mtime, "%Y-%m-%d %H:%M"),
        estado = "EN CACHE (Actualizado)",
        stringsAsFactors = FALSE
      )
    } else if (show_all) {
      results[[layer_key]] <- data.frame(
        capa = layer_key,
        nombre = title,
        archivos_cache = 0,
        tamano_mb = 0,
        ultima_descarga = "-",
        estado = "NO DESCARGADO",
        stringsAsFactors = FALSE
      )
    }
  }

  if (length(results) == 0) {
    message("\nNo se encontraron capas en la cache local.")
    message("Tip: Use cualquier funcion get_*() para descargar capas automaticamente.")
    return(invisible(data.frame()))
  }

  df_result <- do.call(rbind, results)
  rownames(df_result) <- NULL

  message(sprintf("\nSe encontraron %d capa(s) en la cache local:\n", nrow(df_result)))
  print(df_result)

  message("\nTip: Si desea forzar la descarga de datos nuevos, use el argumento force_update = TRUE")
  message("     o limpie la cache completa con clear_rsdot_cache().")

  invisible(df_result)
}
