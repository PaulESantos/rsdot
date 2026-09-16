test_that("fix_encoding_text works correctly", {
    # Caso 1: Ñ mal codificada (C3 83 C2 91)
    bad_nye <- "\u00c3\u0083\u00c2\u0091" # "Ã‘"
    expect_equal(fix_encoding_text(bad_nye), "Ñ")

    # Caso 2: ñ mal codificada (C3 83 C2 B1)
    bad_nye_lower <- "\u00c3\u0083\u00c2\u00b1" # "Ã±"
    expect_equal(fix_encoding_text(bad_nye_lower), "ñ")

    # Caso 3: Á mal codificada
    bad_a <- "\u00c3\u0081"
    expect_equal(fix_encoding_text(bad_a), "Á")

    # Caso 4: Texto normal sin cambios
    normal <- "Hola Mundo"
    expect_equal(fix_encoding_text(normal), "Hola Mundo")

    # Caso 5: Vector
    vec <- c(bad_nye, normal)
    expect_equal(fix_encoding_text(vec), c("Ñ", "Hola Mundo"))
})

test_that("get_rsdot_catalog returns correct structure", {
    cat_aero <- get_rsdot_catalog("aerodromos")
    expect_s3_class(cat_aero, "data.frame")
    expect_contains(names(cat_aero), c("departamento", "file_name", "id_osf"))
    expect_contains(cat_aero$departamento, "CUSCO")

    cat_dens <- get_rsdot_catalog("densidad_poblacional")
    expect_s3_class(cat_dens, "data.frame")
    expect_contains(cat_dens$departamento, "LIMA")
})

test_that("download_file_safe handles errors", {
    # Mock download.file to fail
    mockery::stub(download_file_safe, "utils::download.file", function(...) stop("Simulated error"))

    # Should return FALSE instead of stopping and emit a snapshot warning
    expect_snapshot(
        res <- download_file_safe("http://example.com/fail", tempfile(), show_progress = FALSE)
    )
    expect_false(res)
})

test_that("get_cache_dir returns valid path", {
    cache_dir <- get_cache_dir()
    expect_true(dir.exists(cache_dir))
    expect_true(grepl("rsdot", cache_dir))
})
