# Tests for refactored utility functions
test_that("get_cache_dir returns a valid path", {
    cache_dir <- get_cache_dir()
    expect_match(cache_dir, "rsdot")
    # Verify it exists or was created
    expect_no_error(dir.exists(cache_dir))
    expect_true(dir.exists(cache_dir)) # Still needed for logic but match is better for content
})

test_that("get_rsdot_catalog returns correct metadata for new datasets", {
    deps <- get_rsdot_catalog("departamentos")
    expect_equal(deps$file_name, "v_departamentos_2023.zip")
    expect_equal(deps$id_osf, "grhe3")

    provs <- get_rsdot_catalog("provincias")
    expect_equal(provs$file_name, "v_provincias_2023.zip")

    dists <- get_rsdot_catalog("distritos")
    expect_equal(dists$file_name, "v_distritos_2023.zip")
})

test_that("download_and_read_spatial_data works with mocks", {
    # Mock download_file_safe to return TRUE
    mockery::stub(download_and_read_spatial_data, "download_file_safe", TRUE)

    # Mock unzip to do nothing (or create a dummy file if needed)
    mockery::stub(download_and_read_spatial_data, "utils::unzip", function(...) {
        # Create a dummy shapefile in cache dir to satisfy list.files
        args <- list(...)
        exdir <- args$exdir
        if (is.null(exdir)) exdir <- tempdir() # Fallback

        # We need to simulate the extraction of a shapefile
        # We can't easily fake file creation inside a stub if we don't know the exact path
        # But we can mock list.files to return a dummy path
        NULL
    })

    # Mock list.files to return a dummy path
    mockery::stub(download_and_read_spatial_data, "list.files", "dummy_path.shp")

    # Mock read_sf_safe to return a dummy sf object
    dummy_sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326), nombdep = "LIMA")
    mockery::stub(download_and_read_spatial_data, "read_sf_safe", dummy_sf)

    # Run the function
    result <- download_and_read_spatial_data(
        osf_id = "test_id",
        file_name = "test.zip",
        show_progress = FALSE,
        pattern_shp = ".*\\.shp$"
    )

    expect_s3_class(result, "sf")
    expect_equal(result$nombdep, "LIMA")
})

test_that("fix_encoding_text handles Latin-1 inputs", {
    # Simular input Latin-1: "CAÑETE" en Latin-1 es "CA\xd1ETE"
    # En R, esto se puede simular creando un raw y convirtiéndolo a char con encoding latin1
    raw_bytes <- as.raw(c(0x43, 0x41, 0xd1, 0x45, 0x54, 0x45)) # CAÑETE
    latin1_str <- rawToChar(raw_bytes)
    Encoding(latin1_str) <- "latin1"

    # El problema original era que al pasar esto a gsub (que espera UTF8 u otro compat), fallaba
    # La nueva implementación debe convertirlo a UTF8 primero
    fixed <- fix_encoding_text(latin1_str)

    expect_equal(fixed, "CAÑETE")
    expect_equal(Encoding(fixed), "UTF-8")
})
