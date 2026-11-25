test_that("get_distritos supports ano 2025 and get_distritos_2025 alias", {
  cat_2025 <- rsdot:::get_rsdot_catalog("distritos_2025")
  expect_equal(cat_2025$file_name, "v_distritos_2025.gpkg")
  expect_true(nzchar(cat_2025$id_osf))

  old_opt <- getOption("rsdot.use_staging")
  on.exit(options(rsdot.use_staging = old_opt), add = TRUE)
  options(rsdot.use_staging = TRUE)

  d2025 <- get_distritos_2025(departamento = "CUSCO", show_progress = FALSE)
  expect_s3_class(d2025, "sf")
  expect_gt(nrow(d2025), 0)
})

test_that("get_densidad_actividades validates input and catalog", {
  tipos <- get_densidad_actividades(tipo = NULL)
  expect_type(tipos, "character")
  expect_contains(tipos, c("comercio", "mercados", "financiero", "inmobiliario", "viviendas"))

  expect_error(
    get_densidad_actividades(tipo = "invalido"),
    "Invalid density type"
  )

  cat_com <- rsdot:::get_rsdot_catalog("densidad_comercio")
  expect_equal(cat_com$file_name, "densidad_comercio.gpkg")
  expect_true(nzchar(cat_com$id_osf))
  expect_false(grepl("^STAGING_", cat_com$id_osf))

  old_opt <- getOption("rsdot.use_staging")
  on.exit(options(rsdot.use_staging = old_opt), add = TRUE)
  options(rsdot.use_staging = TRUE)

  d_com <- get_densidad_comercio(show_progress = FALSE)
  expect_s3_class(d_com, "sf")
  expect_gt(nrow(d_com), 0)
})

test_that("get_vab_ccpp validates input and catalog", {
  expect_error(
    get_vab_ccpp(cobertura = "invalido"),
    "Invalid 'cobertura'"
  )

  cat_vab <- rsdot:::get_rsdot_catalog("vab_ccpp_total")
  expect_equal(cat_vab$file_name, "ccpp150_vab_total.gpkg")
  expect_true(nzchar(cat_vab$id_osf))
  expect_false(grepl("^STAGING_", cat_vab$id_osf))

  old_opt <- getOption("rsdot.use_staging")
  on.exit(options(rsdot.use_staging = old_opt), add = TRUE)
  options(rsdot.use_staging = TRUE)

  vab_callao <- get_vab_ccpp(cobertura = "callao", show_progress = FALSE)
  expect_s3_class(vab_callao, "sf")
  expect_gt(nrow(vab_callao), 0)
})

test_that("get_flujos_movilidad validates input and catalog", {
  tipos <- get_flujos_movilidad(tipo = NULL)
  expect_type(tipos, "character")
  expect_contains(tipos, c("estudiantes", "trabajadores"))

  expect_error(
    get_flujos_movilidad(tipo = "invalido"),
    "Invalid flow type"
  )

  expect_error(
    get_flujos_movilidad(tipo = "estudiantes", volumen = "invalido"),
    "Invalid volume bracket"
  )

  expect_error(
    get_flujos_movilidad(tipo = "trabajadores", volumen = "invalido"),
    "Invalid volume bracket"
  )

  cat_flujo <- rsdot:::get_rsdot_catalog("flujos_est_100_1000")
  expect_equal(cat_flujo$file_name, "flujo_est_100_1000.gpkg")
  expect_true(nzchar(cat_flujo$id_osf))
  expect_false(grepl("^STAGING_", cat_flujo$id_osf))

  old_opt <- getOption("rsdot.use_staging")
  on.exit(options(rsdot.use_staging = old_opt), add = TRUE)
  options(rsdot.use_staging = TRUE)

  fl_est <- get_flujos_estudiantes(volumen = "100_1000", show_progress = FALSE)
  expect_s3_class(fl_est, "sf")
  expect_gt(nrow(fl_est), 0)
})

test_that("get_catastro_rural validates staging", {
  expect_error(
    get_catastro_rural(show_progress = FALSE),
    "se encuentran en preparaci\u00f3n"
  )
})

test_that("list_available_layers includes new domains", {
  res_mob <- list_available_layers(domain = "mobility", show_details = TRUE)
  expect_s3_class(res_mob, "data.frame")
  expect_gte(nrow(res_mob), 2)

  res_cad <- list_available_layers(domain = "cadastre", show_details = TRUE)
  expect_s3_class(res_cad, "data.frame")
  expect_gte(nrow(res_cad), 1)

  res_all <- list_available_layers()
  expect_gte(nrow(res_all), 30)
})
