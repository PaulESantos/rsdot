test_that("get_peligros validates input and catalog", {
  # Listing types
  tipos <- get_peligros(tipo = NULL)
  expect_type(tipos, "character")
  expect_gt(length(tipos), 0)

  # Invalid type
  expect_error(get_peligros(tipo = "tipo_invalido"), "Invalid hazard type")

  # Department listing
  deps <- get_peligros(tipo = "puntos_criticos", departamento = NULL)
  expect_type(deps, "character")
  expect_contains(deps, "CUSCO")

  # Catalog resolution and alias resolution
  cat_puntos <- rsdot:::get_rsdot_catalog("peligros_puntos_criticos_inundaciones")
  expect_true(all(nzchar(cat_puntos$id_osf)))
  expect_false(any(grepl("^STAGING_", cat_puntos$id_osf)))

  cat_alias <- rsdot:::get_rsdot_catalog("puntos_criticos")
  expect_equal(cat_puntos$file_name, cat_alias$file_name)

  old_opt <- getOption("rsdot.use_staging")
  on.exit(options(rsdot.use_staging = old_opt), add = TRUE)
  options(rsdot.use_staging = TRUE)

  pel_cusco <- get_peligros(tipo = "puntos_criticos", departamento = "CUSCO", show_progress = FALSE)
  expect_s3_class(pel_cusco, "sf")

  # Shortcut functions
  pel_pc <- get_puntos_criticos(departamento = "CUSCO", show_progress = FALSE)
  expect_s3_class(pel_pc, "sf")
})

test_that("get_riesgos validates input and catalog", {
  # Listing types
  tipos <- get_riesgos(tipo = NULL)
  expect_type(tipos, "character")
  expect_gt(length(tipos), 0)

  # Invalid type
  expect_error(get_riesgos(tipo = "tipo_invalido"), "Invalid risk type")

  # Department listing
  deps <- get_riesgos(tipo = "evaluacion_riesgos", departamento = NULL)
  expect_type(deps, "character")
  expect_contains(deps, "CUSCO")

  # Catalog resolution and alias resolution
  cat_riesgos <- rsdot:::get_rsdot_catalog("riesgos_evaluacion_riesgos")
  expect_true(all(nzchar(cat_riesgos$id_osf)))
  expect_false(any(grepl("^STAGING_", cat_riesgos$id_osf)))

  cat_rie_alias <- rsdot:::get_rsdot_catalog("evaluacion_riesgos")
  expect_equal(cat_riesgos$file_name, cat_rie_alias$file_name)

  old_opt <- getOption("rsdot.use_staging")
  on.exit(options(rsdot.use_staging = old_opt), add = TRUE)
  options(rsdot.use_staging = TRUE)

  rie_cusco <- get_riesgos(tipo = "evaluacion_riesgos", departamento = "CUSCO", show_progress = FALSE)
  expect_s3_class(rie_cusco, "sf")

  # Shortcut function
  rie_eval <- get_evaluacion_riesgos(departamento = "CUSCO", show_progress = FALSE)
  expect_s3_class(rie_eval, "sf")
})

test_that("get_tiempos_desplazamiento validates input and catalog", {
  # Department listing
  deps <- get_tiempos_desplazamiento(departamento = NULL)
  expect_type(deps, "character")
  expect_contains(deps, "CUSCO")

  # Invalid department
  expect_error(
    get_tiempos_desplazamiento(departamento = "INEXISTENTE"),
    "Invalid department"
  )

  # Catalog resolution
  cat_td <- rsdot:::get_rsdot_catalog("tiempos_desplazamiento")
  expect_true(all(nzchar(cat_td$id_osf)))
  expect_false(any(grepl("^STAGING_", cat_td$id_osf)))

  old_opt <- getOption("rsdot.use_staging")
  on.exit(options(rsdot.use_staging = old_opt), add = TRUE)
  options(rsdot.use_staging = TRUE)

  td_callao <- get_tiempos_desplazamiento(departamento = "CALLAO", show_progress = FALSE)
  expect_s3_class(td_callao, "sf")
})
