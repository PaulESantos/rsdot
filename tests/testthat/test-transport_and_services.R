test_that("get_capitales validates input", {
  deps <- get_capitales(departamento = NULL)
  expect_type(deps, "character")
  expect_contains(deps, "CUSCO")

  expect_error(get_capitales(departamento = 123), "vector de caracteres")
  expect_error(get_capitales(departamento = "INVENTADO"), "no v\u00e1lido")
  expect_error(get_capitales(departamento = "CUSCO", show_progress = "yes"), "TRUE o FALSE")
  expect_error(get_capitales(departamento = "CUSCO", force_update = "no"), "TRUE o FALSE")
})

test_that("get_centros_poblados validates input", {
  deps <- get_centros_poblados(departamento = NULL)
  expect_type(deps, "character")
  expect_contains(deps, "CUSCO")

  expect_error(get_centros_poblados(departamento = 123), "vector de caracteres")
  expect_error(get_centros_poblados(departamento = "INVENTADO"), "no v\u00e1lido")
})

test_that("get_red_vial_* functions validate input", {
  # Red vial nacional
  deps_nac <- get_red_vial_nacional(departamento = NULL)
  expect_type(deps_nac, "character")
  expect_contains(deps_nac, "CUSCO")
  expect_error(get_red_vial_nacional(departamento = "INVENTADO"), "Invalid department")

  # Red vial departamental
  deps_dep <- get_red_vial_departamental(departamento = NULL)
  expect_type(deps_dep, "character")
  expect_contains(deps_dep, "CUSCO")
  expect_error(get_red_vial_departamental(departamento = "INVENTADO"), "Invalid department")

  # Red vial vecinal
  deps_vec <- get_red_vial_vecinal(departamento = NULL)
  expect_type(deps_vec, "character")
  expect_contains(deps_vec, "CUSCO")
  expect_error(get_red_vial_vecinal(departamento = "INVENTADO"), "Invalid department")
})

test_that("social services functions validate input", {
  # IPRESS
  deps_ipress <- get_ipress(departamento = NULL)
  expect_type(deps_ipress, "character")
  expect_contains(deps_ipress, "CUSCO")
  expect_error(get_ipress(departamento = "INVENTADO"), "Invalid department")

  # Locales educativos
  deps_loc <- get_locales_educativos(departamento = NULL)
  expect_type(deps_loc, "character")
  expect_contains(deps_loc, "CUSCO")
  expect_error(get_locales_educativos(departamento = "INVENTADO"), "Invalid department")

  # Servicios educativos
  deps_ser <- get_servicios_educativos(departamento = NULL)
  expect_type(deps_ser, "character")
  expect_contains(deps_ser, "CUSCO")
  expect_error(get_servicios_educativos(departamento = "INVENTADO"), "Invalid department")
})
