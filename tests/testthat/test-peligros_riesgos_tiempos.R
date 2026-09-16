test_that("get_peligros validates input and staging status", {
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

  # Staging error message (graceful explanation before OSF upload)
  expect_error(
    get_peligros(tipo = "puntos_criticos", departamento = "CUSCO", show_progress = FALSE),
    "se encuentran en preparaci\u00f3n"
  )
})

test_that("get_riesgos validates input and staging status", {
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

  # Staging error message
  expect_error(
    get_riesgos(tipo = "evaluacion_riesgos", departamento = "CUSCO", show_progress = FALSE),
    "se encuentran en preparaci\u00f3n"
  )
})

test_that("get_tiempos_desplazamiento validates input and staging status", {
  # Department listing
  deps <- get_tiempos_desplazamiento(departamento = NULL)
  expect_type(deps, "character")
  expect_contains(deps, "CUSCO")

  # Invalid department
  expect_error(
    get_tiempos_desplazamiento(departamento = "INEXISTENTE"),
    "Invalid department"
  )

  # Staging error message
  expect_error(
    get_tiempos_desplazamiento(departamento = "CUSCO", show_progress = FALSE),
    "se encuentran en preparaci\u00f3n"
  )
})
