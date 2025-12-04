test_that("get_densidad_poblacional() lista departamentos cuando departamento = NULL", {
  # No debería requerir conexión a internet, solo usa la tabla interna
  res <- get_densidad_poblacional(departamento = NULL, show_progress = FALSE)

  expect_type(res, "character")
  expect_true(length(res) >= 1)
  expect_true("CUSCO" %in% res)
})

test_that("get_densidad_poblacional() valida departamentos y arroja error con nombres inválidos", {
  expect_error(
    get_densidad_poblacional(departamento = "NARNIA", show_progress = FALSE),
    regexp = "Departamento\\(s\\) no v\u00e1lido\\(s\\)"
  )
})

test_that("get_densidad_poblacional() valida que show_progress y force_update sean lógicos", {
  expect_error(
    get_densidad_poblacional(departamento = "CUSCO", show_progress = "si"),
    regexp = "show_progress"
  )

  expect_error(
    get_densidad_poblacional(departamento = "CUSCO", force_update = "no"),
    regexp = "force_update"
  )
})

test_that("get_densidad_poblacional() valida que provincia y distrito sean caracteres", {
  expect_error(
    get_densidad_poblacional(
      departamento = "CUSCO",
      provincia    = 1L,
      show_progress = FALSE
    ),
    regexp = "provincia.*vector de caracteres"
  )

  expect_error(
    get_densidad_poblacional(
      departamento = "CUSCO",
      distrito     = 1L,
      show_progress = FALSE
    ),
    regexp = "distrito.*vector de caracteres"
  )
})
