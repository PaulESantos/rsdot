test_that("get_densidad_poblacional() lists departments when departamento = NULL", {
  # Should not require internet connection, uses internal table
  res <- get_densidad_poblacional(departamento = NULL, show_progress = FALSE)

  expect_type(res, "character")
  expect_gt(length(res), 0)
  expect_contains(res, "CUSCO")
})

test_that("get_densidad_poblacional() validates departments and errors with invalid names", {
  expect_error(
    get_densidad_poblacional(departamento = "NARNIA", show_progress = FALSE),
    regexp = "Invalid department"
  )
})

test_that("get_densidad_poblacional() validates that show_progress and force_update are logical", {
  expect_error(
    get_densidad_poblacional(departamento = "CUSCO", show_progress = "yes"),
    regexp = "show_progress"
  )

  expect_error(
    get_densidad_poblacional(departamento = "CUSCO", force_update = "no"),
    regexp = "force_update"
  )
})

test_that("get_densidad_poblacional() validates that provincia and distrito are characters", {
  expect_error(
    get_densidad_poblacional(
      departamento = "CUSCO",
      provincia    = 1L,
      show_progress = FALSE
    ),
    regexp = "provincia.*character vector"
  )

  expect_error(
    get_densidad_poblacional(
      departamento = "CUSCO",
      distrito     = 1L,
      show_progress = FALSE
    ),
    regexp = "distrito.*character vector"
  )
})
