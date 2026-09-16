test_that("rsdot_check_updates returns structured report with show_all = TRUE", {
  res <- rsdot_check_updates(show_all = TRUE)
  expect_s3_class(res, "data.frame")
  expected_cols <- c("capa", "nombre", "archivos_cache", "tamano_mb", "ultima_descarga", "estado")
  expect_true(all(expected_cols %in% names(res)))
  expect_type(res$tamano_mb, "double")
  expect_gt(nrow(res), 0)
})

test_that("rsdot_check_updates executes without error", {
  expect_no_error({
    res <- rsdot_check_updates(show_all = FALSE)
  })
})
