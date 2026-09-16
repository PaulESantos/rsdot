test_that("list_available_layers returns a data.frame with expected columns", {
  res <- list_available_layers()
  expect_s3_class(res, "data.frame")
  expected_cols <- c("Capa", "Descripcion", "Dominio", "Funcion_R", "Particion")
  expect_true(all(expected_cols %in% names(res)))
  expect_gt(nrow(res), 0)

  # With show_details = TRUE
  res_det <- list_available_layers(show_details = TRUE)
  expect_s3_class(res_det, "data.frame")
  expected_cols_det <- c("layer_key", "title", "domain", "domain_label", "r_function", "partition", "source_layer")
  expect_true(all(expected_cols_det %in% names(res_det)))
})

test_that("list_available_layers filters by valid domains", {
  dominios <- c("limits", "transport", "services", "demography", "telecom", "hazards")
  for (d in dominios) {
    res_dom <- list_available_layers(domain = d, show_details = TRUE)
    expect_s3_class(res_dom, "data.frame")
    expect_gt(nrow(res_dom), 0)
    expect_true(all(res_dom$domain == d))
  }
})

test_that("list_available_layers throws error on invalid domain", {
  expect_error(
    list_available_layers(domain = "no_existe"),
    "Dominio no valido"
  )
})
