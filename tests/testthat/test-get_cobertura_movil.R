test_that("get_cobertura_movil_c validates input", {
    expect_error(get_cobertura_movil_c(departamento = 123), "vector de caracteres")
    expect_error(get_cobertura_movil_c(departamento = "INVALIDO"), "Departamento\\(s\\) no v\u00e1lido\\(s\\)")
})

test_that("get_cobertura_movil_c suggests departments when null", {
    msg <- capture_messages(get_cobertura_movil_c(departamento = NULL))
    expect_match(msg, "Departamentos disponibles", all = FALSE)
})

test_that("get_cobertura_movil alias points to get_cobertura_movil_c", {
    expect_identical(get_cobertura_movil, get_cobertura_movil_c)
})
