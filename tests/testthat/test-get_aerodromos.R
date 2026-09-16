test_that("get_aerodromos validates input", {
    expect_error(get_aerodromos(departamento = 123), "character vector")
    expect_error(get_aerodromos(departamento = "INVALIDO"), "Invalid department")
})

test_that("get_aerodromos suggests departments when null", {
    msg <- capture_messages(get_aerodromos(departamento = NULL))
    expect_match(msg, "Available departments", all = FALSE)
})

test_that("get_aerodromos validates filter parameters", {
    expect_error(
        get_aerodromos(departamento = "CUSCO", provincia = 123),
        "provincia.*character vector"
    )
    expect_error(
        get_aerodromos(departamento = "CUSCO", distrito = 123),
        "distrito.*character vector"
    )
})

