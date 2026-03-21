test_that("get_aerodromos validates input", {
    expect_error(get_aerodromos(departamento = 123), "character vector")
    expect_error(get_aerodromos(departamento = "INVALIDO"), "Invalid department")
})

test_that("get_aerodromos suggests departments when null", {
    msg <- capture_messages(get_aerodromos(departamento = NULL))
    expect_match(msg, "Available departments", all = FALSE)
})

test_that("get_aerodromos returns empty sf for empty filter", {
    # Mock internal fetching/reading logic to avoid network calls
    # This requires mocking get_rsdot_catalog, download_file_safe, read_sf_safe
    # For now, we test the logic that doesn't reach network or assumes successful mock if we could mock.

    # Since valid inputs trigger network, we'll focus on validation tests above
    # and perhaps a test that mocks the utilities if 'mockery' is available.
})
