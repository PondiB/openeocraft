test_that("load_collections stores collection config on the api and returns it", {
    api <- mock_create_openeo_v1()

    result <- withVisible(load_collections(
        api,
        collections = list(a = 1),
        stac_api = NULL,
        catalog_file = "cat.json"
    ))
    # Returned invisibly.
    expect_false(result$visible)

    stored <- api_attr(api, "collections")
    expect_equal(stored$collections, list(a = 1))
    expect_null(stored$stac_api)
    expect_equal(stored$catalog_file, "cat.json")
})

test_that("load_collections defaults are all NULL", {
    api <- mock_create_openeo_v1()
    load_collections(api)
    stored <- api_attr(api, "collections")
    expect_null(stored$collections)
    expect_null(stored$stac_api)
    expect_null(stored$catalog_file)
})
