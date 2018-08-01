context("test-nested_data_matrix.R")

test_that("nested_data_matrix works as intended", {

  ndm <- nested_data_matrix(
    alta_lake_geochem,
    key = param,
    value = value,
    qualifiers = c(zone, depth)
  )

  expect_is(ndm, "tbl_df")
  expect_equal(nrow(ndm), 1)
  expect_equal(
    colnames(ndm),
    c("wide_df", "discarded_columns", "discarded_rows", "qualifiers", "data")
  )
  expect_setequal(
    colnames(ndm$qualifiers[[1]]),
    c("depth", "zone")
  )
  expect_true(!any(c("depth", "zone") %in% colnames(ndm$data[[1]])))
  expect_true(all(purrr::map_lgl(ndm, ~inherits(.x[[1]], "tbl_df"))))

  withr::with_seed(1234, {
    ndm_filtered <- nested_data_matrix(
      alta_lake_geochem,
      key = param,
      value = value,
      qualifiers = c(zone, depth),
      filter_all = dplyr::all_vars(runif(length(.)) > 0.1)
    )

    expect_equal(nrow(ndm_filtered$wide_df[[1]]), 19)
    expect_equal(ncol(ndm_filtered$discarded_columns[[1]]), 0)
    expect_equal(nrow(ndm_filtered$discarded_rows[[1]]), 13)
  })

  ndm_selected <- nested_data_matrix(
    alta_lake_geochem,
    key = param,
    value = value,
    qualifiers = c(zone, depth),
    select_if = ~mean(.) > 1
  )

  expect_setequal(colnames(ndm_selected$discarded_columns[[1]]), c("d13C", "Ti"))
  expect_true(!any(c("d13C", "Ti") %in% colnames(ndm_selected$wide_df[[1]])))
  expect_true(!any(c("d13C", "Ti") %in% colnames(ndm_selected$data[[1]])))

  withr::with_seed(1234, {
    ndm_selected_filtered <- nested_data_matrix(
      alta_lake_geochem,
      key = param,
      value = value,
      qualifiers = c(zone, depth),
      select_if = ~mean(.) > 1,
      filter_all = dplyr::all_vars(runif(length(.)) > 0.1)
    )

    expect_equal(nrow(ndm_selected_filtered$wide_df[[1]]), 22)
    expect_equal(ncol(ndm_selected_filtered$discarded_columns[[1]]), 2)
    expect_equal(nrow(ndm_selected_filtered$discarded_rows[[1]]), 10)

    expect_setequal(colnames(ndm_selected_filtered$discarded_columns[[1]]), c("d13C", "Ti"))
    expect_true(!any(c("d13C", "Ti") %in% colnames(ndm_selected_filtered$wide_df[[1]])))
    expect_true(!any(c("d13C", "Ti") %in% colnames(ndm_selected_filtered$data[[1]])))
  })

  grouped_ndm <- nested_data_matrix(
    alta_lake_geochem,
    key = param,
    value = value,
    qualifiers = c(zone, depth),
    groups = location
  )
  expect_equal(
    colnames(grouped_ndm),
    c("location", "wide_df", "discarded_columns", "discarded_rows", "qualifiers", "data")
  )
  expect_true(
    all(
      purrr::map_lgl(
        purrr::set_names(c("discarded_columns", "discarded_rows", "qualifiers", "data")),
        ~all.equal(grouped_ndm[[.x]], ndm[[.x]])
      )
    )
  )
})

test_that("nested_pca works as intended", {

  ndm <- nested_data_matrix(
    alta_lake_geochem,
    key = param,
    value = value,
    qualifiers = c(zone, depth),
    trans = scale
  )

  ndm_pca <- nested_pca(ndm)

  expect_equal(
    colnames(ndm_pca),
    c("wide_df", "discarded_columns", "discarded_rows", "qualifiers", "data", "model", "variance", "loadings", "scores")
  )

  expect_identical(
    purrr::map_int(ndm_pca$qualifiers, nrow),
    purrr::map_int(ndm_pca$data, nrow)
  )

  expect_identical(
    purrr::map_int(ndm_pca$qualifiers, nrow),
    purrr::map_int(ndm_pca$scores, nrow)
  )

  expect_equal(
    ncol(ndm_pca$loadings[[1]]),
    ncol(ndm_pca$data[[1]]) + 1
  )

})



