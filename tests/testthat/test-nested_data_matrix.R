context("test-nested_data_matrix.R")

test_that("nested_data_matrix works as intended", {

  ndm <- nested_data_matrix(
    alta_lake_geochem,
    key = param,
    value = value,
    qualifiers = c(depth, zone)
  )

  expect_is(ndm, "tbl_df")
  expect_equal(nrow(ndm), 1)
  expect_equal(
    colnames(ndm),
    c("wide_df", "discarded_columns", "discarded_rows", "qualifiers", "data")
  )
  expect_setequal(
    colnames(ndm$qualifiers[[1]]),
    c("depth", "zone", "row_number")
  )
  expect_true(!any(c("depth", "zone") %in% colnames(ndm$data[[1]])))
  expect_true(all(purrr::map_lgl(ndm, ~inherits(.x[[1]], "tbl_df"))))

  withr::with_seed(1234, {
    ndm_filtered <- nested_data_matrix(
      alta_lake_geochem,
      key = param,
      value = value,
      qualifiers = c(depth, zone),
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
    qualifiers = c(depth, zone),
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
      qualifiers = c(depth, zone),
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
    qualifiers = c(depth, zone),
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
    qualifiers = c(depth, zone),
    trans = scale
  )

  ndm_pca <- nested_prcomp(ndm)

  expect_equal(
    colnames(ndm_pca),
    c("wide_df", "discarded_columns", "discarded_rows", "qualifiers", "data",
      "model", "variance", "loadings", "scores")
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

test_that("nested_chclust works as intended", {

  ndm <- nested_data_matrix(
    alta_lake_geochem,
    key = param,
    value = value,
    qualifiers = c(depth, zone),
    trans = scale
  )

  nested_coniss <- nested_chclust(ndm)

  expect_setequal(
    colnames(nested_coniss),
    c("wide_df", "discarded_columns", "discarded_rows", "qualifiers",
      "data", "distance", "model", "broken_stick", "n_groups", "chclust_zone",
      "zone_info", "nodes", "segments")
  )

  expect_setequal(
    colnames(tidyr::unnest(nested_coniss, segments)),
    c("node_id", "chclust_zone", "depth", "dendro_order", "depth_end",
      "dendro_order_end", "dispersion", "dispersion_end", "row_number", "row_number_end")
  )

  expect_setequal(
    colnames(tidyr::unnest(nested_coniss, broken_stick)),
    c("n_groups", "dispersion", "broken_stick_dispersion")
  )

  expect_setequal(
    colnames(tidyr::unnest(nested_coniss, nodes)),
    c("depth", "dendro_order", "chclust_zone", "is_leaf", "dispersion",
      "recursive_level", "node_id", "zone", "row_number")
  )

})



