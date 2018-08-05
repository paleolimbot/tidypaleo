context("test-nested_data_matrix.R")

test_that("nested_data_matrix works as intended", {

  ndm <- nested_data_matrix(
    alta_lake_geochem,
    key = param,
    value = value,
    qualifiers = c(depth, zone)
  )
  expect_equal(get_grouping_vars(ndm), character(0))

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
  expect_equal(get_grouping_vars(grouped_ndm), "location")
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

test_that("nested_data_matrix gives an error with reserved column names", {
  alg_bad <- dplyr::rename(alta_lake_geochem, data = depth)
  expect_error(
    nested_data_matrix(alg_bad, param, value, data),
    "The following names in"
  )
})

test_that("nested_anal gives an error with reserved column names", {
  alg_bad <- dplyr::rename(alta_lake_geochem, model = depth)
  ndm <- expect_silent(
    nested_data_matrix(alg_bad, param, value, model)
  )

  expect_error(
    nested_anal(ndm, data_column = "data", fun = stats::prcomp, data_arg = "x"),
    "The following names in wide_df"
  )

  ndm$model <- 1
  expect_error(
    nested_anal(ndm, data_column = "data", fun = stats::prcomp, data_arg = "x"),
    "The following names in .data"
  )

  ndm <- nested_data_matrix(alta_lake_geochem, param, value, depth)
  ndm$random_name <- 1
  expect_silent(
    nested_anal(ndm, data_column = "data", fun = stats::prcomp, data_arg = "x")
  )
  expect_error(
    nested_anal(ndm, data_column = "data", fun = stats::prcomp, data_arg = "x", reserved_names = "random_name"),
    "The following names in .data"
  )

})

test_that("nested data matrix works with a grouping variable", {
  ndm_grp <- nested_data_matrix(keji_lakes_plottable, taxon, rel_abund, depth, fill = 0, groups = location)
  expect_identical(
    ndm_grp,
    nested_data_matrix(dplyr::group_by(keji_lakes_plottable, location), taxon, rel_abund, depth, fill = 0)
  )

  expect_true("location" %in% colnames(ndm_grp))
  expect_true(is.atomic(ndm_grp$location))
})

test_that("class inheritance works", {
  ndm <- nested_data_matrix(alta_lake_geochem, param, value, depth, trans = scale)
  na <- nested_anal(ndm, data_column = "data", fun = stats::prcomp, data_arg = "x")
  expect_is(ndm, "nested_data_matrix")
  expect_is(na, "nested_anal")
  expect_is(na, "nested_data_matrix")
  expect_is(dplyr::filter(ndm, TRUE), "nested_data_matrix")
  expect_is(dplyr::filter(na, TRUE), "nested_anal")
  expect_is(dplyr::filter(na, TRUE), "nested_data_matrix")
  expect_is(dplyr::slice(ndm, 1), "nested_data_matrix")
  expect_is(dplyr::slice(na, 1), "nested_anal")
  expect_is(dplyr::slice(na, 1), "nested_data_matrix")

  expect_equal(nrow(dplyr::filter(ndm, FALSE)), 0)
  expect_equal(nrow(dplyr::slice(ndm, numeric(0))), 0)
  expect_equal(nrow(dplyr::filter(ndm, TRUE)), 1)
  expect_equal(nrow(dplyr::slice(ndm, 1)), 1)
})

test_that("nested anal plotting works", {
  ndm <- nested_data_matrix(alta_lake_geochem, param, value, depth, trans = scale)
  ndm_grp <- nested_data_matrix(keji_lakes_plottable, taxon, rel_abund, depth, fill = 0, trans = sqrt, groups = location)
  pca <- nested_anal(ndm, data_column = "data", fun = stats::prcomp, data_arg = "x")
  pca_grp <- nested_anal(ndm_grp, data_column = "data", fun = stats::prcomp, data_arg = "x")

  expect_length(plot(pca, sub = "default"), 1)
  expect_length(plot(pca_grp, plot_labels = location, sub = "default grouped"), 2)

  plot(pca_grp, plot_labels = location, nrow = 2, sub = "nrow = 2")
  plot(pca_grp, plot_labels = location, ncol = 1, sub = "ncol = 1")

  expect_error(plot(dplyr::filter(pca_grp, FALSE)), "Nothing to plot")
})
