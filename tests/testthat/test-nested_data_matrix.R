
test_that("nested_data works as intended", {

  ndm <- nested_data(
    alta_lake_geochem,
    qualifiers = c(depth, zone),
    key = param,
    value = value
  )
  expect_equal(get_grouping_vars(ndm), character(0))

  expect_is(ndm, "tbl_df")
  expect_equal(nrow(ndm), 1)
  expect_equal(
    colnames(ndm),
    c("discarded_columns", "discarded_rows", "qualifiers", "data")
  )
  expect_setequal(
    colnames(ndm$qualifiers[[1]]),
    c("depth", "zone", "row_number")
  )
  expect_true(!any(c("depth", "zone") %in% colnames(ndm$data[[1]])))
  expect_true(all(purrr::map_lgl(ndm, ~inherits(.x[[1]], "tbl_df"))))
  expect_equal(ncol(ndm$discarded_columns[[1]]), 0)
  expect_equal(nrow(ndm$discarded_rows[[1]]), 0)

  withr::with_seed(1234, {
    ndm_filtered <- nested_data(
      alta_lake_geochem,
      qualifiers = c(depth, zone),
      key = param,
      value = value,
      filter_all = dplyr::all_vars(runif(length(.)) > 0.1)
    )

    expect_equal(nrow(ndm_filtered$data[[1]]), 19)
    expect_equal(ncol(ndm_filtered$discarded_columns[[1]]), 0)
    expect_equal(nrow(ndm_filtered$discarded_rows[[1]]), 13)
  })

  ndm_selected <- nested_data(
    alta_lake_geochem,
    qualifiers = c(depth, zone),
    key = param,
    value = value,
    select_if = ~mean(.) > 1
  )

  expect_setequal(colnames(ndm_selected$discarded_columns[[1]]), c("d13C", "Ti"))
  expect_true(!any(c("d13C", "Ti") %in% colnames(ndm_selected$data[[1]])))

  withr::with_seed(1234, {
    ndm_selected_filtered <- nested_data(
      alta_lake_geochem,
      key = param,
      value = value,
      qualifiers = c(depth, zone),
      select_if = ~mean(.) > 1,
      filter_all = dplyr::all_vars(runif(length(.)) > 0.1)
    )

    expect_equal(nrow(ndm_selected_filtered$data[[1]]), 22)
    expect_equal(ncol(ndm_selected_filtered$discarded_columns[[1]]), 2)
    expect_equal(nrow(ndm_selected_filtered$discarded_rows[[1]]), 10)

    expect_setequal(colnames(ndm_selected_filtered$discarded_columns[[1]]), c("d13C", "Ti"))
    expect_true(!any(c("d13C", "Ti") %in% colnames(ndm_selected_filtered$data[[1]])))
  })

  grouped_ndm <- nested_data(
    alta_lake_geochem,
    key = param,
    value = value,
    qualifiers = c(depth, zone),
    groups = location
  )
  expect_equal(get_grouping_vars(grouped_ndm), "location")
  expect_equal(
    colnames(grouped_ndm),
    c("location", "discarded_columns", "discarded_rows", "qualifiers", "data")
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

test_that("unnested_data works", {
  grouped_ndm <- nested_data(
    alta_lake_geochem,
    key = param,
    value = value,
    qualifiers = c(depth, zone),
    groups = location
  )

  expect_true("location" %in% names(unnested_data(grouped_ndm, data)))
  expect_equal(nrow(unnested_data(grouped_ndm, qualifiers, data)), nrow(grouped_ndm$data[[1]]))
})

test_that("nested_data works as intended with wide data", {

  alg_wide <- tidyr::spread(dplyr::select(alta_lake_geochem, depth, zone, param, value), param, value)

  ndm_wide <- nested_data(
    alg_wide,
    qualifiers = c(depth, zone),
    value = dplyr::everything()
  )

  ndm_long <- nested_data(
    alta_lake_geochem,
    qualifiers = c(depth, zone),
    key = param,
    value = value
  )

  expect_identical(ndm_wide, ndm_long)

  expect_message(
    nested_data(
      alg_wide,
      qualifiers = depth,
      key = zone,
      value = dplyr::everything()
    ),
    "Ignoring variables specified in `key`"
  )

  expect_message(
    nested_data(
      alg_wide,
      qualifiers = depth,
      value = dplyr::everything(),
      fill = 0
    ),
    "Ignoring `fill`"
  )
})

test_that("more than one value column must be specified", {
  expect_error(
    nested_data(
      alta_lake_geochem,
      qualifiers = c(depth, zone),
      key = param,
      value = NULL
    ),
    "value columns must be specified"
  )
})

test_that("nested_data gives an error with reserved column names", {
  alg_bad <- dplyr::rename(alta_lake_geochem, data = depth)
  expect_error(
    nested_data(alg_bad, qualifiers = data, key = param, value = value),
    "The following names in"
  )
})

test_that("nested_analysis gives an error with reserved column names", {
  alg_bad <- dplyr::rename(alta_lake_geochem, model = depth)
  ndm <- expect_silent(
    nested_data(alg_bad, qualifiers = model, key = param, value = value)
  )

  expect_error(
    nested_analysis(ndm, stats::prcomp, data),
    "The following names in discarded_rows"
  )

  ndm$model <- 1
  expect_error(
    nested_analysis(ndm, stats::prcomp, data),
    "The following names in .data"
  )

  ndm <- nested_data(alta_lake_geochem, depth, param, value)
  ndm$random_name <- 1
  expect_silent(
    nested_analysis(ndm, stats::prcomp, data)
  )
  expect_error(
    nested_analysis(ndm, stats::prcomp, data, .reserved_names = "random_name"),
    "The following names in .data"
  )

})

test_that("nested data matrix works with a grouping variable", {
  ndm_grp <- nested_data(keji_lakes_plottable, depth, taxon, rel_abund, fill = 0, groups = location)
  expect_identical(
    ndm_grp,
    nested_data(dplyr::group_by(keji_lakes_plottable, location), depth, taxon, rel_abund, fill = 0)
  )

  expect_true("location" %in% colnames(ndm_grp))
  expect_true(is.atomic(ndm_grp$location))
})

test_that("class inheritance works", {
  ndm <- nested_data(alta_lake_geochem, depth, param, value, trans = scale)
  na <- nested_analysis(ndm, stats::prcomp, data)
  class(na) <- setdiff(class(na), "nested_data")

  expect_is(ndm, "nested_data")
  expect_is(na, "nested_analysis")
  expect_false(inherits(na, "nested_data"))

  expect_is(dplyr::filter(ndm, TRUE), "nested_data")
  expect_is(dplyr::filter(na, TRUE), "nested_analysis")

  expect_is(dplyr::arrange(ndm, 1), "nested_data")
  expect_is(dplyr::arrange(na, 1), "nested_analysis")

  # dropping a column results in loss of class propogation
  expect_is(dplyr::mutate(ndm, new_thing = 1), "nested_data")
  expect_is(dplyr::mutate(na, new_thing = 1), "nested_analysis")
  expect_false(inherits(dplyr::mutate(ndm, data = NULL), "nested_data"))
  expect_false(inherits(dplyr::mutate(na, data = NULL), "nested_analysis"))

  expect_is(dplyr::slice(ndm, 1), "nested_data")
  expect_is(dplyr::slice(na, 1), "nested_analysis")


  expect_equal(nrow(dplyr::filter(ndm, FALSE)), 0)
  expect_equal(nrow(dplyr::slice(ndm, numeric(0))), 0)
  expect_equal(nrow(dplyr::filter(ndm, TRUE)), 1)
  expect_equal(nrow(dplyr::slice(ndm, 1)), 1)
})

test_that("nested anal plotting works", {
  ndm <- nested_data(alta_lake_geochem, depth, param, value, trans = scale)
  ndm_grp <- nested_data(keji_lakes_plottable, depth, taxon, rel_abund, fill = 0, trans = sqrt, groups = location)

  pca <- nested_analysis(ndm, stats::prcomp, data)
  pca_grp <- nested_analysis(ndm_grp, stats::prcomp, data)

  expect_identical(plot(pca, sub = "default"), pca)
  expect_identical(plot(pca_grp, sub = "default"), pca_grp)
  expect_length(plot(pca, sub = "default", .output_column = "plot")$plot, 1)
  expect_length(plot(pca_grp, main = location, sub = "default grouped", .output_column = "plot")$plot, 2)

  plot(pca_grp, main = location, nrow = 2, sub = "nrow = 2")
  plot(pca_grp, main = location, ncol = 1, sub = "ncol = 1")

  # silent handling of zero-row analyses
  expect_equal(plot(dplyr::filter(pca_grp, FALSE)) %>% nrow(), 0)
})
