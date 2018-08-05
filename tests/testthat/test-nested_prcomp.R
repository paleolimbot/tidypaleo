context("test-nested_prcomp.R")

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

test_that("nested_prcomp works with a grouping variable", {
  ndm_grp <- nested_data_matrix(keji_lakes_plottable, taxon, rel_abund, depth, fill = 0, trans = sqrt, groups = location)
  prcomp_grp <- nested_prcomp(ndm_grp)

  expect_true("location" %in% colnames(prcomp_grp))
  expect_true(is.atomic(prcomp_grp$location))
  # ggplot2::ggplot(tidyr::unnest(prcomp_grp, variance), ggplot2::aes(component, variance_proportion)) +
  #   ggplot2::geom_point() +
  #   ggplot2::facet_wrap(vars(location))
})
