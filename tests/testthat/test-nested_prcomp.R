context("test-nested_prcomp.R")

test_that("nested_pca works as intended", {

  ndm <- nested_data(
    alta_lake_geochem,
    qualifiers = c(depth, zone),
    key = param,
    value = value,
    trans = scale
  )

  ndm_pca <- nested_prcomp(ndm)

  expect_is(ndm_pca, "nested_prcomp")

  expect_equal(
    colnames(ndm_pca),
    c("discarded_columns", "discarded_rows", "qualifiers", "data",
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
  ndm_grp <- nested_data(
    keji_lakes_plottable,
    depth, taxon, rel_abund,
    fill = 0, trans = sqrt, select_if = ~any(. != 0),
    groups = location
  )
  prcomp_grp <- nested_prcomp(ndm_grp)

  expect_true("location" %in% colnames(prcomp_grp))
  expect_true(is.atomic(prcomp_grp$location))

  plot(prcomp_grp, main = location, sub = "grouped PCA skree")
  biplot(prcomp_grp, main = location, sub = "grouped PCA biplot")
})

test_that("biplot works with nested_prcomp objects", {
  ndm <- nested_data(
    alta_lake_geochem,
    qualifiers = c(depth, zone),
    key = param,
    value = value,
    trans = scale
  )

  ndm_pca <- nested_prcomp(ndm)

  ndm_grp <- nested_data(
    keji_lakes_plottable,
    depth, taxon, rel_abund,
    fill = 0, trans = sqrt, select_if = ~any(. != 0),
    groups = location
  )
  prcomp_grp <- nested_prcomp(ndm_grp)

  biplot(prcomp_grp, main = location)
  biplot(ndm_pca)

  expect_true(TRUE)
})
