
test_that("facet reordering works", {

  p <- mtcars %>%
    dplyr::mutate(
      # factor not in sorted order
      cyl_fct = paste("cyl =", cyl) %>% factor(levels = c("cyl = 8", "cyl = 4", "cyl = 6")),
      # character
      gear_fct = paste("gear =", gear)
    ) %>%
    ggplot2::ggplot(ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point() +

    # additional layer with character versions of what is a factor in original data
    ggplot2::geom_point(
      ggplot2::aes(x = 5, y = mpg_line),
      data = tibble::tibble(
        gear_fct = c("gear = 3", "gear = 4", "gear = 5", "gear = 6"),
        cyl_fct = c("cyl = 4", "cyl = 6", "cyl = 8", "cyl = 2"),
        mpg_line = c(15, 25, 35, 45)
      ),
      col = "red"
    )

  vdiffr::expect_doppelganger(
    "sequential_layer_facet grid",
    p +
      ggplot2::facet_grid(ggplot2::vars(cyl_fct), ggplot2::vars(gear_fct)) +
      sequential_layer_facets()
  )

  vdiffr::expect_doppelganger(
    "sequential_layer_facet wrap",
    p +
      ggplot2::facet_wrap(ggplot2::vars(cyl_fct, gear_fct)) +
      sequential_layer_facets()
  )

  expect_silent(
    ggplot2::ggplot_build(
      p + ggplot2::facet_null()
    )
  )
})

test_that("CONISS can be added to a plot", {
  coniss <- alta_lake_geochem %>%
    nested_data(age, param, value, trans = scale) %>%
    nested_chclust_coniss()

  # skip("CONISS plots do not render identically between vdiffrAddin() and CMD check")
  withr::with_envvar(list(VDIFFR_RUN_TESTS = FALSE), {
    vdiffr::expect_doppelganger(
      "plot coniss y",
      ggplot2::ggplot(alta_lake_geochem, ggplot2::aes(x = value, y = age)) +
        geom_lineh() +
        ggplot2::facet_grid(cols = vars(param)) +
        layer_dendrogram(coniss, ggplot2::aes(y = age), param = "CONISS") +
        layer_zone_boundaries(coniss, ggplot2::aes(y = age))
    )

    vdiffr::expect_doppelganger(
      "plot coniss x",
      ggplot2::ggplot(alta_lake_geochem, ggplot2::aes(x = age, y = value)) +
        ggplot2::geom_line() +
        ggplot2::facet_grid(rows = vars(param)) +
        layer_dendrogram(coniss, ggplot2::aes(x = age), param = "CONISS") +
        layer_zone_boundaries(coniss, ggplot2::aes(x = age))
    )

    grp_coniss <- keji_lakes_plottable %>%
      dplyr::group_by(location) %>%
      nested_data(depth, taxon, rel_abund) %>%
      nested_chclust_coniss()

    vdiffr::expect_doppelganger(
      "plot coniss abundance y",
      plot_layer_dendrogram(grp_coniss, ggplot2::aes(y = depth), taxon = "CONISS") +
        ggplot2::facet_grid(rows = vars(location), cols = vars(taxon)) +
        ggplot2::scale_y_reverse()
    )

    vdiffr::expect_doppelganger(
      "plot coniss abundance x",
      plot_layer_dendrogram(grp_coniss, ggplot2::aes(x = depth), taxon = "CONISS") +
        ggplot2::facet_grid(cols = vars(location)) +
        ggplot2::scale_y_reverse()
    )
  })
})

test_that("PCAs can be added to a plot", {

  pca <- alta_lake_geochem %>%
    nested_data(age, param, value, trans = scale) %>%
    nested_prcomp()

  # skip("PCA plots do not render identically between vdiffrAddin() and CMD check")
  withr::with_envvar(list(VDIFFR_RUN_TESTS = FALSE), {
    vdiffr::expect_doppelganger(
      "plot PCA x",
      ggplot2::ggplot(alta_lake_geochem, ggplot2::aes(x = value, y = age)) +
        geom_lineh() +
        ggplot2::facet_grid(cols = vars(param)) +
        layer_scores(pca, key = "param", which = c("PC1", "PC2"))
    )

    vdiffr::expect_doppelganger(
      "plot PCA y",
      ggplot2::ggplot(alta_lake_geochem, ggplot2::aes(y = value, x = age)) +
        ggplot2::geom_line() +
        ggplot2::facet_grid(rows = vars(param)) +
        layer_scores(pca, key = "param", value = "value", which = c("PC1", "PC2"))
    )

    grp_pca <- keji_lakes_plottable %>%
      dplyr::group_by(location) %>%
      nested_data(depth, taxon, rel_abund, trans = sqrt) %>%
      nested_prcomp()

    vdiffr::expect_doppelganger(
      "plot PCA scores y (rev)",
      plot_layer_scores(grp_pca, ggplot2::aes(y = depth), which = c("PC1", "PC2")) +
        ggplot2::scale_y_reverse()
    )

    vdiffr::expect_doppelganger(
      "plot PCA scores x",
      plot_layer_scores(grp_pca, ggplot2::aes(x = depth), which = c("PC1", "PC2"))
    )
  })
})
