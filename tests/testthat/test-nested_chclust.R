
test_that("nested_chclust_coniss produces the correct columns", {

  ndm <- nested_data(
    alta_lake_geochem,
    qualifiers = c(depth, zone),
    key = param,
    value = value,
    trans = scale
  )

  nested_coniss <- nested_chclust_coniss(ndm)
  expect_is(nested_coniss, "nested_chclust_coniss")
  expect_is(nested_coniss, "nested_hclust")

  expect_setequal(
    colnames(nested_coniss),
    c("discarded_columns", "discarded_rows", "qualifiers",
      "data", "distance", "model", "CCC", "broken_stick", "n_groups", "dendro_order", "hclust_zone",
      "zone_info", "nodes", "segments")
  )

  expect_setequal(
    colnames(tidyr::unnest(drop_list_cols(nested_coniss, "segments"), segments)),
    c("node_id", "hclust_zone", "depth", "dendro_order", "depth_end",
      "dendro_order_end", "dispersion", "dispersion_end", "row_number", "row_number_end")
  )

  expect_setequal(
    colnames(tidyr::unnest_legacy(nested_coniss, broken_stick)),
    c("n_groups", "dispersion", "broken_stick_dispersion")
  )

  expect_setequal(
    colnames(tidyr::unnest_legacy(nested_coniss, nodes)),
    c("depth", "dendro_order", "hclust_zone", "is_leaf", "dispersion",
      "recursive_level", "node_id", "zone", "row_number")
  )

})

test_that("nested_chclust_coniss produces the correct segments and nodes", {

  ndm <- nested_data(
    alta_lake_geochem,
    qualifiers = c(depth, zone),
    key = param,
    value = value,
    trans = scale
  )

  nested_coniss <- nested_chclust_coniss(ndm)

  # skip("coniss placement test do not render identically between vdiffrAddin() and CMD check")
  withr::with_envvar(list(VDIFFR_RUN_TESTS = FALSE), {
    vdiffr::expect_doppelganger(
      "CONISS placement",
      ggplot2::ggplot(alta_lake_geochem, ggplot2::aes(x = value, y = depth)) +
        geom_lineh() +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(vars(param), scales = "free_x") +
        ggplot2::geom_point(
          ggplot2::aes(x = dispersion),
          data = tidyr::unnest_legacy(nested_coniss, nodes) %>% dplyr::mutate(param = "Z_CONISS"),
          size = 0.4
        ) +
        ggplot2::geom_segment(
          ggplot2::aes(x = dispersion, xend = dispersion_end, yend = depth_end, col = factor(hclust_zone)),
          data = tidyr::unnest_legacy(nested_coniss, segments) %>% dplyr::mutate(param = "Z_CONISS"),
          linewidth = 0.3
        ) +
        ggplot2::geom_hline(
          ggplot2::aes(yintercept = boundary_depth),
          data = tidyr::unnest_legacy(nested_coniss, zone_info),
          na.rm = TRUE,
          lty = 2, col = "red"
        ) +
        ggplot2::scale_y_reverse()
    )

    vdiffr::expect_doppelganger(
      "CONISS bstick",
      nested_coniss %>%
        tidyr::unnest_legacy(broken_stick) %>%
        tidyr::gather(type, value, broken_stick_dispersion, dispersion) %>%
        ggplot2::ggplot(ggplot2::aes(n_groups, value, col = type)) +
        ggplot2::geom_point() +
        ggplot2::geom_line()
    )
  })
})

test_that("nested_chclust_coniss works with a grouping variable", {
  ndm_grp <- nested_data(keji_lakes_plottable, depth, taxon, rel_abund, fill = 0, groups = location)
  nested_coniss <- nested_chclust_coniss(ndm_grp)

  expect_true("location" %in% colnames(nested_coniss))
  expect_true(is.atomic(nested_coniss$location))

  vdiffr::expect_doppelganger(
    "CONISS placement grouped",
    ggplot2::ggplot() +
      ggplot2::geom_segment(
        ggplot2::aes(
          y = depth, x = dispersion,
          xend = dispersion_end, yend = depth_end,
          col = factor(hclust_zone)
        ),
        data = tidyr::unnest_legacy(nested_coniss, segments) %>% dplyr::mutate(param = "Z_CONISS"),
        linewidth = 0.3
      ) +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = boundary_depth),
        data = tidyr::unnest_legacy(nested_coniss, zone_info),
        na.rm = TRUE,
        lty = 2, col = "red"
      ) +
      ggplot2::scale_y_reverse() +
      ggplot2::facet_wrap(vars(location))
  )

  vdiffr::expect_doppelganger(
    "CONISS bstick grouped",
    nested_coniss %>%
      tidyr::unnest_legacy(broken_stick) %>%
      tidyr::gather(type, value, broken_stick_dispersion, dispersion) %>%
      ggplot2::ggplot(ggplot2::aes(n_groups, value, col = type)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(vars(location))
  )
})

test_that("nested hclust works as planned", {
  ndm <- nested_data(halifax_lakes_plottable, c(location, sample_type), taxon, rel_abund)
  nest_hc <- nested_hclust(ndm)
  expect_is(nest_hc, "nested_hclust")
})

test_that("plot methods for hclust work", {
  ndm <- nested_data(halifax_lakes_plottable, c(location, sample_type), taxon, rel_abund)
  nest_hc <- nested_hclust(ndm, method = "average")
  nest_chc2 <- nested_chclust_conslink(ndm)
  nest_chc <- nested_chclust_coniss(ndm)

  vdiffr::expect_doppelganger(
    "nested hclust plot",
    function() plot(
      nest_hc,
      main = sprintf("CCC = %0.2f", CCC),
      labels = paste(qualifiers$location, qualifiers$sample_type),
      sub = "no fishy subtext (unconstrained)",
      cex = 0.6
    )
  )

  vdiffr::expect_doppelganger(
    "nested CONISS plot",
    function() plot(
      nest_chc,
      main = sprintf("CCC = %0.2f", CCC),
      labels = paste(qualifiers$location, qualifiers$sample_type),
      sub = "no fishy subtext (unconstrained)",
      cex = 0.6
    )
  )

  vdiffr::expect_doppelganger(
    "nested conslink plot",
    function() plot(
      nest_chc2,
      main = sprintf("CCC = %0.2f", CCC),
      labels = paste(qualifiers$location, qualifiers$sample_type),
      sub = "no fishy subtext (unconstrained, conslink)",
      cex = 0.6
    )
  )
})

test_that("stat_nested_hclust methods work with hclust objects", {
  ndm <- nested_data(halifax_lakes_plottable[halifax_lakes_plottable$sample_type == "top",], c(location), taxon, rel_abund)
  nest_hc <- nested_hclust(ndm, method = "average")
  nest_hc_order <- dplyr::arrange(
    tidyr::unnest(
      nest_hc,
      c(qualifiers, dendro_order)
    ),
    dendro_order
  )$location

  vdiffr::expect_doppelganger(
    "stat_nested_hclust x",
    ggplot2::ggplot(nest_hc, ggplot2::aes(x = location)) +
      stat_nested_hclust() +
      ggplot2::scale_x_discrete(limits = nest_hc_order) +
      rotated_axis_labels()
  )

  vdiffr::expect_doppelganger(
    "stat_nested_hclust y",
    ggplot2::ggplot(nest_hc, ggplot2::aes(y = location)) +
      stat_nested_hclust() +
      ggplot2::scale_y_discrete(limits = nest_hc_order)
  )

  #  nested version
  ndm_grp <- nested_data(keji_lakes_plottable, depth, taxon, rel_abund, fill = 0, groups = location)
  nested_coniss <- nested_chclust_coniss(ndm_grp)

  vdiffr::expect_doppelganger(
    "depth stat_nested_hclust",
    ggplot2::ggplot() +
      stat_nested_hclust(ggplot2::aes(y = depth), data = nested_coniss) +
      ggplot2::facet_wrap(vars(location)) +
      ggplot2::scale_y_reverse() +
      ggplot2::labs(caption = "Dendrograms in different panels")
  )

  # skip("nested hclust test with aes does not render identically between R 3.6 and R 4.0")
  withr::with_envvar(list(VDIFFR_RUN_TESTS = FALSE), {
    vdiffr::expect_doppelganger(
      "stat_nested_hclust aes",
      ggplot2::ggplot() +
        stat_nested_hclust(ggplot2::aes(x = depth, col = location), data = nested_coniss) +
        ggplot2::labs(caption = "Dendrograms in different colours")
    )
  })
})
