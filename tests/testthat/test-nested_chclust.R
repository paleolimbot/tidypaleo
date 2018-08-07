context("test-nested_chclust_coniss.R")


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
    colnames(tidyr::unnest(nested_coniss, segments)),
    c("node_id", "hclust_zone", "depth", "dendro_order", "depth_end",
      "dendro_order_end", "dispersion", "dispersion_end", "row_number", "row_number_end")
  )

  expect_setequal(
    colnames(tidyr::unnest(nested_coniss, broken_stick)),
    c("n_groups", "dispersion", "broken_stick_dispersion")
  )

  expect_setequal(
    colnames(tidyr::unnest(nested_coniss, nodes)),
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

  print(
    ggplot2::ggplot(alta_lake_geochem, ggplot2::aes(x = value, y = depth)) +
      geom_lineh() +
      ggplot2::geom_point() +
      facet_geochem_wrap(vars(param)) +
      ggplot2::geom_point(
        ggplot2::aes(x = dispersion),
        data = tidyr::unnest(nested_coniss, nodes) %>% dplyr::mutate(param = "Z_CONISS"),
        size = 0.4
      ) +
      ggplot2::geom_segment(
        ggplot2::aes(x = dispersion, xend = dispersion_end, yend = depth_end, col = factor(hclust_zone)),
        data = tidyr::unnest(nested_coniss, segments) %>% dplyr::mutate(param = "Z_CONISS"),
        size = 0.3
      ) +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = boundary_depth),
        data = tidyr::unnest(nested_coniss, zone_info),
        na.rm = TRUE,
        lty = 2, col = "red"
      ) +
      ggplot2::scale_y_reverse() +
      ggplot2::labs(caption = "CONISS nodes in the right place, dendroram, and boundary lines in the correct place")
  )

  print(
    nested_coniss %>%
      tidyr::unnest(broken_stick) %>%
      tidyr::gather(type, value, broken_stick_dispersion, dispersion) %>%
      ggplot2::ggplot(ggplot2::aes(n_groups, value, col = type)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::labs(caption = "3 groups should be a plausible number based on this")
  )

  expect_true(TRUE)
})

test_that("nested_chclust_coniss works with a grouping variable", {
  ndm_grp <- nested_data(keji_lakes_plottable, depth, taxon, rel_abund, fill = 0, groups = location)
  nested_coniss <- nested_chclust_coniss(ndm_grp)

  expect_true("location" %in% colnames(nested_coniss))
  expect_true(is.atomic(nested_coniss$location))

  print(
    ggplot2::ggplot() +
      ggplot2::geom_segment(
        ggplot2::aes(y = depth, x = dispersion, xend = dispersion_end, yend = depth_end, col = factor(hclust_zone)),
        data = tidyr::unnest(nested_coniss, segments) %>% dplyr::mutate(param = "Z_CONISS"),
        size = 0.3
      ) +
      ggplot2::geom_hline(
        ggplot2::aes(yintercept = boundary_depth),
        data = tidyr::unnest(nested_coniss, zone_info),
        na.rm = TRUE,
        lty = 2, col = "red"
      ) +
      ggplot2::scale_y_reverse() +
      ggplot2::facet_wrap(vars(location)) +
      ggplot2::labs(caption = "two CONISS dendrorams, with two groups each and one boundary line")
  )

  print(
    nested_coniss %>%
      tidyr::unnest(broken_stick) %>%
      tidyr::gather(type, value, broken_stick_dispersion, dispersion) %>%
      ggplot2::ggplot(ggplot2::aes(n_groups, value, col = type)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(vars(location)) +
      ggplot2::labs(caption = "2 groups should be a plausible number for both based on this")
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

  plot(
    nest_hc,
    main = sprintf("CCC = %0.2f", CCC),
    labels = paste(qualifiers$location, qualifiers$sample_type),
    sub = "no fishy subtext (unconstrained)",
    cex = 0.6
  )

  plot(
    nest_chc,
    main = sprintf("CCC = %0.2f", CCC),
    labels = paste(qualifiers$location, qualifiers$sample_type),
    sub = "no fishy subtext (unconstrained)",
    cex = 0.6
  )

  plot(
    nest_chc2,
    main = sprintf("CCC = %0.2f", CCC),
    labels = paste(qualifiers$location, qualifiers$sample_type),
    sub = "no fishy subtext (unconstrained, conslink)",
    cex = 0.6
  )

  expect_true(TRUE)
})

test_that("autoplot methods work with hclust objects", {
  ndm <- nested_data(halifax_lakes_plottable, c(location, sample_type), taxon, rel_abund)
  nest_hc <- nested_hclust(ndm, method = "average")
  nest_chc2 <- nested_chclust_conslink(ndm)
  nest_chc <- nested_chclust_coniss(ndm)

  print(ggplot2::autoplot(nest_hc))
  print(ggplot2::autoplot(nest_hc, ggplot2::aes(x = dendro_order, xend = dendro_order_end, col = hclust_zone), flip = TRUE))
  print(ggplot2::autoplot(nest_chc))
  print(ggplot2::autoplot(nest_chc2))

  ndm_grp <- nested_data(keji_lakes_plottable, depth, taxon, rel_abund, fill = 0, groups = location)
  nested_coniss <- nested_chclust_coniss(ndm_grp)
  print(ggplot2::autoplot(nested_coniss, ggplot2::aes(x = depth, xend = depth_end), ncol = 1, node_geom = NULL))

  expect_true(TRUE)
})
