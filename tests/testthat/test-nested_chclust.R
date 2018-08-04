context("test-nested_chclust.R")


test_that("nested_chclust produces the correct columns", {

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
      "data", "distance", "model", "broken_stick", "n_groups", "dendro_order", "chclust_zone",
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

test_that("nested_chclust produces the correct segments and nodes", {

  ndm <- nested_data_matrix(
    alta_lake_geochem,
    key = param,
    value = value,
    trans = scale,
    qualifiers = depth
  )

  nested_coniss <- nested_chclust(ndm)

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
        ggplot2::aes(x = dispersion, xend = dispersion_end, yend = depth_end, col = factor(chclust_zone)),
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
      ggplot2::labs(caption = "3 groups should be a plausible number based on this...")
  )

  expect_true(TRUE)
})

test_that("nested_chclust works with a grouping variable", {
  ndm_grp <- nested_data_matrix(keji_lakes_plottable, taxon, rel_abund, depth, fill = 0, groups = location)
  nested_coniss <- nested_chclust(ndm_grp)

  expect_true("location" %in% colnames(nested_coniss))
  expect_true(is.atomic(nested_coniss$location))

  print(
    ggplot2::ggplot() +
      ggplot2::geom_segment(
        ggplot2::aes(y = depth, x = dispersion, xend = dispersion_end, yend = depth_end, col = factor(chclust_zone)),
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
      ggplot2::facet_wrap(vars(location)) +
      ggplot2::labs(caption = "2 groups should be a plausible number for both based on this...")
  )
})
