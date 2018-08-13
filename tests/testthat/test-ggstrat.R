context("test-ggstrat.R")

test_that("facet label rotating works properly", {
  test_data <- rbind(
    data.frame(
      facet = c("a very very very very long facet name 1"),
      facet2 = c("a very very very very long facet name 1"),
      x = runif(10),
      y = runif(10),
      stringsAsFactors = FALSE
    ),
    data.frame(
      facet = c("a very very very long facet name 2"),
      facet2 = c("a very very very very very very very long facet name 1"),
      x = rnorm(10),
      y = rnorm(10),
      stringsAsFactors = FALSE
    ),
    data.frame(
      facet = c("a very very long facet name 3"),
      facet2 = c("a very very very very long facet name 2"),
      x = rlnorm(10),
      y = rlnorm(10),
      stringsAsFactors = FALSE
    ),
    data.frame(
      facet = c("a very long facet name 4"),
      facet2 = c("a very very very very long facet name 2"),
      x = rlnorm(10),
      y = rlnorm(10),
      stringsAsFactors = FALSE
    )
  )

  p <- ggplot2::ggplot(test_data, ggplot2::aes(x, y)) + ggplot2::geom_point()

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(45, direction = c("x", "y")) +
      ggplot2::labs(caption = "x and y labels rotated by 45 degrees with no strip background")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(45, direction = c("x", "y")) +
      ggplot2::labs(caption = "x and y labels rotated by 45 degrees with no strip background")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(45) +
      ggplot2::labs(caption = "only x rotated by 45 degrees (default) with no strip background")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(45, direction = "x") +
      ggplot2::labs(
        caption = "only x rotated by 45 degrees with no strip background (lack of clip on left is not our fault ggplot2 #2772...)")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(45, direction = "y") +
      ggplot2::labs(caption = "only y rotated by 45 degrees with no strip background")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(45, direction = "y") +
      ggplot2::labs(caption = "only y rotated by 45 degrees with no strip background")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(-45, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by -45 degrees in both directions (text base should align to top/right of plot)")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(-45, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by -45 degrees in both directions (text base should align to top/right of plot)")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(0, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by 0 degrees in both directions (text should align to middle)")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(0, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by 0 degrees in both directions (text should align to middle)")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(90, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by 90 degrees in both directions (text should align to middle)")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(90, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by 90 degrees in both directions (text should align to middle)")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(-90, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by -90 degrees in both directions (text should align to middle)")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(-90, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by -90 degrees in both directions (text should align to middle)")
  )

  print(
    p +
      ggplot2::facet_wrap(~facet, strip.position = "top") +
      rotated_facet_labels(45, direction = c("x", "y")) +
      ggplot2::labs(caption = "facet_wrap with top labels, 45 degree rotation")
  )

  print(
    p +
      ggplot2::facet_wrap(~facet, strip.position = "left") +
      rotated_facet_labels(45, direction = c("x", "y")) +
      ggplot2::labs(caption = "facet_wrap with left labels, 45 degree rotation")
  )

  print(
    p +
      ggplot2::facet_wrap(~facet, strip.position = "bottom") +
      rotated_facet_labels(45, direction = c("x", "y")) +
      ggplot2::labs(caption = "facet_wrap with bottom labels, 45 degree rotation")
  )

  print(
    p +
      ggplot2::facet_wrap(~facet, strip.position = "right") +
      rotated_facet_labels(45, direction = c("x", "y")) +
      ggplot2::labs(caption = "facet_wrap with right labels, 45 degree rotation")
  )

  expect_true(TRUE)
})

test_that("axis label rotating works properly", {
  p <- ggplot2::ggplot(
    data.frame(x = runif(10), y = runif(10)),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(
      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      labels = c("0", "0.2000000", "0.4", "0.6000", "0.80", "1.000"),
      limits = c(0, 1),
      sec.axis = ggplot2::dup_axis()
    ) +
    ggplot2::scale_y_continuous(
      breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
      labels = c("0", "0.2000000", "0.4", "0.6000", "0.80", "1.000"),
      limits = c(0, 1),
      sec.axis = ggplot2::dup_axis()
    )

  print(
    p +
      rotated_axis_labels(0, direction = c("x", "y")) +
      ggplot2::labs(caption = "axis labels rotated by 0 degrees, text aligned naturally")
  )

  print(
    p +
      rotated_axis_labels(90, direction = c("x", "y")) +
      ggplot2::labs(caption = "axis labels rotated by 90 degrees, text aligned naturally")
  )

  print(
    p +
      rotated_axis_labels(-90, direction = c("x", "y")) +
      ggplot2::labs(caption = "axis labels rotated by 90 degrees, text aligned naturally")
  )

  print(
    p +
      rotated_axis_labels(45, direction = c("x", "y")) +
      ggplot2::labs(caption = "axis labels rotated by 90 degrees, text aligned naturally")
  )

  print(
    p +
      rotated_axis_labels(-45, direction = c("x", "y")) +
      ggplot2::labs(caption = "axis labels rotated by 90 degrees, text aligned naturally")
  )

  # plot-generating tests
  expect_true(TRUE)
})

test_that("age depth axes work as expected", {

  adm_ident <- age_depth_model(depth = 1:10, age = -(1:10))
  test_data <- data.frame(depth = 1:20, age = -(1:20), value = cumsum(rnorm(20)))
  py <- ggplot2::ggplot(test_data, ggplot2::aes(y = depth, x = value)) + ggplot2::geom_path()
  px <- ggplot2::ggplot(test_data, ggplot2::aes(y = value, x = depth)) + ggplot2::geom_path()

  print(
    py + scale_y_depth_age(adm_ident, age_name = "age axis", age_breaks = seq(0, -20, -4)) +
      ggplot2::labs(caption = "reversed y axis, negative age axis with name, non-standard breaks")
  )

  print(
    px + scale_x_depth_age(adm_ident, age_name = "age axis", age_breaks = seq(0, -20, -4)) +
      ggplot2::labs(caption = "reversed x axis, negative age axis with name, non-standard breaks")
  )

  print(
    py + scale_y_depth_age(NULL) +
      ggplot2::labs(caption = "reversed y axis, no age axis")
  )

  print(
    px + scale_x_depth_age(NULL) +
      ggplot2::labs(caption = "reversed x axis, no age axis")
  )

  py <- ggplot2::ggplot(test_data, ggplot2::aes(y = age, x = value)) + ggplot2::geom_path()
  px <- ggplot2::ggplot(test_data, ggplot2::aes(y = value, x = age)) + ggplot2::geom_path()

  print(
    py + scale_y_age_depth(adm_ident, depth_name = "depth axis", depth_breaks = seq(0, 20, 4)) +
      ggplot2::labs(caption = "normal y axis, negative depth axis with name, non-standard breaks")
  )

  print(
    px + scale_x_age_depth(adm_ident, depth_name = "depth axis", depth_breaks = seq(0, 20, 4)) +
      ggplot2::labs(caption = "normal x axis, negative age axis with name, non-standard breaks")
  )

  print(
    py + scale_y_age_depth(NULL) +
      ggplot2::labs(caption = "normal y axis, no age axis")
  )

  print(
    py + scale_y_age_depth(NULL, reversed = TRUE) +
      ggplot2::labs(caption = "reversed y axis, no age axis")
  )

  print(
    px + scale_x_age_depth(NULL) +
      ggplot2::labs(caption = "normal x axis, no age axis")
  )

  print(
    px + scale_x_age_depth(NULL, reversed = TRUE) +
      ggplot2::labs(caption = "reversed x axis, no age axis")
  )

  # plot-generating tests
  expect_true(TRUE)
})

test_that("relative abundance scales", {

  print(
    ggplot2::ggplot(keji_lakes_plottable, ggplot2::aes(x = rel_abund, y = depth)) +
      geom_col_segsh() +
      ggplot2::scale_y_reverse() +
      ggplot2::facet_grid(location ~ taxon, scales = "free_x", space = "free_x") +
      rotated_facet_labels() +
      scale_x_abundance() +
      ggplot2::labs(caption = "zero-based x scale with minor breaks every 10%")
  )

  print(
    ggplot2::ggplot(keji_lakes_plottable, ggplot2::aes(y = rel_abund, x = depth)) +
      geom_col_segs() +
      ggplot2::scale_x_reverse() +
      ggplot2::facet_grid(taxon ~ location, scales = "free_y", space = "free_y") +
      rotated_facet_labels(direction = "y") +
      scale_y_abundance() +
      ggplot2::labs(caption = "zero-based y scale with minor breaks every 10%")
  )

  # plot-generating tests
  expect_true(TRUE)
})

test_that("horizontal and vertical segment geometries look as they should", {
  test_data <- data.frame(a = 1:20, b = cumsum(rnorm(20)))
  test_data <- dplyr::sample_n(test_data, 20, replace = FALSE)

  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x = a, y = b)) +
      geom_col_segs(yend = 1) +
      ggplot2::geom_line() +
      ggplot2::labs(caption = "vertical segments starting at 1, path geom in x order")
  )

  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x = b, y = a)) +
      geom_col_segsh(xend = 1) +
      geom_lineh() +
      ggplot2::labs(caption = "horizontal segments starting at 1, path geom in y order")
  )

  # plot-generating tests
  expect_true(TRUE)
})

test_that("facet_abundanceh? shortcuts work as expected", {

  print(
    ggplot2::ggplot(keji_lakes_plottable, ggplot2::aes(y = rel_abund, x = depth)) +
      geom_col_segs() +
      ggplot2::scale_x_reverse() +
      facet_abundance(vars(taxon), vars(location)) +
      ggplot2::labs(caption = "zero-based fixed-space y scale with minor breaks every 10%, partial italics on facets")
  )

  print(
    ggplot2::ggplot(keji_lakes_plottable, ggplot2::aes(x = rel_abund, y = depth)) +
      geom_col_segsh() +
      ggplot2::scale_y_reverse() +
      facet_abundanceh(vars(taxon), vars(location)) +
      ggplot2::labs(caption = "zero-based fixed-space x scale with minor breaks every 10%, partial italics on facets")
  )

  print(
    ggplot2::ggplot(keji_lakes_plottable, ggplot2::aes(x = rel_abund, y = depth)) +
      geom_col_segsh() +
      ggplot2::scale_y_reverse() +
      facet_abundanceh(vars(taxon), vars(location), labeller = ggplot2::label_value) +
      ggplot2::labs(caption = "zero-based fixed-space x scale with minor breaks every 10%, no italics on facets")
  )

  expect_true(TRUE)
})

test_that("Species italicizer works as planned", {
  test_data <- data.frame(
    x = 1,
    y = 1,
    species = c(
      "Thinger sp.", "Thinger spp.", "Thinger thinger",
      "Thinger thinger (nope)", "Thinger thinger-complex"
    ),
    not_species = "Contain's \"weird\" ~things "
  )

  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(vars(species, not_species), labeller = label_species) +
      ggplot2::labs(caption = "partially italicised strip 1, non-italicized strip 2")
  )

  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(cols = vars(species), rows = vars(not_species),
                          labeller = function(...) label_species(..., species_facet = "species")) +
      ggplot2::labs(caption = "partially italicised strip top, non-italicized strip right")
  )

  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(rows = vars(species), cols = vars(not_species),
                          labeller = function(...) label_species(..., species_facet = "species")) +
      ggplot2::labs(caption = "partially italicised strip right, non-italicized strip top")
  )

  expect_true(TRUE)
})

test_that("facets for geochem work as expected", {
  test_data <- data.frame(
    x = 1,
    y = 1,
    geochem = c(
      "d15N", "d13C", "d18O", "d18S", "210Pb",
      "Ca", "Pb", "C/N", 'Wierd"s things~'
    ),
    not_geochem = "Contain's \"weird\" ~things "
  )

  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      facet_geochem_wraph(
        vars(geochem),
        grouping = vars(not_geochem)
      ) +
      ggplot2::labs(caption = "label_geochem by default, facet_wrap() with rotated labs")
  )

  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      facet_geochem_wrap(
        vars(geochem),
        grouping = vars(not_geochem)
      ) +
      ggplot2::labs(caption = "label_geochem by default, facet_wrap() with no rotated labs")
  )

  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      facet_geochem_wraph(
        vars(geochem),
        units = c(
          "210Pb" = "Bq/g",
          "Ca" = "ppm",
          "Pb" = "ppm",
          "d15N" = "permille",
          "d13C" = "permille",
          "C/N" = NA
        )
      ) +
      ggplot2::labs(caption = "label_geochem by default, facet_wrap(), units as appropriate")
  )

  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      facet_geochem_grid(
        vars(geochem),
        grouping = vars(not_geochem)
      ) +
      ggplot2::labs(caption = "label_geochem by default (y direction), facet_grid()")
  )

  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      facet_geochem_gridh(
        vars(geochem),
        grouping = vars(not_geochem)
      ) +
      ggplot2::labs(caption = "label_geochem by default (x direction), facet_grid()")
  )

  expect_true(TRUE)
})

test_that("geochem labeller works as planned", {
  test_data <- data.frame(
    x = 1,
    y = 1,
    geochem = c(
      "d15N", "d13C", "d18O", "d18S", "210Pb",
      "Ca", "Pb", "C/N", 'Wierd"s things~'
    ),
    not_geochem = "Contain's \"weird\" ~things "
  )

  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(
        vars(geochem, not_geochem),
        labeller = function(...) label_geochem(
          ...,
          units = c(
            "Contain's \"weird\" ~things " = "ppm",
            'Wierd"s things~' = "ppm",
            "d15N" = "permille",
            "C/N" = NA
          ),
          default_units = "def"
        )
      ) +
      ggplot2::labs(caption = "lots of default units, 'deltas', superscript 210, ppms")
  )

  # NULL (disabled) renamer
  print(
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(
        vars(geochem, not_geochem),
        labeller = function(...) label_geochem(
          ...,
          renamers = NULL
        )
      ) +
      ggplot2::labs(caption = "lots of default units, 'deltas', superscript 210, ppms")
  )


  expect_true(TRUE)
})

test_that("default args for facet_* functions are consistent", {
  expect_identical(formals(facet_abundance)$dont_italicize, formals(facet_abundanceh)$dont_italicize)
  expect_identical(formals(facet_abundance)$dont_italicize, formals(label_species)$dont_italicize)

  expect_identical(formals(facet_geochem_wraph)$scales, formals(facet_geochem_gridh)$scales)
  expect_identical(formals(facet_geochem_wraph)$renamers, formals(facet_geochem_gridh)$renamers)
  expect_identical(formals(facet_geochem_wraph)$units, formals(facet_geochem_gridh)$units)
  expect_identical(formals(facet_geochem_wraph)$default_units, formals(facet_geochem_gridh)$default_units)

  expect_identical(formals(facet_geochem_wraph)$renamers, formals(facet_geochem_grid)$renamers)
  expect_identical(formals(facet_geochem_wraph)$units, formals(facet_geochem_grid)$units)
  expect_identical(formals(facet_geochem_wraph)$default_units, formals(facet_geochem_grid)$default_units)

  expect_identical(formals(facet_geochem_wraph)$renamers, formals(label_geochem)$renamers)
  expect_identical(formals(facet_geochem_wraph)$units, formals(label_geochem)$units)
  expect_identical(formals(facet_geochem_wraph)$default_units, formals(label_geochem)$default_units)
})
