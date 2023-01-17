
test_that("facet label rotating works properly", {

  test_data <- withr::with_seed(238,{
    rbind(
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
  })

  p <- ggplot2::ggplot(test_data, ggplot2::aes(x, y)) + ggplot2::geom_point()

  vdiffr::expect_doppelganger(
    "label rotation",
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(45, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation switch",
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(45, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation x",
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(45)
  )

  vdiffr::expect_doppelganger(
    "label rotation switch x",
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(45, direction = "x")
  )

  vdiffr::expect_doppelganger(
    "label rotation y",
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(45, direction = "y")
  )

  vdiffr::expect_doppelganger(
    "label rotation switch y",
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(45, direction = "y")
  )

  vdiffr::expect_doppelganger(
    "label rotation neg",
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(-45, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation neg switch",
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(-45, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation 0",
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(0, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation 0 switch",
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(0, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation 90",
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(90, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation 90 switch",
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(90, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation neg90",
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotated_facet_labels(-90, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation neg90 switch",
    p +
      ggplot2::facet_grid(facet2 ~ facet, switch = "both") +
      rotated_facet_labels(-90, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation (wrap)",
    p +
      ggplot2::facet_wrap(~facet, strip.position = "top") +
      rotated_facet_labels(45, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation (wrap, left)",
    p +
      ggplot2::facet_wrap(~facet, strip.position = "left") +
      rotated_facet_labels(45, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation (wrap, bottom)",
    p +
      ggplot2::facet_wrap(~facet, strip.position = "bottom") +
      rotated_facet_labels(45, direction = c("x", "y"))
  )

  vdiffr::expect_doppelganger(
    "label rotation (wrap, right)",
    p +
      ggplot2::facet_wrap(~facet, strip.position = "right") +
      rotated_facet_labels(45, direction = c("x", "y"))
  )
})

test_that("age depth axes work as expected", {

  adm_ident <- age_depth_model(depth = 1:10, age = -(1:10))
  test_data <- withr::with_seed(394, {
    data.frame(depth = 1:20, age = -(1:20), value = cumsum(rnorm(20)))
  })
  py <- ggplot2::ggplot(test_data, ggplot2::aes(y = depth, x = value)) + ggplot2::geom_path()
  px <- ggplot2::ggplot(test_data, ggplot2::aes(y = value, x = depth)) + ggplot2::geom_path()

  vdiffr::expect_doppelganger(
    "adm rev yaxis depth age",
    py + scale_y_depth_age(adm_ident, age_name = "age axis", age_breaks = seq(0, -20, -4))
  )

  vdiffr::expect_doppelganger(
    "adm rev xaxis depth age",
    px + scale_x_depth_age(adm_ident, age_name = "age axis", age_breaks = seq(0, -20, -4))
  )

  vdiffr::expect_doppelganger(
    "adm null yaxis depth age",
    py + scale_y_depth_age(NULL)
  )

  vdiffr::expect_doppelganger(
    "adm null xaxis depth age",
    px + scale_x_depth_age(NULL)
  )

  py <- ggplot2::ggplot(test_data, ggplot2::aes(y = age, x = value)) + ggplot2::geom_path()
  px <- ggplot2::ggplot(test_data, ggplot2::aes(y = value, x = age)) + ggplot2::geom_path()

  vdiffr::expect_doppelganger(
    "adm yaxis age depth",
    py + scale_y_age_depth(adm_ident, depth_name = "depth axis", depth_breaks = seq(0, 20, 4))
  )

  vdiffr::expect_doppelganger(
    "adm xaxis age depth",
    px + scale_x_age_depth(adm_ident, depth_name = "depth axis", depth_breaks = seq(0, 20, 4))
  )

  vdiffr::expect_doppelganger(
    "adm null yaxis age depth",
    py + scale_y_age_depth(NULL)
  )

  vdiffr::expect_doppelganger(
    "adm null rev yaxis age depth",
    py + scale_y_age_depth(NULL, reversed = TRUE)
  )

  vdiffr::expect_doppelganger(
    "adm null xaxis age depth",
    px + scale_x_age_depth(NULL)
  )

  vdiffr::expect_doppelganger(
    "adm null rev xaxis age depth",
    px + scale_x_age_depth(NULL, reversed = TRUE)
  )
})

test_that("relative abundance scales", {

  vdiffr::expect_doppelganger(
    "rel abund x",
    ggplot2::ggplot(keji_lakes_plottable, ggplot2::aes(x = rel_abund, y = depth)) +
      geom_col_segsh() +
      ggplot2::scale_y_reverse() +
      ggplot2::facet_grid(location ~ taxon, scales = "free_x", space = "free_x") +
      rotated_facet_labels() +
      scale_x_abundance()
  )

  vdiffr::expect_doppelganger(
    "rel abund y",
    ggplot2::ggplot(keji_lakes_plottable, ggplot2::aes(y = rel_abund, x = depth)) +
      geom_col_segs() +
      ggplot2::scale_x_reverse() +
      ggplot2::facet_grid(taxon ~ location, scales = "free_y", space = "free_y") +
      rotated_facet_labels(direction = "y") +
      scale_y_abundance()
  )
})

test_that("horizontal and vertical segment geometries look as they should", {
  test_data <- withr::with_seed(202, data.frame(a = 1:20, b = cumsum(rnorm(20))))
  test_data <- withr::with_seed(203, dplyr::sample_n(test_data, 20, replace = FALSE))

  vdiffr::expect_doppelganger(
    "vertical col segs",
    ggplot2::ggplot(test_data, ggplot2::aes(x = a, y = b)) +
      geom_col_segs(yend = 1) +
      ggplot2::geom_line()
  )

  vdiffr::expect_doppelganger(
    "horizontal col segs",
    ggplot2::ggplot(test_data, ggplot2::aes(x = b, y = a)) +
      geom_col_segsh(xend = 1) +
      geom_lineh()
  )
})

test_that("horizontal area and ribbon plots work as expected", {
  huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron), level_half = as.vector(LakeHuron) / 2)
  h <- ggplot2::ggplot(huron, ggplot2::aes(y = year))

  vdiffr::expect_doppelganger(
    "horizontal ribbon",
    h + geom_ribbonh(ggplot2::aes(xmin = 0, xmax = level))
  )

  vdiffr::expect_doppelganger(
    "horizontal area",
    h + geom_areah(ggplot2::aes(x = level))
  )

  vdiffr::expect_doppelganger(
    "horizontal ribbon with aes",
    # Add aesthetic mappings
    h +
      geom_ribbonh(ggplot2::aes(xmin = level - 1, xmax = level + 1), fill = "grey70") +
      geom_lineh(ggplot2::aes(x = level))
  )

  vdiffr::expect_doppelganger(
    "horizontal area (stacked)",
    # horizontal stacking by default
    ggplot2::ggplot(
      rbind(
        data.frame(thing = "a", year = 1875:1972, level = as.vector(LakeHuron), stringsAsFactors = FALSE),
        data.frame(thing = "b", year = 1875:1972, level = as.vector(LakeHuron), stringsAsFactors = FALSE)
      ),
      ggplot2::aes(y = year, x = level, fill = thing)
    ) +
      geom_areah()
  )
})

test_that("exaggerated geometries work", {

  p <- ggplot2::ggplot(
    withr::with_seed(23, data.frame(x = cumsum(runif(100)), y = cumsum(rnorm(100)))),
    ggplot2::aes(x, y)
  ) +
    ggplot2::geom_line()

  vdiffr::expect_doppelganger(
    "exaggerate (point align)",
    p +
      geom_point_exaggerate(exaggerate_y = 2, exaggerate_x = 1.5, col = "red", size = 2) +
      ggplot2::geom_point(ggplot2::aes(x = x * 1.5, y = y * 2), size = 0.5)
  )

  vdiffr::expect_doppelganger(
    "exaggerate (point)",
    p +
      geom_point_exaggerate(exaggerate_y = 2, exaggerate_x = 1.5, col = "red", size = 2)
  )

  # regular geoms
  withr::with_envvar(list(VDIFFR_RUN_TESTS = FALSE), {
    vdiffr::expect_doppelganger(
      "exaggerate (point, line area)",
      p +
        geom_point_exaggerate(exaggerate_y = 2, alpha = 0.3, col = "red") +
        geom_line_exaggerate(exaggerate_y = 2, alpha = 0.3, col = "red") +
        geom_area_exaggerate(exaggerate_y = 2, alpha = 0.3, fill = "red")
    )
  })

  # flipped geoms
  p2 <- ggplot2::ggplot(
    withr::with_seed(21, data.frame(x = cumsum(runif(100)), y = cumsum(rnorm(100)))),
    ggplot2::aes(y, x)
  ) +
    geom_lineh()

  vdiffr::expect_doppelganger(
    "exaggerate (point, lineh, areah)",
    p2 +
      geom_point_exaggerate(exaggerate_x = 2, alpha = 0.3, col = "red") +
      geom_lineh_exaggerate(exaggerate_x = 2, alpha = 0.3, col = "red") +
      geom_areah_exaggerate(exaggerate_x = 2, alpha = 0.3, fill = "red")
  )
})

test_that("facet_abundanceh? shortcuts work as expected", {

  vdiffr::expect_doppelganger(
    "facet_abundance",
    ggplot2::ggplot(keji_lakes_plottable, ggplot2::aes(y = rel_abund, x = depth)) +
      geom_col_segs() +
      ggplot2::scale_x_reverse() +
      facet_abundance(vars(taxon), vars(location))
  )

  vdiffr::expect_doppelganger(
    "facet_abundanceh",
    ggplot2::ggplot(keji_lakes_plottable, ggplot2::aes(x = rel_abund, y = depth)) +
      geom_col_segsh() +
      ggplot2::scale_y_reverse() +
      facet_abundanceh(vars(taxon), vars(location))
  )

  vdiffr::expect_doppelganger(
    "facet_abundanceh, value labeller",
    ggplot2::ggplot(keji_lakes_plottable, ggplot2::aes(x = rel_abund, y = depth)) +
      geom_col_segsh() +
      ggplot2::scale_y_reverse() +
      facet_abundanceh(vars(taxon), vars(location), labeller = ggplot2::label_value)
  )
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

  vdiffr::expect_doppelganger(
    "partial italics",
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(vars(species, not_species), labeller = label_species)
  )

  vdiffr::expect_doppelganger(
    "partial italics multi facet",
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(cols = vars(species), rows = vars(not_species),
                          labeller = function(...) label_species(..., species_facet = "species"))
  )

  vdiffr::expect_doppelganger(
    "partial italics mult facet 2",
    ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(rows = vars(species), cols = vars(not_species),
                          labeller = function(...) label_species(..., species_facet = "species"))
  )
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

  # skip("parsed label tests do not render identically between vdiffrAddin() and CMD check")
  withr::with_envvar(list(VDIFFR_RUN_TESTS = FALSE), {
    vdiffr::expect_doppelganger(
      "facet_geochem_wraph",
      ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
        ggplot2::geom_point() +
        facet_geochem_wraph(
          vars(geochem),
          grouping = vars(not_geochem)
        )
    )

    vdiffr::expect_doppelganger(
      "facet_geochem_wrap",
      ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
        ggplot2::geom_point() +
        facet_geochem_wrap(
          vars(geochem),
          grouping = vars(not_geochem)
        )
    )

    vdiffr::expect_doppelganger(
      "facet_geochem_wraph, units",
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
        )
    )

    vdiffr::expect_doppelganger(
      "facet_geochem_grid",
      ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
        ggplot2::geom_point() +
        facet_geochem_grid(
          vars(geochem),
          grouping = vars(not_geochem)
        )
    )

    vdiffr::expect_doppelganger(
      "facet_geochem_gridh",
      ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
        ggplot2::geom_point() +
        facet_geochem_gridh(
          vars(geochem),
          grouping = vars(not_geochem)
        )
    )
  })
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

  # skip("parsed label tests do not render identically between vdiffrAddin() and CMD check")
  withr::with_envvar(list(VDIFFR_RUN_TESTS = FALSE), {
    vdiffr::expect_doppelganger(
      "label_geochem",
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
        )
    )

    # NULL (disabled) renamer
    vdiffr::expect_doppelganger(
      "label_geochem (no renamer)",
      ggplot2::ggplot(test_data, ggplot2::aes(x, y)) +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(
          vars(geochem, not_geochem),
          labeller = function(...) label_geochem(
            ...,
            renamers = NULL
          )
        )
    )
  })
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
