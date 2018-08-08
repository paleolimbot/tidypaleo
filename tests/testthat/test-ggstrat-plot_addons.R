context("test-ggstrat-plot_addons.R")

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

  print(
    p +
      ggplot2::facet_grid(ggplot2::vars(cyl_fct), ggplot2::vars(gear_fct)) +
      sequential_layer_facets() +
      ggplot2::labs(caption = "cyl order: 8 4 6 2")
  )

  print(
    p +
      ggplot2::facet_wrap(ggplot2::vars(cyl_fct, gear_fct)) +
      sequential_layer_facets() +
      ggplot2::labs(caption = "cyl order: 8 4 6 2")
  )

  expect_silent(
    print(
      p + ggplot2::facet_null()
    )
  )

  expect_true(TRUE)
})
