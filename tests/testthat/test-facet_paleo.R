context("test-facet_paleo.R")

test_that("removing facet cliping works according to plan", {
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
      facet2 = c("a very very very very long facet name 1"),
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

  # facet_wrap()

  print(
    p +
      ggplot2::facet_wrap(~facet, nrow = 1) +
      remove_label_clip(NULL) +
      ggplot2::labs(caption = "facet_wrap with cut-off labels")
  )

  print(
    p +
      ggplot2::facet_wrap(~facet, nrow = 1, strip.position = "bottom") +
      remove_label_clip(NULL) +
      ggplot2::labs(caption = "facet_wrap with cut-off labels")
  )

  print(
    p +
      ggplot2::facet_wrap(~facet, nrow = 1, strip.position = "bottom") +
      remove_label_clip("x") +
      ggplot2::labs(caption = "facet_wrap with unclipped labels")
  )

  print(
    p +
      ggplot2::facet_wrap(~facet, nrow = 1) +
      remove_label_clip("t") +
      ggplot2::labs(caption = "facet_wrap with unclipped labels")
  )

  print(
    p +
      ggplot2::facet_wrap(~facet, nrow = 1, strip.position = "bottom") +
      remove_label_clip("x") +
      ggplot2::labs(caption = "facet_wrap with unclipped labels")
  )

  print(
    p +
      ggplot2::facet_wrap(~facet, nrow = 1, strip.position = "bottom") +
      remove_label_clip("b") +
      ggplot2::labs(caption = "facet_wrap with unclipped labels")
  )

  # facet_grid()

  print(
    p +
      ggplot2::facet_grid(facet2~facet) +
      remove_label_clip(NULL) +
      ggplot2::labs(caption = "facet_grid with cut-off labels in all directions")
  )

  print(
    p +
      ggplot2::facet_grid(facet2~facet) +
      remove_label_clip(c("x", "y")) +
      ggplot2::labs(caption = "facet_grid with no cut-off labels")
  )

  print(
    p +
      ggplot2::facet_grid(facet2~facet) +
      remove_label_clip( c("t", "r")) +
      ggplot2::labs(caption = "facet_grid with no cut-off labels")
  )

  print(
    p +
      ggplot2::facet_grid(facet2~facet, switch = "both") +
      remove_label_clip( c("b", "l")) +
      ggplot2::labs(caption = "facet_grid with no cut-off labels")
  )

  # shouldn't work on a FacetNull
  expect_error(p + remove_label_clip(NULL), "current facet")

  # only visual tests
  expect_true(TRUE)
})

test_that("label rotating works properly", {
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
      facet2 = c("a very very very very long facet name 1"),
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
      rotate_facet_labels(45, direction = c("x", "y")) +
      ggplot2::labs(caption = "x and y labels rotated by 45 degrees with no strip background")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotate_facet_labels(45) +
      ggplot2::labs(caption = "only x rotated by 45 degrees (default) with no strip background")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotate_facet_labels(45, direction = "y") +
      ggplot2::labs(caption = "only y rotated by 45 degrees with no strip background")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotate_facet_labels(-45, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by -45 degrees in both directions (text base should align to top/right of plot)")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotate_facet_labels(0, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by 0 degrees in both directions (text should align to middle)")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotate_facet_labels(90, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by 90 degrees in both directions (text should align to middle)")
  )

  print(
    p +
      ggplot2::facet_grid(facet2 ~ facet) +
      rotate_facet_labels(-90, direction = c("x", "y")) +
      ggplot2::labs(caption = "labels rotated by -90 degrees in both directions (text should align to middle)")
  )

  expect_true(TRUE)
})
