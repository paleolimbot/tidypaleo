context("test-sec_axis.R")

test_that("creating a second axis object", {
  alta_lake_adm <- age_depth_model(
    alta_lake_210Pb_ages, depth = depth_cm, age = age_year_ad
  )

  # depth as the primary axis
  expect_true(ggplot2::is.ggproto(as_sec_axis(alta_lake_adm)))
  expect_is(as_sec_axis(alta_lake_adm), "AxisSecondary")

  expect_silent(
    ggplot2::ggplot(alta_lake_210Pb_ages, ggplot2::aes(y = depth_cm, x = age_year_ad)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_y_reverse(sec.axis = as_sec_axis(alta_lake_adm))
  )

  # age as the primary axis
  expect_true(ggplot2::is.ggproto(as_sec_axis(alta_lake_adm, primary = "age")))
  expect_is(as_sec_axis(alta_lake_adm, primary = "age"), "AxisSecondary")

  expect_silent(
    ggplot2::ggplot(alta_lake_210Pb_ages, ggplot2::aes(y = depth_cm, x = age_year_ad)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_y_reverse() +
      ggplot2::scale_x_continuous(sec.axis = as_sec_axis(alta_lake_adm, primary = "age"))
  )

})
