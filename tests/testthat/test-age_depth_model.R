context("test-age_depth_model.R")

test_that("age_depth_model() creates valid age depth model objects", {
  test_data <- data.frame(depth_col = 0:5, age_col = 2000:1995)
  adm <- age_depth_model(test_data, depth = depth_col, age = age_col)
  expect_is(adm, "age_depth_model")
  expect_true(is_age_depth_model(adm))
  expect_identical(validate_age_depth_model(adm), adm)
})

test_that("data is optional in constructor", {
  test_data <- data.frame(depth_col = 0:5, age_col = 2000:1995)
  expect_identical(
    age_depth_model(test_data, depth = depth_col, age = age_col)$data,
    age_depth_model(depth = 0:5, age = 2000:1995)$data
  )
})

test_that("NULL or missing arguments throw an error", {
  test_data <- data.frame(depth_col = 0:5, age_col = 2000:1995)
  expect_error(age_depth_model(), "depth is a required argument")
  expect_error(age_depth_model(depth = 0:5), "age is a required argument")
  expect_error(age_depth_model(depth = NULL, age = 2000:1995), "must all be non-NULL")
  expect_error(age_depth_model(depth = 0:5, age = NULL), "must all be non-NULL")
})

test_that("non-finite age/depth values are handled appropriately", {
  expect_error(age_depth_model(depth = c(NA, 0:5), age = c(2000:1995, NA)), "Non-finite values")
  expect_warning(
    age_depth_model(depth = c(NA, 0:5), age = c(2000:1995, NA), interpolate_age = age_depth_na),
    "Non-finite values"
  )
})

test_that("age depth predictions are accurate", {
  test_data <- data.frame(depth_col = 0:5, age_col = 2000:1995)

  # the default will always be to interpolate within the depth range, and extrapolate outside it
  # using average rates
  adm <- age_depth_model(test_data, depth = depth_col, age = age_col)

  expect_equal(predict(adm, depth = 0:5)$age, 2000:1995) # interpolated
  expect_equal(predict(adm, age = 1995:2000)$depth, 5:0) # inverse interpolated
  expect_equal(predict(adm, depth = 6:10)$age, 1994:1990) # extrapolate below
  expect_equal(predict(adm, depth = -5:-1)$age, 2005:2001) # extrapolate above
  expect_equal(predict(adm, age = 1990:1994)$depth, 10:6) # inverse extrapolate below
  expect_equal(predict(adm, age = 2001:2005)$depth, -1:-5) # inverse extrapolate above

  # check methods
  expect_equal(predict(adm, depth = 2.5)$method, "interpolate")
  expect_equal(predict(adm, age = 1997.5)$method, "inverse_interpolate")
  expect_equal(predict(adm, depth = -1)$method, "extrapolate_above")
  expect_equal(predict(adm, depth = 6)$method, "extrapolate_below")
  expect_equal(predict(adm, age = 2001)$method, "inverse_extrapolate_above")
  expect_equal(predict(adm, age = 1994)$method, "inverse_extrapolate_below")

  # this adm uses increasing age with depth, which has a slightly different reverse
  adm_increase <- age_depth_model(data.frame(depth_col = 0:5, age_col = 0:5),
                                  depth = depth_col, age = age_col)
  expect_equal(predict(adm_increase, depth = 0:5)$age, as.numeric(0:5)) # interpolated
  expect_equal(predict(adm_increase, age = 5:0)$depth, as.numeric(5:0)) # inverse interpolated
  expect_equal(predict(adm_increase, depth = 6:10)$age, 6:10) # extrapolate below
  expect_equal(predict(adm_increase, depth = -5:-1)$age, -5:-1) # extrapolate above
  expect_equal(predict(adm_increase, age = 10:6)$depth, 10:6) # inverse extrapolate below
  expect_equal(predict(adm_increase, age = -1:-5)$depth, -1:-5) # inverse extrapolate above

  # check methods
  expect_equal(predict(adm_increase, depth = 2.5)$method, "interpolate")
  expect_equal(predict(adm_increase, age = 2.5)$method, "inverse_interpolate")
  expect_equal(predict(adm_increase, depth = -1)$method, "extrapolate_above")
  expect_equal(predict(adm_increase, depth = 6)$method, "extrapolate_below")
  expect_equal(predict(adm_increase, age = -1)$method, "inverse_extrapolate_above")
  expect_equal(predict(adm_increase, age = 6)$method, "inverse_extrapolate_below")
})

test_that("predict works with newdata and straight args", {
  test_data <- data.frame(depth_col = 0:5, age_col = 2000:1995)
  adm <- age_depth_model(test_data, depth = depth_col, age = age_col)

  expect_identical(
    predict(adm, depth = 0:5) %>% dplyr::select(-depth),
    predict(adm, tibble::tibble(depth = 0:5), depth = depth)
  )
})

test_that("predict shortcuts work as expected", {
  adm <- age_depth_model(depth = 0:5, age = 2000:1995)
  expect_equal(predict_age(adm, 0:5), 2000:1995)
  expect_equal(predict_depth(adm, 1995:2000), 5:0)
})

test_that("predict throws an error when no input is specified", {
  test_data <- data.frame(depth_col = 0:5, age_col = 2000:1995)
  adm <- age_depth_model(test_data, depth = depth_col, age = age_col)

  expect_error(predict(adm, age = NULL, depth = NULL), "One of depth or age must be NULL")
  expect_error(
    predict(adm, newdata = tibble::tibble(dummy = 1), age = NULL, depth = NULL),
    "One of depth or age must be NULL"
  )
})

test_that("print outputs things", {
  expect_output(print(age_depth_model(depth = 0:5, age = 2000: 1995)), "<age_depth_model>")
})

test_that("base graphics plotting renders properly", {

  test_data <- tibble::tibble(
    depth_col = 0:5,
    age_col = 2000:1995 - depth_col ^ 1.5,
    err = depth_col ^ 1.5
  )

  # should only have error bars
  adm <- age_depth_model(
    test_data, depth = depth_col, age = age_col,
    age_min = age_col - err, age_max = age_col + err
  )

  vdiffr::expect_doppelganger("only error bars", function() plot(adm))

  # should have dotted lines for interpolated error
  adm_cont_err <- age_depth_model(
    test_data, depth = depth_col, age = age_col,
    age_min = age_col - err, age_max = age_col + err,
    interpolate_age_limits = age_depth_interpolate
  )

  vdiffr::expect_doppelganger("continuous error", function() plot(adm_cont_err))

  # should have dotted lines for extrapolated below error
  adm_cont_err_below <- age_depth_model(
    test_data, depth = depth_col, age = age_col,
    age_min = age_col - err, age_max = age_col + err,
    interpolate_age_limits = age_depth_interpolate,
    extrapolate_age_limits_below = age_depth_extrapolate
  )

  vdiffr::expect_doppelganger("continuous error below", function() plot(adm_cont_err_below))
})

test_that("invalid age depth model objects are detected", {
  expect_error(
    validate_age_depth_model(structure(list())),
    "objects of class age_depth_model must have components"
  )
  expect_error(
    validate_age_depth_model(structure(list(
      call_label = 4, trans = list(), trans_factories = list(), data = tibble::tibble()
    ))),
    "character vector of length 1"
  )
})


test_that("creating a second axis object", {
  alta_lake_adm <- age_depth_model(
    alta_lake_210Pb_ages, depth = depth_cm, age = age_year_ad
  )

  # depth as the primary axis
  expect_true(ggplot2::is.ggproto(age_depth_as_sec_axis(alta_lake_adm)))
  expect_is(age_depth_as_sec_axis(alta_lake_adm), "AxisSecondary")

  expect_silent(
    ggplot2::ggplot(alta_lake_210Pb_ages, ggplot2::aes(y = depth_cm, x = age_year_ad)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_y_reverse(sec.axis = age_depth_as_sec_axis(alta_lake_adm))
  )

  # age as the primary axis
  expect_true(ggplot2::is.ggproto(age_depth_as_sec_axis(alta_lake_adm, primary = "age")))
  expect_is(age_depth_as_sec_axis(alta_lake_adm, primary = "age"), "AxisSecondary")

  expect_silent(
    ggplot2::ggplot(alta_lake_210Pb_ages, ggplot2::aes(y = depth_cm, x = age_year_ad)) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::scale_y_reverse() +
      ggplot2::scale_x_continuous(sec.axis = age_depth_as_sec_axis(alta_lake_adm, primary = "age"))
  )

})


test_that("approx transform works as expected", {
  test_depth <- c(0, 10, 15)
  test_age <- c(100, 90, 80)

  test_trans <- age_depth_interpolate(test_depth, test_age)
  expect_identical(test_trans$trans(test_depth), test_age)
  expect_identical(test_trans$inverse(test_age), test_depth)
  expect_identical(test_trans$trans(5), 95)
  expect_identical(test_trans$inverse(95), 5)
  expect_identical(test_trans$trans(12.5), 85)
  expect_identical(test_trans$inverse(85), 12.5)

  expect_identical(test_trans$trans(-1), NA_real_)
  expect_identical(test_trans$trans(16), NA_real_)
  expect_identical(test_trans$inverse(79), NA_real_)
  expect_identical(test_trans$inverse(101), NA_real_)
})

test_that("lm transform anchors work as expected", {
  test_depth <- c(0, 10, 15)
  test_age <- c(100, 90, 85)

  # number anchor
  test_trans <- age_depth_extrapolate(test_depth, test_age, x0 = 4256, y0 = 1272)
  expect_identical(test_trans$trans(4256), 1272)
  expect_identical(test_trans$inverse(1272), 4256)

  # function anchor
  test_trans2 <- age_depth_extrapolate(c(-1, test_depth), c(200, test_age),
                               x0 = dplyr::first, y0 = dplyr::first)
  expect_identical(test_trans2$trans(-1), 200)
  expect_identical(test_trans2$inverse(200), -1)

  test_trans3 <- age_depth_extrapolate(c(test_depth, 16), c(test_age, 0),
                               x0 = dplyr::last, y0 = dplyr::last)
  expect_identical(test_trans3$trans(16), 0)
  expect_identical(test_trans3$inverse(0), 16)

  # null anchor
  # (equal because identical fails on M1 mac)
  test_trans4 <- age_depth_extrapolate(test_depth, test_age, x0 = NULL, y0 = NULL)
  expect_equal(test_trans4$trans(20), 80)
  expect_equal(test_trans4$inverse(80), 20)

  # override slope
  test_trans5 <- age_depth_extrapolate(test_depth, test_age, x0 = 0, y0 = 100, slope = -2)
  expect_identical(test_trans5$trans(20), 60)
  expect_identical(test_trans5$inverse(60), 20)
})

test_that("age_depth_exact works as expected", {
  test_depth <- c(0, 10, 15)
  test_age <- c(100, 90, 80)

  test_trans <- age_depth_exact(test_depth, test_age)
  expect_identical(test_trans$trans(test_depth), test_age)
  expect_identical(test_trans$inverse(test_age), test_depth)
  expect_identical(test_trans$trans(5), NA_real_)
  expect_identical(test_trans$inverse(95), NA_real_)
  expect_identical(test_trans$trans(12.5), NA_real_)
  expect_identical(test_trans$inverse(85), NA_real_)
})

test_that("age_depth_na works as expected", {
  test_depth <- c(0, 10, 15)
  test_age <- c(100, 90, 80)

  test_trans <- age_depth_na(test_depth, test_age)
  expect_identical(test_trans$trans(test_depth), c(NA_real_, NA_real_, NA_real_))
  expect_identical(test_trans$inverse(test_age), c(NA_real_, NA_real_, NA_real_))
})

test_that("trans factory tester works with all included trans factories", {
  expect_identical(age_depth_interpolate, validate_trans_factory(age_depth_interpolate))
  expect_identical(age_depth_extrapolate, validate_trans_factory(age_depth_extrapolate))
  expect_identical(age_depth_exact, validate_trans_factory(age_depth_exact))
  expect_identical(age_depth_na, validate_trans_factory(age_depth_na))
})

test_that("as_trans_factory works with included trans factories", {
  expect_identical(age_depth_interpolate, as_trans_factory(age_depth_interpolate))
  expect_identical(age_depth_extrapolate, as_trans_factory(age_depth_extrapolate))
  expect_identical(age_depth_exact, as_trans_factory(age_depth_exact))
  expect_identical(age_depth_na, as_trans_factory(age_depth_na))
})

test_that("as_trans_factory works with rlang lambda functions", {
  test_depth <- c(0, 10, 15)
  test_age <- c(100, 90, 80)

  factory <- as_trans_factory(~age_depth_interpolate(.x, .y))
  expect_equal(
    factory(test_depth, test_age),
    age_depth_interpolate(test_depth, test_age)
  )

  # check passing of objects from calling environment
  first_fun <- dplyr::first
  last_fun <- dplyr::last
  factory2 <- as_trans_factory(~age_depth_extrapolate(.x, .y, x0 = first_fun, y0 = last_fun))
  expect_equal(
    factory2(test_depth, test_age),
    age_depth_extrapolate(test_depth, test_age, x0 = first_fun, y0 = last_fun)
  )
})

test_that("invalid trans factories are detected by validate_trans_factory", {
  expect_error(validate_trans_factory(NULL), "transform factory must be a function")
})

test_that("invalid trans objects are detected by validate_trans", {
  expect_error(validate_trans(NULL), "trans must be a list")
  expect_error(validate_trans(list()), "trans must have trans and inverse components")
  expect_error(validate_trans(list(trans = NULL, inverse = NULL)), "trans must be a function")
  expect_error(validate_trans(list(trans = identity, inverse = NULL)), "inverse must be a function")
  expect_error(validate_trans(list(
    trans = mean, inverse = identity)
  ), "non-vectorized or non-numeric result")
  expect_error(validate_trans(list(
    trans = identity, inverse = mean)
  ), "non-vectorized or non-numeric result")
  expect_error(validate_trans(list(
    trans = stop, inverse = identity)
  ), "test of trans\\$trans.*?failed")
  expect_error(validate_trans(list(
    trans = identity, inverse = stop)
  ), "test of trans\\$inverse.*?failed")
  expect_silent(validate_trans(list(trans = identity, inverse = identity)))
})

test_that("trans objects can't be created with too few observations", {
  expect_error(age_depth_interpolate(1, 1), "greater than or equal to 2")
  expect_error(age_depth_interpolate(1, 1:2), "must be equal to length")
  expect_error(age_depth_extrapolate(1, 1), "greater than or equal to 2")
  expect_error(age_depth_extrapolate(1, 1:2), "must be equal to length")
  expect_error(age_depth_exact(numeric(0), numeric(0)), "must be greater than or equal to 1")
  expect_error(age_depth_exact(1, 1:2), "must be equal to length")
})

test_that("args to age_depth_extrapolate are checked", {
  expect_error(age_depth_extrapolate(1:3, 1:3, x0 = "nope"), "or a scalar numeric")
  expect_error(age_depth_extrapolate(1:3, 1:3, y0 = "nope"), "or a scalar numeric")
  expect_error(age_depth_extrapolate(1:3, 1:3, slope = "nope"), "slope must be a numeric scalar")
})
