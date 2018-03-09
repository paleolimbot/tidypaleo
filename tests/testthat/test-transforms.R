context("transforms")

test_that("approx transform works as expected", {
  test_depth <- c(0, 10, 15)
  test_age <- c(100, 90, 80)

  test_trans <- trans_interpolate(test_depth, test_age)
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
  test_trans <- trans_average(test_depth, test_age, x0 = 4256, y0 = 1272)
  expect_identical(test_trans$trans(4256), 1272)
  expect_identical(test_trans$inverse(1272), 4256)

  # function anchor
  test_trans2 <- trans_average(c(-1, test_depth), c(200, test_age),
                          x0 = dplyr::first, y0 = dplyr::first)
  expect_identical(test_trans2$trans(-1), 200)
  expect_identical(test_trans2$inverse(200), -1)

  test_trans3 <- trans_average(c(test_depth, 16), c(test_age, 0),
                          x0 = dplyr::last, y0 = dplyr::last)
  expect_identical(test_trans3$trans(16), 0)
  expect_identical(test_trans3$inverse(0), 16)

  # null anchor
  test_trans4 <- trans_average(test_depth, test_age, x0 = NULL, y0 = NULL)
  expect_identical(test_trans4$trans(20), 80)
  expect_identical(test_trans4$inverse(80), 20)

  # override slope
  test_trans5 <- trans_average(test_depth, test_age, x0 = 0, y0 = 100, slope = -2)
  expect_identical(test_trans5$trans(20), 60)
  expect_identical(test_trans5$inverse(60), 20)
})

test_that("trans_exact works as expected", {
  test_depth <- c(0, 10, 15)
  test_age <- c(100, 90, 80)

  test_trans <- trans_exact(test_depth, test_age)
  expect_identical(test_trans$trans(test_depth), test_age)
  expect_identical(test_trans$inverse(test_age), test_depth)
  expect_identical(test_trans$trans(5), NA_real_)
  expect_identical(test_trans$inverse(95), NA_real_)
  expect_identical(test_trans$trans(12.5), NA_real_)
  expect_identical(test_trans$inverse(85), NA_real_)
})

test_that("trans_na works as expected", {
  test_depth <- c(0, 10, 15)
  test_age <- c(100, 90, 80)

  test_trans <- trans_na(test_depth, test_age)
  expect_identical(test_trans$trans(test_depth), c(NA_real_, NA_real_, NA_real_))
  expect_identical(test_trans$inverse(test_age), c(NA_real_, NA_real_, NA_real_))
})

test_that("trans factory tester works with all included trans factories", {
  expect_identical(trans_interpolate, validate_trans_factory(trans_interpolate))
  expect_identical(trans_average, validate_trans_factory(trans_average))
  expect_identical(trans_exact, validate_trans_factory(trans_exact))
  expect_identical(trans_na, validate_trans_factory(trans_na))
})

test_that("as_trans_factory works with included trans factories", {
  expect_identical(trans_interpolate, as_trans_factory(trans_interpolate))
  expect_identical(trans_average, as_trans_factory(trans_average))
  expect_identical(trans_exact, as_trans_factory(trans_exact))
  expect_identical(trans_na, as_trans_factory(trans_na))
})

test_that("as_trans_factory works with rlang lambda functions", {
  test_depth <- c(0, 10, 15)
  test_age <- c(100, 90, 80)

  factory <- as_trans_factory(~trans_interpolate(.x, .y))
  expect_equal(
    factory(test_depth, test_age),
    trans_interpolate(test_depth, test_age)
  )

  # check passing of objects from calling environment
  first_fun <- dplyr::first
  last_fun <- dplyr::last
  factory2 <- as_trans_factory(~trans_average(.x, .y, x0 = first_fun, y0 = last_fun))
  expect_equal(
    factory2(test_depth, test_age),
    trans_average(test_depth, test_age, x0 = first_fun, y0 = last_fun)
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
  expect_error(trans_interpolate(1, 1), "greater than or equal to 2")
  expect_error(trans_interpolate(1, 1:2), "must be equal to length")
  expect_error(trans_average(1, 1), "greater than or equal to 2")
  expect_error(trans_average(1, 1:2), "must be equal to length")
  expect_error(trans_exact(numeric(0), numeric(0)), "must be greater than or equal to 1")
  expect_error(trans_exact(1, 1:2), "must be equal to length")
})

test_that("args to trans_average are checked", {
  expect_error(trans_average(1:3, 1:3, x0 = "nope"), "or a scalar numeric")
  expect_error(trans_average(1:3, 1:3, y0 = "nope"), "or a scalar numeric")
  expect_error(trans_average(1:3, 1:3, slope = "nope"), "slope must be a numeric scalar")
})
