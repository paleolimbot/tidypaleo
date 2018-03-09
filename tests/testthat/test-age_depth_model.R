context("test-age_depth_model.R")

test_that("age_depth_model() creates valid age depth model objects", {
  test_data <- data.frame(depth_col = 0:5, age_col = 2000:1995)
  adm <- age_depth_model(test_data, depth = depth_col, age = age_col)
  expect_is(adm, "age_depth_model")
  expect_true(is_age_depth_model(adm))
  expect_identical(validate_age_depth_model(adm), adm)
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
