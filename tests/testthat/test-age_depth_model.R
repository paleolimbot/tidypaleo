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
  adm <- age_depth_model(test_data, depth = depth_col, age = age_col)
  expect_equal(predict(adm, depth = 0:5)$age, as.numeric(2000:1995))
  expect_equal(predict(adm, age = 1995:2000)$depth, as.numeric(5:0))

  adm_increase <- age_depth_model(data.frame(depth_col = 0:5, age_col = 0:5),
                                  depth = depth_col, age = age_col)
  expect_equal(predict(adm_increase, depth = 0:5)$age, as.numeric(0:5))
  expect_equal(predict(adm_increase, age = 5:0)$depth, as.numeric(5:0))
})
