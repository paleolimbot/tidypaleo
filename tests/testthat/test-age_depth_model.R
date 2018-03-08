context("test-age_depth_model.R")

test_that("age_depth_model() creates valid age depth model objects", {
  test_data <- data.frame(depth_col = 0:5, age_col = 2000:1995)
  adm <- age_depth_model(test_data, depth = depth_col, age = age_col)
  expect_is(adm, "age_depth_model")
  expect_true(is_age_depth_model(adm))
  expect_identical(validate_age_depth_model(adm), adm)
})
