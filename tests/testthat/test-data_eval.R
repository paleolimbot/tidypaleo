
test_that("data_eval creates tibbles with or without data", {
  expect_is(data_eval(a = 1:5, b = 6:10), "tbl_df")
  expect_is(data_eval(.data = tibble::tibble(a = 1:5, b = 6:10), a = a, b = b), "tbl_df")
  expect_identical(
    data_eval(a = 1:5, b = 6:10),
    data_eval(.data = tibble::tibble(a = 1:5, b = 6:10), a = a, b = b)
  )
})

test_that("data_eval accepts tidy eval input", {
  wrapper <- function(.data = NULL, ...) {
    args <- rlang::quos(...)
    data_eval(.data, !!!args)
  }

  expect_is(wrapper(a = 1:5, b = 6:10), "tbl_df")
  expect_is(wrapper(.data = tibble::tibble(a = 1:5, b = 6:10), a = a, b = b), "tbl_df")
  expect_identical(
    wrapper(a = 1:5, b = 6:10),
    wrapper(.data = tibble::tibble(a = 1:5, b = 6:10), a = a, b = b)
  )
})

test_that("NULL values result in no columns", {
  expect_identical(colnames(data_eval(a = 1:5, b = NULL)), "a")
  expect_identical(
    colnames(data_eval(.data = tibble::tibble(a = 1:5, b = 6:10), a = a, b = NULL)),
    "a"
  )
  expect_silent(colnames(data_eval(.data = tibble::tibble(a = 1:5, b = 6:10), a = a, b = NULL)))
})

test_that("no arguments is no problem", {
  expect_is(data_eval(), "tbl_df")
  expect_identical(ncol(data_eval()), 0L)
  expect_is(data_eval(tibble::tibble(a = 1:5, b = 6:10)), "tbl_df")
  expect_identical(ncol(data_eval(tibble::tibble(a = 1:5, b = 6:10))), 0L)
  expect_identical(
    data_eval(),
    data_eval(a = NULL, b = NULL)
  )
  expect_identical(
    data_eval(tibble::tibble(a = 1:5, b = 6:10)),
    data_eval(tibble::tibble(a = 1:5, b = 6:10), a = NULL, b = NULL)
  )
})
