
.onLoad <- function(...) {
  vctrs::s3_register("dplyr::filter", "nested_data")
  vctrs::s3_register("dplyr::filter", "nested_analysis")
  vctrs::s3_register("dplyr::slice", "nested_data")
  vctrs::s3_register("dplyr::slice", "nested_analysis")
  vctrs::s3_register("dplyr::arrange", "nested_data")
  vctrs::s3_register("dplyr::arrange", "nested_analysis")
  vctrs::s3_register("dplyr::mutate", "nested_data")
  vctrs::s3_register("dplyr::mutate", "nested_analysis")
}
