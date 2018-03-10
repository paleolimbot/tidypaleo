
#' Use Age Depth Models as a Second ggplot Axis
#'
#' @param x An \link{age_depth_model}
#' @param primary Specify the primary axis as 'age' or 'depth'
#' @param ... Passed to \link[ggplot2]{sec_axis}
#'
#' @return A ggplot2 \link[ggplot2]{sec_axis} for use in \link[ggplot2]{scale_x_continuous},
#'   \link[ggplot2]{scale_y_continuous}, or their reverse variants.
#' @export
#'
#' @examples
#' library(ggplot2)
#' alta_lake_adm <- age_depth_model(
#'   alta_lake_210Pb_ages, depth = depth_cm, age = age_year_ad
#' )
#'
#' ggplot(alta_lake_210Pb_ages, aes(y = depth_cm, x = age_year_ad)) +
#'   geom_line() +
#'   geom_point() +
#'   scale_y_reverse(sec.axis = as_sec_axis(alta_lake_adm))
#'
as_sec_axis <- function(x, ...) {
  UseMethod("as_sec_axis")
}

#' @rdname as_sec_axis
#' @export
as_sec_axis.age_depth_model <- function(x, ..., primary = c("depth", "age")) {
  primary <- match.arg(primary)
  . <- NULL; rm(.) # no other way to create a second axis than with a .
  if(primary == "depth") {
    ggplot2::sec_axis(trans = ~predict(x, depth = .)$age)
  } else {
    ggplot2::sec_axis(trans = ~predict(x, age = .)$depth)
  }
}
