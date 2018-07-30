
#' Scales for relative abundance values
#'
#' Continuous scales that (1) always start at 0, (2) always have the same breaks, and
#' (3) expand using a constant rather than a percentage. These scales assume that data are
#' in percentages (i.e., range 0 to 100 rather than 0 to 1).
#'
#' @param ... Passed to \link[ggplot2]{scale_y_continuous} or \link[ggplot2]{scale_x_continuous}
#' @param limits Limits for the scale
#' @param breaks Where to place labels on the scale
#' @param minor_breaks Where to place minor breaks
#' @param expand A vector of expantion constants
#'
#' @return A \link[ggplot2]{scale_y_continuous} or \link[ggplot2]{scale_x_continuous}
#' @export
#'
scale_x_abundance <- function(..., limits = c(0, NA), breaks = seq(10, 90, 30),
                              minor_breaks = seq(0, 100, 10), expand = c(0, 1)) {
  ggplot2::scale_x_continuous(..., limits = limits, breaks = breaks, expand = expand, minor_breaks = minor_breaks)
}

#' @rdname scale_x_abundance
#' @export
scale_y_abundance <- function(..., limits = c(0, NA), breaks = seq(10, 90, 30),
                              minor_breaks = seq(0, 100, 10), expand = c(0, 1)) {
  ggplot2::scale_y_continuous(..., limits = limits, breaks = breaks, expand = expand, minor_breaks = minor_breaks)
}

#' Age-depth scales
#'
#' @param model An age-depth model, or NULL to suppress the second axis
#' @param reversed Reverse the primary age axis (for years BP or similar)
#' @param age_name,depth_name Label for the second axis
#' @param age_breaks,depth_breaks Breaks for the second axis
#' @param ... Passed to \link[ggplot2]{scale_y_continuous} or \link[ggplot2]{scale_x_continuous}
#'
#' @return A \link[ggplot2]{scale_y_continuous} or \link[ggplot2]{scale_x_continuous}
#' @export
#' @importFrom ggplot2 waiver
#'
scale_y_depth_age <- function(model = NULL, age_name = "age", age_breaks = waiver(), ...) {
  second_axis <- age_depth_as_sec_axis(model, primary = "depth", name = age_name, breaks = age_breaks)
  ggplot2::scale_y_reverse(..., sec.axis = second_axis)
}

#' @rdname scale_y_depth_age
#' @export
scale_y_age_depth <- function(model = NULL, reversed = FALSE, depth_name = "depth", depth_breaks = waiver(), ...) {
  second_axis <- age_depth_as_sec_axis(model, primary = "age", name = depth_name, breaks = depth_breaks)
  if(reversed) {
    ggplot2::scale_y_reverse(..., sec.axis = second_axis)
  } else {
    ggplot2::scale_y_continuous(..., sec.axis = second_axis)
  }
}

#' @rdname scale_y_depth_age
#' @export
scale_x_depth_age <- function(model = NULL, age_name = "age", age_breaks = waiver(), ...) {
  second_axis <- age_depth_as_sec_axis(model, primary = "depth", name = age_name, breaks = age_breaks)
  ggplot2::scale_x_reverse(..., sec.axis = second_axis)
}

#' @rdname scale_y_depth_age
#' @export
scale_x_age_depth <- function(model = NULL, reversed = FALSE, depth_name = "depth", depth_breaks = waiver(), ...) {
  second_axis <- age_depth_as_sec_axis(model, primary = "age", name = depth_name, breaks = depth_breaks)
  if(reversed) {
    ggplot2::scale_x_reverse(..., sec.axis = second_axis)
  } else {
    ggplot2::scale_x_continuous(..., sec.axis = second_axis)
  }
}
