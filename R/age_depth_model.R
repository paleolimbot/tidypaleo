
#' Create age depth models
#'
#' @param .data A data frame
#' @param depth,age,age_min,age_max Expressions evaluated in \code{.data} that provide the
#'   known depths, known ages, and error information if available. These expressions
#'   are evaluated like they are within \link[dplyr]{mutate}.
#' @param interpolate_age,extrapolate_age_below,extrapolate_age_above These arguments
#'   provide the rules for interpolating and extrapolating ages based on depths.
#' @param interpolate_age_err,extrapolate_age_err_below,extrapolate_age_err_above These arguments
#'   provide the rules for interpolating and extrapolating age min and max values
#'   based on depths.
#'
#' @return An age depth model object.
#' @export
#' @importFrom rlang !!
#' @importFrom rlang .data
#'
age_depth_model <- function(
  .data, depth, age, age_min = NA_real_, age_max = NA_real_,
  interpolate_age = trans_interpolate,
  extrapolate_age_below = ~trans_average(.x, .y, x0 = last, y0 = last),
  extrapolate_age_above = ~trans_average(.x, .y, x0 = first, y0 = first),
  interpolate_age_err = trans_exact,
  extrapolate_age_err_below = trans_na,
  extrapolate_age_err_above = trans_na
) {
  # enquose arguments
  depth <- rlang::enquo(depth)
  age <- rlang::enquo(age)
  age_min <- rlang::enquo(age_min)
  age_max <- rlang::enquo(age_max)

  # create data from .data + args + dplyr::mutate()
  data <- dplyr::transmute(
    .data,
    depth = !!depth,
    age = !!age,
    age_min = !!age_min,
    age_max = !!age_max
  )
  data <- dplyr::arrange(data, .data$depth)

  # sanitize, validate trans factory arguments
  trans_factory_args <- list(
    interpolate_age = interpolate_age,
    extrapolate_age_below = extrapolate_age_below,
    extrapolate_age_above = extrapolate_age_above,
    interpolate_age_err = interpolate_age_err,
    extrapolate_age_err_below = extrapolate_age_err_below,
    extrapolate_age_err_above = extrapolate_age_err_above
  )
  trans_factory_args <- lapply(trans_factory_args, as_trans_factory)
  lapply(trans_factory_args, validate_trans_factory)

  # create
  adm <- new_age_depth_model(
    list(
      data = tibble::as_tibble(data),
      trans_factories = trans_factory_args
    )
  )

  # add transforms trained with data
  adm$trans <- create_trans_list(adm)

  # validate
  validate_age_depth_model(adm)

  # return
  adm
}

#' Create age depth model objects
#'
#' @param x A list to be classed as an age depth model object
#'
#' @return An age depth model object
#' @export
#'
new_age_depth_model <- function(x) {
  if(!is.list(x)) stop("objects of class age_depth_model must be a list")
  structure(x, class = "age_depth_model")
}

#' Validate age depth model objects
#'
#' @param x An age depth model object
#'
#' @return The input, invisibly
#' @export
#'
validate_age_depth_model <- function(x) {
  if(!is.list(x)) stop("objects of class age_depth_model must be a list")
  if(!all(c("trans", "trans_factories", "data") %in% names(x))) {
    stop("objects of class age_depth_model must have components, ",
         "'trans', 'trans_factories', and 'data")
  }
  if(!tibble::is_tibble(x$data)) stop("x$data is not a tibble")
  if(!is.list(x$trans)) stop("x$trans is not a list")
  if(!is.list(x$trans_factories)) stop("x$trans_factories is not a list")
  lapply(x$trans, validate_trans)
  lapply(x$trans_factories, validate_trans_factory)
  invisible(x)
}

#' Test for age depth models
#'
#' @param x An object
#'
#' @return TRUE if the object is an age depth model, FALSE otherwise
#' @export
#'
is_age_depth_model <- function(x) {
  inherits(x, "age_depth_model")
}

create_trans_list <- function(adm) {

  # set up how each trans should be created
  trans_args <- list(
    interpolate_age = list(cols = c("depth", "age"), trans = "interpolate_age"),
    extrapolate_age_above = list(cols = c("depth", "age"), trans = "extrapolate_age_above"),
    extrapolate_age_below = list(cols = c("depth", "age"), trans = "extrapolate_age_below"),
    interpolate_age_min = list(cols = c("depth", "age_min"), trans = "interpolate_age_err"),
    interpolate_age_max = list(cols = c("depth", "age_max"), trans = "interpolate_age_err"),
    extrapolate_age_min_above = list(
      cols = c("depth", "age_min"), trans = "extrapolate_age_err_above"
    ),
    extrapolate_age_max_above = list(
      cols = c("depth", "age_max"), trans = "extrapolate_age_err_above"
    ),
    extrapolate_age_min_below = list(
      cols = c("depth", "age_min"), trans = "extrapolate_age_err_below"
    ),
    extrapolate_age_max_below = list(
      cols = c("depth", "age_max"), trans = "extrapolate_age_err_below"
    )
  )

  # create each transform
  trans <- lapply(trans_args, function(x) {
    adm$trans_factories[[x$trans]](adm$data[[x$cols[1]]], adm$data[[x$cols[2]]])
  })

  # return list
  trans
}
