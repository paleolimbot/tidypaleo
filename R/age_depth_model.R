
#' Create age depth models
#'
#' @param .data A data frame
#' @param depth,age,age_min,age_max Expressions evaluated in \code{.data} that
#'   provide the known depths, known ages, and error information if available.
#'   These expressions are evaluated like they are within \link[dplyr]{mutate}
#'   if \code{.data} is present.
#' @param interpolate_age,extrapolate_age_below,extrapolate_age_above These
#'   arguments provide the rules for interpolating and extrapolating ages based
#'   on depths.
#' @param interpolate_age_limits,extrapolate_age_limits_below,extrapolate_age_limits_above
#'   These arguments provide the rules for interpolating and extrapolating age
#'   min and max values based on depths.
#'
#' @return An age depth model object.
#' @export
#' @importFrom rlang !!
#' @importFrom rlang .data
#'
age_depth_model <- function(
  .data = NULL, depth, age, age_min = NA_real_, age_max = NA_real_,
  interpolate_age = trans_interpolate,
  extrapolate_age_below = ~trans_average(.x, .y, x0 = last, y0 = last),
  extrapolate_age_above = ~trans_average(.x, .y, x0 = first, y0 = first),
  interpolate_age_limits = trans_exact,
  extrapolate_age_limits_below = trans_na,
  extrapolate_age_limits_above = trans_na
) {
  # check missingness of depth and age (odd error message otherwise)
  if(missing(depth)) stop("depth is a required argument")
  if(missing(age)) stop("age is a required argument")

  # capture call for printing purposes
  call <- match.call()
  call_label <- rlang::expr_text(call)

  # enquose arguments
  depth <- rlang::enquo(depth)
  age <- rlang::enquo(age)
  age_min <- rlang::enquo(age_min)
  age_max <- rlang::enquo(age_max)

  # create data from .data_eval
  data <- data_eval(
    .data,
    depth = !!depth,
    age = !!age,
    age_min = !!age_min,
    age_max = !!age_max
  )
  if(!identical(colnames(data), c("depth", "age", "age_min", "age_max"))) {
    stop("depth, age, age_min, and age_max must all be non-NULL")
  }
  data <- dplyr::arrange(data, .data$depth)

  # sanitize, validate trans factory arguments
  trans_factory_args <- list(
    interpolate_age = interpolate_age,
    extrapolate_age_below = extrapolate_age_below,
    extrapolate_age_above = extrapolate_age_above,
    interpolate_age_limits = interpolate_age_limits,
    extrapolate_age_limits_below = extrapolate_age_limits_below,
    extrapolate_age_limits_above = extrapolate_age_limits_above
  )
  trans_factory_args <- lapply(trans_factory_args, as_trans_factory)
  lapply(trans_factory_args, validate_trans_factory)

  # create
  adm <- new_age_depth_model(
    list(
      call_label = call_label,
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
  if(!all(c("trans", "trans_factories", "data", "call_label") %in% names(x))) {
    stop("objects of class age_depth_model must have components, ",
         "'call_label', 'trans', 'trans_factories', and 'data")
  }
  if(!is.character(x$call_label) || length(x$call_label) != 1) {
    stop("x$call_label is not a character vector of length 1")
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

#' Predict age and depth values
#'
#' @param object An \link{age_depth_model} object
#' @param .data Optional input data frame
#' @param depth,age Specify one of these to predict the other.
#' @param ... Unused
#'
#' @return A data frame with the same number of observations as the input age or
#'   depth vector.
#' @export
#'
#' @importFrom stats predict
#'
predict.age_depth_model <- function(object, .data = NULL, depth = NULL, age = NULL, ...) {
  # create data
  depth <- rlang::enquo(depth)
  age <- rlang::enquo(age)
  data <- data_eval(.data, depth = !!depth, age = !!age)

  # check for both depth and age in data
  if(all(c("depth", "age") %in% data)) stop("One of depth or age must be NULL")

  # using mutate for null data means that the input is in the output,
  # which is probably what the user wants
  if(is.null(.data)) {
    dplyr_method <- dplyr::mutate
  } else {
    dplyr_method <- dplyr::transmute
  }

  # extract trans functions
  trans <- object$trans

  if("depth" %in% colnames(data)) {
    # predict age + age_min + age_max
    max_depth <- max(object$data$depth)
    min_depth <- min(object$data$depth)

    dplyr_method(
      data,
      age = dplyr::case_when(
        .data$depth > max_depth ~ trans$extrapolate_age_below$trans(.data$depth),
        .data$depth < min_depth ~ trans$extrapolate_age_above$trans(.data$depth),
        TRUE ~ trans$interpolate_age$trans(.data$depth)
      ),
      age_min = dplyr::case_when(
        .data$depth > max_depth ~ trans$extrapolate_age_min_below$trans(.data$depth),
        .data$depth < min_depth ~ trans$extrapolate_age_min_above$trans(.data$depth),
        TRUE ~ trans$interpolate_age_min$trans(.data$depth)
      ),
      age_max = dplyr::case_when(
        .data$depth > max_depth ~ trans$extrapolate_age_max_below$trans(.data$depth),
        .data$depth < min_depth ~ trans$extrapolate_age_max_above$trans(.data$depth),
        TRUE ~ trans$interpolate_age_max$trans(.data$depth)
      ),
      method = dplyr::case_when(
        .data$depth > max_depth ~ "extrapolate_below",
        .data$depth < min_depth ~ "extrapolate_above",
        TRUE ~ "interpolate"
      )
    )
  } else if("age" %in% colnames(data)) {
    # predict depth only
    max_age <- max(object$data$age)
    min_age <- min(object$data$age)

    # different if ages go up with depth or go down with depth
    spear <- stats::cor(object$data$depth, object$data$age, method = "spear")
    if(spear >= 0) {
      dplyr_method(
        data,
        depth = dplyr::case_when(
          (.data$age <= max_age) & (.data$age >= min_age) ~ trans$interpolate_age$inverse(.data$age),
          .data$age > max_age ~ trans$extrapolate_age_below$inverse(.data$age),
          .data$age < min_age ~ trans$extrapolate_age_above$inverse(.data$age)
        ),
        method = dplyr::case_when(
          (.data$age <= max_age) & (.data$age >= min_age) ~ "inverse_interpolate",
          .data$age > max_age ~ "inverse_extrapolate_below",
          .data$age < min_age ~ "inverse_extrapolate_above"
        )
      )
    } else {
      dplyr_method(
        data,
        depth = dplyr::case_when(
          (.data$age <= max_age) & (.data$age >= min_age) ~ trans$interpolate_age$inverse(.data$age),
          .data$age < max_age ~ trans$extrapolate_age_below$inverse(.data$age),
          .data$age > min_age ~ trans$extrapolate_age_above$inverse(.data$age)
        ),
        method = dplyr::case_when(
          (.data$age <= max_age) & (.data$age >= min_age) ~ "inverse_interpolate",
          .data$age < max_age ~ "inverse_extrapolate_below",
          .data$age > min_age ~ "inverse_extrapolate_above"
        )
      )
    }
  } else {
    stop("One of depth or age must be NULL")
  }
}

#' @rdname predict.age_depth_model
#' @export
predict_depth <- function(object, age) {
  predict(object, age = age)$depth
}

#' @rdname predict.age_depth_model
#' @export
predict_age <- function(object, depth) {
  predict(object, depth = depth)$age
}

#' Plot an age depth model using base graphics
#'
#' @param x An \link{age_depth_model}
#' @param xlab,ylab Axis labels
#' @param xlim,ylim Axis limits
#' @param add Pass TRUE to skip creating a new plot
#' @param ... Passed to \link[graphics]{points} to customize points display
#'
#' @export
#'
#' @importFrom graphics plot
#'
plot.age_depth_model <- function(x, xlab = "depth", ylab = "age", xlim = NULL, ylim = NULL,
                                 add = FALSE, ...) {
  if(!add) {
    graphics::plot(
      scales::expand_range(range(x$data$depth), mul = 0.1),
      scales::expand_range(
        range(c(x$data$age, x$data$age_min, x$data$age_max), na.rm = TRUE),
        mul = 0.1
      ),
      type = "n",
      xlab = xlab,
      ylab = ylab,
      xlim = xlim,
      ylim = ylim
    )
  }

  plot_range <- graphics::par("usr")
  depth <- NULL; rm(depth) # CMD check gets mad with syntax of curve()
  # above
  graphics::curve(predict(x, depth = depth)[["age"]], xname = "depth", add = TRUE, lty = 2,
        from = plot_range[1], to = min(x$data$depth))
  # interpolate
  graphics::curve(predict(x, depth = depth)[["age"]], xname = "depth", add = TRUE, lty = 1,
        from = min(x$data$depth), to = max(x$data$depth))
  # below
  graphics::curve(predict(x, depth = depth)[["age"]], xname = "depth", add = TRUE, lty = 2,
        from = max(x$data$depth), to = plot_range[2])

  # errors for points
  interp_exact_points <- predict(x, depth = x$data$depth)
  graphics::segments(x$data$depth, interp_exact_points$age_min,
           x$data$depth, interp_exact_points$age_max)

  # interpolated errors without differentiating between interp/extrap
  graphics::curve(predict(x, depth = depth)[["age_min"]], xname = "depth", add = TRUE, lty = 3,
        from = plot_range[1], to = plot_range[2])
  graphics::curve(predict(x, depth = depth)[["age_max"]], xname = "depth", add = TRUE, lty = 3,
        from = plot_range[1], to = plot_range[2])

  # data for model
  graphics::points(x$data$depth, x$data$age, ...)
}

create_trans_list <- function(adm) {

  # set up how each trans should be created
  trans_args <- list(
    interpolate_age = list(cols = c("depth", "age"), trans = "interpolate_age"),
    extrapolate_age_above = list(cols = c("depth", "age"), trans = "extrapolate_age_above"),
    extrapolate_age_below = list(cols = c("depth", "age"), trans = "extrapolate_age_below"),
    interpolate_age_min = list(cols = c("depth", "age_min"), trans = "interpolate_age_limits"),
    interpolate_age_max = list(cols = c("depth", "age_max"), trans = "interpolate_age_limits"),
    extrapolate_age_min_above = list(
      cols = c("depth", "age_min"), trans = "extrapolate_age_limits_above"
    ),
    extrapolate_age_max_above = list(
      cols = c("depth", "age_max"), trans = "extrapolate_age_limits_above"
    ),
    extrapolate_age_min_below = list(
      cols = c("depth", "age_min"), trans = "extrapolate_age_limits_below"
    ),
    extrapolate_age_max_below = list(
      cols = c("depth", "age_max"), trans = "extrapolate_age_limits_below"
    )
  )

  # create each transform
  trans <- lapply(trans_args, function(x) {
    adm$trans_factories[[x$trans]](adm$data[[x$cols[1]]], adm$data[[x$cols[2]]])
  })

  # return list
  trans
}
