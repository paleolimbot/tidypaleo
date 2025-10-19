
#' Create age depth models
#'
#' @param .data A data frame
#' @param depth,age,age_min,age_max Expressions evaluated in `.data` that
#'   provide the known depths, known ages, and error information if available.
#'   These expressions are evaluated like they are within [mutate][dplyr::mutate]
#'   if `.data` is present.
#' @param interpolate_age,extrapolate_age_below,extrapolate_age_above These
#'   arguments provide the rules for interpolating and extrapolating ages based
#'   on depths.
#' @param interpolate_age_limits,extrapolate_age_limits_below,extrapolate_age_limits_above
#'   These arguments provide the rules for interpolating and extrapolating age
#'   min and max values based on depths.
#'
#' @return An age depth model object.
#' @export
#'
#' @examples
#' age_depth_model(
#'   alta_lake_210Pb_ages,
#'   depth = depth_cm, age = age_year_ad,
#'   age_max = age_year_ad + age_error_yr,
#'   age_min = age_year_ad - age_error_yr
#' )
#'
#' @importFrom rlang !!
#' @importFrom rlang .data
#'
age_depth_model <- function(
  .data = NULL, depth, age, age_min = NA_real_, age_max = NA_real_,
  interpolate_age = age_depth_interpolate,
  extrapolate_age_below = ~age_depth_extrapolate(.x, .y, x0 = last, y0 = last),
  extrapolate_age_above = ~age_depth_extrapolate(.x, .y, x0 = first, y0 = first),
  interpolate_age_limits = age_depth_exact,
  extrapolate_age_limits_below = age_depth_na,
  extrapolate_age_limits_above = age_depth_na
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
#' @noRd
#'
new_age_depth_model <- function(x) {
  if(!is.list(x)) stop("objects of class age_depth_model must be a list")
  structure(x, class = "age_depth_model")
}

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

#' @export
print.age_depth_model <- function(x, ...) {
  cat("<age_depth_model>\n")
  cat("Call: \n")
  cat(paste(styler::style_text(x$call_label, strict = TRUE), collapse = "\n"))
  cat("\n")
  invisible(x)
}

is_age_depth_model <- function(x) {
  inherits(x, "age_depth_model")
}

#' Predict age and depth values
#'
#' @param object An [age_depth_model] object
#' @param .data Optional input data frame
#' @param depth,age Specify exactly one of these to predict the other.
#' @param ... Unused
#'
#' @return A data frame with the same number of observations as the input age or
#'   depth vector.
#' @export
#'
#' @examples
#' adm <- age_depth_model(
#'   alta_lake_210Pb_ages,
#'   depth = depth_cm, age = age_year_ad,
#'   age_max = age_year_ad + age_error_yr,
#'   age_min = age_year_ad - age_error_yr
#' )
#'
#' predict(adm, depth = 1:5)
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
#' @param x An [age_depth_model]
#' @param xlab,ylab Axis labels
#' @param xlim,ylim Axis limits
#' @param add Pass TRUE to skip creating a new plot
#' @param ... Passed to [points][graphics::points] to customize points display
#'
#' @return The input, invisibly
#' @export
#'
#' @examples
#' adm <- age_depth_model(
#'   alta_lake_210Pb_ages,
#'   depth = depth_cm, age = age_year_ad,
#'   age_max = age_year_ad + age_error_yr,
#'   age_min = age_year_ad - age_error_yr
#' )
#'
#' plot(adm)
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

  invisible(x)
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


#' Age-depth model interpolators/extrapolators
#'
#' @param x A paired vector of x values
#' @param y A paired vector of y values
#' @param x0 The x value to anchor the transform
#' @param y0 The y value to anchor the transform
#' @param slope The slope (in units of y/x) to use for the transform
#'
#' @return A list with component functions `trans` and `inverse`
#' @export
#'
#' @examples
#' age_depth_model(
#'   alta_lake_210Pb_ages,
#'   depth = depth_cm, age = age_year_ad,
#'   age_max = age_year_ad + age_error_yr,
#'   age_min = age_year_ad - age_error_yr,
#'   extrapolate_age_below = ~age_depth_extrapolate(
#'     tail(.x, 3), tail(.y, 3), x0 = dplyr::last, y0 = dplyr::last
#'   ),
#'   extrapolate_age_above = ~age_depth_extrapolate(
#'     head(.x, 3), head(.y, 3), x0 = dplyr::first, y0 = dplyr::first
#'   )
#' )
#'
age_depth_interpolate <- function(x, y) {
  if(!all(is.finite(c(x, y)))) stop("Non-finite values in transformation")
  verify_length(x, y)
  force(x)
  force(y)
  list(
    trans = function(new_x) {
      stats::approx(x, y, xout = new_x)$y
    },
    inverse = function(new_y) {
      stats::approx(y, x, xout = new_y)$y
    }
  )
}

# need to re-export first and last from dplyr...they both get used here

#' @importFrom dplyr first
#' @export
dplyr::first

#' @importFrom dplyr last
#' @export
dplyr::last

#' @rdname age_depth_interpolate
#' @export
age_depth_extrapolate <- function(x, y, x0 = last, y0 = last, slope = NULL) {
  if(!all(is.finite(c(x, y)))) warning("Non-finite values in average transformation")
  verify_length(x, y)

  model <- stats::lm(y ~ x)
  if(is.null(slope)) {
    slope <- unname(stats::coef(model)[2])
  } else if(is.numeric(slope) && length(slope) == 1) {
    slope <- slope
  } else {
    stop("slope must be a numeric scalar")
  }

  if(is.null(x0)) {
    x0 <- 0
  } else if(is.function(x0)) {
    x0 <- x0(x)
  } else if(is.numeric(x0) && length(x0) == 1) {
    x0 <- x0
  } else {
    stop("x0 must be NULL, a function of x, or a scalar numeric")
  }

  if(is.null(y0)) {
    y0 <- unname(stats::coef(model)[1])
  } else if(is.function(y0)) {
    y0 <- y0(y)
  } else if(is.numeric(y0) && length(y0) == 1) {
    y0 <- y0
  } else {
    stop("y0 must be NULL, a function of y, or a scalar numeric")
  }

  list(
    trans = function(new_x) {
      (new_x - x0) * slope + y0
    },
    inverse = function(new_y) {
      (new_y - y0) / slope + x0
    }
  )
}

#' @rdname age_depth_interpolate
#' @export
age_depth_exact <- function(x, y) {
  verify_length(x, y, 1)
  list(
    trans = function(new_x) {
      y[match(new_x, x)]
    },
    inverse = function(new_y) {
      x[match(new_y, y)]
    }
  )
}

#' @rdname age_depth_interpolate
#' @export
age_depth_na <- function(x, y) {
  verify_length(x, y, 0)
  list(
    trans = function(new_x) {
      rep_len(NA_real_, length.out = length(new_x))
    },
    inverse = function(new_y) {
      rep_len(NA_real_, length.out = length(new_y))
    }
  )
}

#' Coerce and validate transforms and functions that produce them
#'
#' @param factory A function that produces a transform object
#' @param trans A transform object
#' @param x The test x data
#' @param y The test y data
#' @param env The calling environment, for transform factories that are calls or
#'   rlang lambda-style functions.
#'
#' @return The input, invisibly.
#' @export
#'
#' @examples
#' as_trans_factory(age_depth_interpolate)
#'
as_trans_factory <- function(factory, env = parent.frame()) {
  if(is.function(factory)) {
    factory
  } else {
    rlang::as_function(factory, env = env)
  }
}

#' @rdname as_trans_factory
#' @export
validate_trans_factory <- function(factory, x = 1:3, y = 1:3) {
  if(!is.function(factory)) stop("transform factory must be a function")
  validate_trans(factory(x, y), x, y)
  invisible(factory)
}

#' @rdname as_trans_factory
#' @export
validate_trans <- function(trans, x = 1:3, y = 1:3) {
  if(!is.list(trans)) stop("trans must be a list")
  if(!all(c("trans", "inverse") %in% names(trans))) {
    stop("trans must have trans and inverse components")
  }
  if(!is.function(trans$trans)) stop("trans$trans must be a function")
  if(!is.function(trans$inverse)) stop("trans$inverse must be a function")

  trans_result <- try(trans$trans(x), silent = TRUE)
  if(inherits(trans_result, "try-error")) {
    stop("test of trans$trans() failed: ", as.character(trans_result))
  }
  if(!is.numeric(trans_result) || length(trans_result) != length(x)) {
    stop("test of trans$trans() failed: non-vectorized or non-numeric result")
  }
  inverse_result <- try(trans$inverse(y), silent = TRUE)
  if(inherits(inverse_result, "try-error")) {
    stop("test of trans$inverse() failed: ", as.character(trans_result))
  }
  if(!is.numeric(inverse_result) || length(inverse_result) != length(y)) {
    stop("test of trans$inverse() failed: non-vectorized or non-numeric result")
  }
  invisible(trans)
}

verify_length <- function(x, y, n = 2) {
  if(length(y) != length(x)) stop("length(y) must be equal to length(x)")
  if(length(y) < n) stop("length(y) and length(x) must be greater than or equal to ", n)
}


#' Use an age depth model as a second ggplot axis
#'
#' @param x An [age_depth_model]
#' @param primary Specify the primary axis as 'age' or 'depth'
#' @param ... Passed to [sec_axis][ggplot2::sec_axis]
#'
#' @return A ggplot2 [sec_axis][ggplot2::sec_axis] for use in [scale_x_continuous][ggplot2::scale_x_continuous],
#'   [scale_y_continuous][ggplot2::scale_y_continuous], or their reverse variants.
#' @export
#'
#' @examples
#' library(ggplot2)
#' alta_lake_adm <- age_depth_model(
#'   alta_lake_210Pb_ages,
#'   depth = depth_cm,
#'   age = age_year_ad
#' )
#'
#' ggplot(alta_lake_210Pb_ages, aes(y = depth_cm, x = age_year_ad)) +
#'   geom_path() +
#'   geom_point() +
#'   scale_y_reverse(sec.axis = age_depth_as_sec_axis(alta_lake_adm))
#'
age_depth_as_sec_axis <- function(x, primary = c("depth", "age"), ...) {
  primary <- match.arg(primary)

  # if NULL, return the default arg for continuous scales sec.axis
  if(is.null(x)) {
    return(ggplot2::waiver())
  }

  . <- NULL; rm(.) # no other way to create a second axis than with a .
  if(primary == "depth") {
    ggplot2::sec_axis(transform = ~predict(x, depth = .)$age, ...)
  } else {
    ggplot2::sec_axis(transform = ~predict(x, age = .)$depth, ...)
  }
}
