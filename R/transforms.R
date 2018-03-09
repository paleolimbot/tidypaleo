
#' Transforms
#'
#' @param x A paired vector of x values
#' @param y A paired vector of y values
#' @param x0 The x value to anchor the transform
#' @param y0 The y value to anchor the transform
#' @param slope The slope (in units of y/x) to use for the transform
#'
#' @return A list with component functions \code{trans} and \code{inverse}
#' @export
#'
trans_interpolate <- function(x, y) {
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

#' @importFrom dplyr last
#' @importFrom dplyr first
#' @rdname trans_interpolate
#' @export
trans_average <- function(x, y, x0 = last, y0 = last, slope = NULL) {
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

#' @rdname trans_interpolate
#' @export
trans_exact <- function(x, y) {
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

#' @rdname trans_interpolate
#' @export
trans_na <- function(x, y) {
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
