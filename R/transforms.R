
#' Transforms
#'
#' @param x A paired vector of x values
#' @param y A paired vector of y values
#' @param x0 The x value to anchor the transform
#' @param y0 The y value to anchor the transform
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
      stop("Inverse transform for trans_na is not defined")
    }
  )
}

verify_length <- function(x, y, n = 2) {
  if(length(y) != length(x)) stop("length(y) must be equal to length(x)")
  if(length(y) < n) stop("length(y) and length(x) must be greater than or equal to ", n)
}
