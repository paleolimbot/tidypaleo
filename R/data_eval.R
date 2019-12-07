
#' Create tibbles from user objects and/or user data
#'
#' @param data A data.frame/tibble, or NULL
#' @param ... Arguments are passed to [transmute][dplyr::transmute] if `data` is
#'   present, and [tibble][tibble::tibble] if it is not.
#'
#' @return A [tibble][tibble::tibble] with the results of ...
#' @noRd
#' @importFrom rlang !!!
#'
data_eval <- function(.data = NULL, ...) {
  args <- rlang::quos(...)
  # discard NULLs, which tibble doesn't accept and transmute complains about
  args <- args[!vapply(args, identical, FUN.VALUE = logical(1), rlang::quo(NULL))]

  if(is.null(.data)) {
    tibble::tibble(!!!args)
  } else {
    tibble::as_tibble(dplyr::transmute(.data, !!!args))
  }
}






