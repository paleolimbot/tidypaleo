
#' Prepare a parameter-long data frame for statistical analysis
#'
#' @param .data Data in parameter-long form
#' @param key The column name that contains the column names of the data matrix
#' @param value The column name that contains the values
#' @param qualifiers Columns that add context to observations (e.g., depth, zone, core)
#' @param fill If a key/value combination doesn't exist in the input, this value will be
#'   assigned in the data matrix. Generally, using NA for geochemical data and 0 for relative
#'   abundance data is advised.
#' @param groups Use \link[dplyr]{group_by} or this argument to group by one or more columns (e.g., core or lake)
#' @param select_if Use \code{~TRUE} to keep all columns; use \code{~all(is.finite(.))} to keep columns
#'   with all finite values. See \link[dplyr]{select_if}.
#' @param filter_all Use \code{any_vars(TRUE)} to keep all observations; use \code{all_vars(is.finite(.))} to
#'   keep only observations with finite (non-missing) values. See \link[dplyr]{filter_all}.
#' @param trans A function that will be applied to all columns, column-wise. Use \link[base]{identity}
#'   to perform no transformation, use \link[base]{scale} to scale each column to a mean of zero and
#'   variance of 1. See \link[dplyr]{mutate_all}.
#'
#' @return A nested data matrix
#' @export
#'
#' @importFrom dplyr any_vars all_vars
#' @importFrom rlang enquo !!
#'
nested_data <- function(.data, key, value, qualifiers = NULL, fill = NA, groups = NULL,
                               select_if = ~TRUE, filter_all = any_vars(TRUE), trans = identity) {

  stopifnot(
    is.data.frame(.data)
  )

  qualifiers <- enquo(qualifiers)
  key <- enquo(key)
  value <- enquo(value)
  groups <- enquo(groups)

  if(rlang::quo_is_null(groups)) {
    groups <- dplyr::group_vars(.data)
  }

  variables <- as.character(
    dplyr::distinct(
      dplyr::select(
        dplyr::ungroup(.data),
        !!key
      )
    )[[1]]
  )

  data <- dplyr::select(dplyr::ungroup(.data), !!groups, !!qualifiers, !!key, !!value)

  reserved_names <- c(
    "wide_df", "discarded_columns", "discarded_rows", "qualifiers",
    "data", "wide_df_original"
  )

  # col names can't be reserved, and can't be identical to any of the variables
  check_problematic_names(colnames(data), c(reserved_names, variables))

  # variables can't be reserved names either
  check_problematic_names(variables, reserved_names, data_name = rlang::quo_label(key))

  wide <- tidyr::spread(
    data,
    key = !!key, value = !!value,
    fill = fill
  )

  grouped <- dplyr::group_by_at(wide, dplyr::vars(!!groups))
  nested <- tidyr::nest(grouped, !!qualifiers, !!variables, .key = "wide_df_original")
  nested$wide_df <- purrr::map(nested$wide_df_original, function(df) {
    # select -> filter
    data_df <- dplyr::select(df, !!variables)
    data_df_selected <- dplyr::select_if(data_df, select_if)
    deselected_names <- setdiff(colnames(data_df), colnames(data_df_selected))

    # deselect names that were identified by the previous operation
    df <- dplyr::select(df, -!!deselected_names)

    # only filter at value variables
    df <- dplyr::filter_at(df, dplyr::vars(-!!qualifiers), filter_all)

    tibble::as_tibble(df)
  })

  nested$discarded_columns <- purrr::map2(nested$wide_df, nested$wide_df_original, function(wide_df, wide_df_original) {
    deselected_names <- setdiff(colnames(wide_df_original), colnames(wide_df))
    tibble::as_tibble(dplyr::select(wide_df_original, !!deselected_names))
  })

  nested$discarded_rows <- purrr::map2(nested$wide_df, nested$wide_df_original, function(wide_df, wide_df_original) {
    deselected_names <- setdiff(colnames(wide_df_original), colnames(wide_df))
    tibble::as_tibble(dplyr::setdiff(
      dplyr::select(wide_df_original, -!!deselected_names),
      wide_df
    ))
  })

  nested$qualifiers <- purrr::map(nested$wide_df, function(df) {
    df <- dplyr::select(df, !!qualifiers)
    df$row_number <- seq_len(nrow(df))
    df
  })
  nested$data <- purrr::map(nested$wide_df, function(df) {
    df <- dplyr::select(df, -!!qualifiers)
    dplyr::mutate_all(df, trans)
  })

  nested$wide_df_original <- NULL
  new_nested_data(tibble::as_tibble(dplyr::ungroup(nested)))
}

#' Perform an analysis on a nested data matrix
#'
#' @param .data A data frame with a list column of data frames, possibly created using
#'   \link{nested_data}.
#' @param data_column The name of the column that contains the data. Evaluated
#'   like \link[dplyr]{pull}.
#' @param fun A model function
#' @param data_arg The data argument of fun
#' @param reserved_names Names that should not be allowed as columns in any
#'   data frame within this object
#' @param ... Passed to fun
#'
#' @return .data with an additional list column of fun output
#' @export
#'
nested_analysis <- function(.data, data_column, fun, data_arg, ..., reserved_names = NULL) {
  data_column <- enquo(data_column)
  model_column <- "model"

  # column names can't be reserved names in .data or in nested data columns
  check_problematic_names(colnames(.data), c(reserved_names, model_column))
  purrr::map(colnames(.data), function(col_name) {
    col <- .data[[col_name]]

    if(is.list(col)) {
      purrr::map(col, function(list_item) {
        if(is.data.frame(list_item)) {
          check_problematic_names(colnames(list_item), c(reserved_names, model_column), data_name = col_name)
        }
      })
    }
  })

  .data[[model_column]] <- purrr::map(
    dplyr::pull(.data, !!data_column),
    function(df) {
      data_arg_list <- list()
      data_arg_list[[data_arg]] <- df
      purrr::invoke(fun, data_arg_list, ...)
    }
  )

  new_nested_analysis(.data)
}

new_nested_data <- function(x, subclasses = character(0)) {
  structure(x, class = unique(c(subclasses, "nested_data", class(x))))
}

new_nested_analysis <- function(x, subclasses = character(0)) {
  structure(x, class = unique(c(subclasses, "nested_analysis", class(x))))
}

#' @importFrom dplyr filter
#' @export
dplyr::filter


#' @importFrom dplyr filter
#' @export
filter.nested_data <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}

#' @importFrom dplyr filter
#' @export
filter.nested_analysis <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}

#' @importFrom dplyr slice
#' @export
slice.nested_data <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}

#' @importFrom dplyr slice
#' @export
slice.nested_analysis <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}

#' @importFrom dplyr arrange
#' @export
arrange.nested_data <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}

#' @importFrom dplyr arrange
#' @export
arrange.nested_analysis <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}

#' @importFrom dplyr mutate
#' @export
mutate.nested_data <- function(.data, ...) {
  data_class <- class(.data)
  result <- NextMethod()

  # only maintain class if no columns were dropped
  # it's possible that the user corrupts an existing column here
  # but this is probably rare
  if(all(colnames(.data) %in% colnames(result))) {
    structure(result, class = data_class)
  } else {
    result
  }
}

#' @importFrom dplyr mutate
#' @export
mutate.nested_analysis <- function(.data, ...) {
  data_class <- class(.data)
  result <- NextMethod()

  # only maintain class if no columns were dropped
  # it's possible that the user corrupts an existing column here
  # but this is probably rare
  if(all(colnames(.data) %in% colnames(result))) {
    structure(result, class = data_class)
  } else {
    result
  }
}

#' Plot a nested analysis
#'
#' Calls \link[graphics]{plot} or another (base) plotting function on all models, arranging the output in subplots.
#'
#' @param x,.x A \link{nested_analysis} object (or subclass)
#' @param .fun A function that produces graphical output
#' @param main The plot title
#' @param ... Passed to the plot function. Tidy evaluation is supported, and arguments are evaluated
#'   within a transposed version of x for each row.
#' @param nrow,ncol Force a number of rows or columns in the output
#'
#' @return A list containing the result of the plot function (invisibly)
#'
#' @importFrom graphics plot
#' @export
plot.nested_analysis <- function(x, ..., main = "", nrow = NULL, ncol = NULL) {
  main <- enquo(main)
  plot_nested_analysis(x, .fun = graphics::plot, ..., main = !!main, nrow = nrow, ncol = ncol)
}

#' @rdname plot.nested_analysis
#' @export
plot_nested_analysis <- function(.x, .fun, ..., nrow = NULL, ncol = NULL) {
  n_plots <- nrow(.x)

  # args get evalulated 'tidily' within the transposed data,
  # so they can refer to columns in the nested_analysis data frame
  more_args <- rlang::quos(...)

  if(n_plots == 0) {
    stop("Nothing to plot, object has zero rows")
  } else {
    dims <- wrap_dims(n_plots, nrow, ncol)

    invisible(
      withr::with_par(list(mfrow = dims), {

        # using map rather than for in case the plot function returns something
        purrr::map(purrr::transpose(.x), function(row) {

          args <- c(
            list(row$model),
            purrr::map(more_args, function(arg_q) rlang::eval_tidy(arg_q, data = row))
          )

          purrr::invoke(.fun, args)
        })
      })
    )
  }
}

# I ripped this off of ggplot2 to see how it was done...
wrap_dims <- function(n, nrow = NULL, ncol = NULL) {
  if (is.null(ncol) && is.null(nrow)) {
    default_row_col <- grDevices::n2mfrow(n)
    nrow <- default_row_col[2]
    ncol <- default_row_col[1]
  } else if (is.null(ncol)) {
    ncol <- ceiling(n / nrow)
  } else if (is.null(nrow)) {
    nrow <- ceiling(n / ncol)
  }

  c(nrow, ncol)
}

get_grouping_vars <- function(ndm) {
  # everything before "wide_df"
  wide_loc <- which(colnames(ndm) == "wide_df")[1]
  if(is.na(wide_loc)) stop("'wide_df' was not found in the nested data matrix")
  colnames(ndm)[seq_len(wide_loc - 1)]
}

check_problematic_names <- function(col_names, bad_names, data_name = ".data", action = stop) {
  bad_names_in_df <- intersect(col_names, bad_names)
  if(length(bad_names_in_df) > 0) {
    action(
      "The following names in ", data_name, " are reserved must be renamed: ",
      paste0("`", bad_names_in_df, "`", collapse = ", ")
    )
  }
}
