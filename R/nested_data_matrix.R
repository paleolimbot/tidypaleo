
#' Prepare a parameter-long data frame for statistical analysis
#'
#' @param .data Data in parameter-long form
#' @param key The column name that contains the column names of the data matrix
#' @param value The column name that contains the values
#' @param qualifiers Columns that add context to observations (e.g., depth, zone, core)
#' @param fill If a key/value combination doesn't exist in the input, this value will be
#'   assigned in the data matrix. Generally, using NA for geochemical data and 0 for relative
#'   abundance data is advised.
#' @param groups Use [group_by][dplyr::group_by] or this argument to group by one or more columns (e.g., core or lake)
#' @param select_if Use `~TRUE` to keep all columns; use `~all(is.finite(.))` to keep columns
#'   with all finite values. See [select_if][dplyr::select_if].
#' @param filter_all Use `any_vars(TRUE)` to keep all observations; use `all_vars(is.finite(.))` to
#'   keep only observations with finite (non-missing) values. See [filter_all][dplyr::filter_all].
#' @param trans A function that will be applied to all columns, column-wise. Use [identity][base::identity]
#'   to perform no transformation, use [scale][base::scale] to scale each column to a mean of zero and
#'   variance of 1. See [mutate_all][dplyr::mutate_all].
#' @param ... One or more columns to unnest.
#'
#' @return A nested data matrix, which is composed of a [tibble::tibble()]
#'   with tibble list-columns `data`, `discarded_rows`, `discarded_columns`, and
#'   `qualifiers`.
#' @export
#'
#' @examples
#' nested_data(
#'   alta_lake_geochem,
#'   qualifiers = c(age, depth, zone),
#'   key = param,
#'   value = value,
#'   trans = scale
#' )
#'
#' @importFrom dplyr any_vars all_vars
#' @importFrom rlang enquo quos !! !!!
#'
nested_data <- function(.data, qualifiers = NULL, key = NULL, value, fill = NA,
                        select_if = ~TRUE, filter_all = any_vars(TRUE), trans = identity,
                        groups = NULL) {

  stopifnot(
    is.data.frame(.data)
  )

  groups <- enquo(groups)
  qualifiers <- enquo(qualifiers)
  key <- enquo(key)
  value <- enquo(value)

  if(rlang::quo_is_null(groups)) {
    groups <- dplyr::group_vars(.data)
  }

  # this makes sure all args refer to valid columns, enables checking names later
  group_vars <- unname(tidyselect::vars_select(colnames(.data), !!groups))
  qualifier_vars <- unname(tidyselect::vars_select(colnames(.data), !!qualifiers))
  key_vars <- unname(tidyselect::vars_select(colnames(.data), !!key))
  value_vars <- unname(tidyselect::vars_select(colnames(.data), !!value))

  # key, qualifier, and group vars can't intersect
  stopifnot(
    length(intersect(group_vars, qualifier_vars)) == 0,
    length(intersect(group_vars, key_vars)) == 0,
    length(intersect(qualifier_vars, key_vars)) == 0
  )

  # value vars are value_vars minus all others (facilitates use of tidyselect helpers)
  value_vars <- setdiff(value_vars, c(group_vars, qualifier_vars, key_vars))

  if(length(value_vars) == 0) {
    stop(">0 value columns must be specified")
  } else if(length(value_vars) == 1) {
    if(length(key_vars) != 1) stop("Using a single `value` column, exactly one column must be specified by `key`")

    # variables as distinct values of `key`
    variables <- dplyr::distinct(
        dplyr::select(
          dplyr::ungroup(.data),
          !!key
        )
      )[[1]]

    # makes sure factor keys stay in order later
    variables <- as.character(sort(variables))

  } else {
    # ignore key_vars and fill if wide data is specified
    if(length(key_vars) > 0) message("Ignoring variables specified in `key` because more than one `value` column was specified")
    key_vars <- character(0)
    if(!identical(fill, NA)) message("Ignoring `fill`")
    variables <- character(0)
  }

  # checking column names here to make sure everything can be unnest()ed
  # without name conflicts
  reserved_names <- c(
    "discarded_columns", "discarded_rows", "qualifiers",
    "data", "df_original"
  )

  # col names can't be reserved, and can't be identical to any of the variables
  check_problematic_names(c(group_vars, qualifier_vars, key_vars, value_vars), c(reserved_names, variables))

  # variables can't be reserved names either, because they become columns eventually (if they aren't already)
  check_problematic_names(variables, reserved_names, data_name = key_vars)

  data <- dplyr::select(
    dplyr::ungroup(.data),
    !!group_vars, !!qualifier_vars, !!key_vars, !!value_vars
  )
  grouped <- dplyr::group_by_at(data, dplyr::vars(!!group_vars))
  nested <- tidyr::nest(grouped, df_original = c(!!qualifier_vars, !!key_vars, !!value_vars))

  output <- purrr::map(nested$df_original, function(df) {
    # spread if there is one value var
    if(length(value_vars) == 1) {
      df <- tidyr::spread(df, !!key_vars, !!value_vars, fill = fill)
      # some "variables" aren't present in all groups
      value_vars <- intersect(variables, colnames(df))
    }

    # select -> filter
    data_df <- dplyr::select(df, !!value_vars)
    data_df_selected <- dplyr::select_if(data_df, select_if)
    deselected_names <- setdiff(colnames(data_df), colnames(data_df_selected))

    # deselect names that were identified by the previous operation
    selected_df <- dplyr::select(df, -!!deselected_names)
    discarded_columns <- dplyr::select(data_df, !!deselected_names)
    value_vars <- setdiff(value_vars, deselected_names)

    # only filter at value variables
    filtered_df <- dplyr::filter_at(selected_df, dplyr::vars(!!value_vars), filter_all)
    discarded_rows <- dplyr::setdiff(selected_df, filtered_df)

    # only transform at value variables
    trans_df <- dplyr::mutate_at(filtered_df, dplyr::vars(!!value_vars), trans)

    # qualifier vars should always have at least something (row number)
    qualifiers <- dplyr::select(trans_df, !!qualifier_vars)
    qualifiers$row_number <- seq_len(nrow(qualifiers))

    list(
      discarded_columns = discarded_columns,
      discarded_rows = discarded_rows,
      qualifiers = qualifiers,
      data = dplyr::select(trans_df, -!!qualifier_vars)
    )
  })

  new_nested_data(
    tibble::as_tibble(
      dplyr::bind_cols(
        dplyr::select(
          dplyr::ungroup(nested),
          !!group_vars
        ),
        tibble::as_tibble(
          purrr::transpose(output)
        )
      )
    )
  )
}

#' @rdname nested_data
#' @export
unnested_data <- function(.data, ...) {
  not_list_cols <- names(.data)[!vapply(.data, is.list, logical(1))]
  dots <- rlang::quos(...)

  .data <- dplyr::select(.data, !!not_list_cols, !!!dots)
  tidyr::unnest(.data, c(!!!dots))
}


#' Perform an analysis on a nested data matrix
#'
#' @param .data A data frame with a list column of data frames, possibly created using
#'   [nested_data].
#' @param .fun A model function
#' @param .reserved_names Names that should not be allowed as columns in any
#'   data frame within this object
#' @param .output_column A column name in which the output of .fun should be stored.
#' @param .env Passed to [as_function][rlang::as_function]
#' @param ... Passed to fun
#'
#' @return .data with an additional list column of fun output
#' @export
#'
#' @examples
#' nd <- nested_data(
#'   alta_lake_geochem,
#'   qualifiers = c(age, depth, zone),
#'   key = param,
#'   value = value,
#'   trans = scale
#' )
#'
#' na <- nested_analysis(nd, vegan::rda, X = data)
#' plot(na)
#'
nested_analysis <- function(.data, .fun, ..., .output_column = "model", .reserved_names = NULL, .env = parent.frame()) {
  # allow lambda-style functions
  .fun <- rlang::as_function(.fun, env = .env)

  # args get evalulated 'tidily' within the transposed data,
  # so they can refer to columns in the nested_analysis data frame
  more_args <- rlang::quos(...)

  stopifnot(
    is.null(.output_column) || is.character(.output_column) & length(.output_column) == 1
  )

  # column names can't be reserved names in .data or in nested data columns
  check_problematic_names(colnames(.data), c(.reserved_names, .output_column))
  purrr::map(colnames(.data), function(col_name) {
    col <- .data[[col_name]]

    if(is.list(col)) {
      purrr::map(col, function(list_item) {
        if(is.data.frame(list_item)) {
          check_problematic_names(colnames(list_item), c(.reserved_names, .output_column), data_name = col_name)
        }
      })
    }
  })

  result <- purrr::map(purrr::transpose(.data), function(row) {
    args <- purrr::map(more_args, function(arg_q) rlang::eval_tidy(arg_q, data = row))
    do.call(.fun, args)
  })

  if(!is.null(.output_column)) {
    .data[[.output_column]] <- result
  }

  new_nested_analysis(.data)
}

new_nested_data <- function(x, subclasses = character(0)) {
  structure(x, class = unique(c(subclasses, "nested_data", class(x))))
}

new_nested_analysis <- function(x, subclasses = character(0)) {
  structure(x, class = unique(c(subclasses, "nested_analysis", class(x))))
}

# dynamically exported in zzz.R
filter.nested_data <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}


# dynamically exported in zzz.R
filter.nested_analysis <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}

# dynamically exported in zzz.R
slice.nested_data <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}

# dynamically exported in zzz.R
slice.nested_analysis <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}

# dynamically exported in zzz.R
arrange.nested_data <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}

# dynamically exported in zzz.R
arrange.nested_analysis <- function(.data, ...) {
  data_class <- class(.data)
  structure(NextMethod(), class = data_class)
}

# dynamically exported in zzz.R
mutate.nested_data <- function(.data, ...) {
  data_class <- class(.data)
  result <- NextMethod()

  # only maintain class if no columns were dropped
  # it's possible that the user corrupts an existing column here
  # but this is probably rare
  if(all(colnames(.data) %in% colnames(result))) {
    structure(result, class = data_class)
  } else {
    structure(result, class = setdiff(data_class, "nested_data"))
  }
}

# dynamically exported in zzz.R
mutate.nested_analysis <- function(.data, ...) {
  data_class <- class(.data)
  result <- NextMethod()

  # only maintain class if no columns were dropped
  # it's possible that the user corrupts an existing column here
  # but this is probably rare
  if(all(colnames(.data) %in% colnames(result))) {
    structure(result, class = data_class)
  } else {
    structure(result, class = setdiff(data_class, "nested_analysis"))
  }
}

#' Plot a nested analysis
#'
#' Calls [plot][graphics::plot] or another (base) plotting function on all models, arranging the output in subplots.
#'
#' @param x,.x A [nested_analysis] object (or subclass)
#' @param .fun A function that produces graphical output
#' @param main The plot title
#' @param ... Passed to the plot function. Tidy evaluation is supported, and arguments are evaluated
#'   within a transposed version of x for each row.
#' @param nrow,ncol Force a number of rows or columns in the output
#' @param .model_column The column containing the model
#' @param .output_column The column in which the output of the plot function should be placed
#'
#' @return the input, invisibly
#' @export
#'
#' @examples
#' nd <- nested_data(
#'   alta_lake_geochem,
#'   qualifiers = c(age, depth, zone),
#'   key = param,
#'   value = value,
#'   trans = scale
#' )
#'
#' na <- nested_analysis(nd, vegan::rda, X = data)
#' plot(na)
#'
#' @importFrom graphics plot
#'
plot.nested_analysis <- function(x, ..., main = "", nrow = NULL, ncol = NULL) {
  main <- enquo(main)
  plot_nested_analysis(x, .fun = graphics::plot, ..., main = !!main, nrow = nrow, ncol = ncol)
}

#' @rdname plot.nested_analysis
#' @export
plot_nested_analysis <- function(.x, .fun, ..., nrow = NULL, ncol = NULL, .model_column = .data$model, .output_column = NULL) {
  .model_column <- enquo(.model_column)

  # handle zero rows quietly
  if(nrow(.x) == 0) return(.x)

  dims <- wrap_dims(nrow(.x), nrow, ncol)
  invisible(
    withr::with_par(list(mfrow = dims), {
      nested_analysis(.x, .fun, !!.model_column, ..., .output_column = .output_column)
    })
  )
}

# I ripped this off of ggplot2 to see how it was done...hard to write it any better
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
  # everything that isn't a list column before "data"
  data_loc <- which(colnames(ndm) == "data")[1]
  if(is.na(data_loc)) stop("'wide_df' was not found in the nested data matrix")
  list_cols <- colnames(ndm)[purrr::map_lgl(ndm, is.list)]
  possible_names <- colnames(ndm)[seq_len(data_loc - 1)]
  setdiff(possible_names, list_cols)
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

drop_list_cols <- function(x, except = character(0)) {
  list_cols <- setdiff(names(x)[vapply(x, is.list, logical(1))], except)
  x[setdiff(names(x), list_cols)]
}
