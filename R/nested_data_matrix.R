
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
nested_data_matrix <- function(.data, key, value, qualifiers = NULL, fill = NA, groups = NULL,
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

  variables <- dplyr::distinct(
    dplyr::select(
      dplyr::ungroup(.data),
      !!key
    )
  )[[1]]

  data <- dplyr::select(dplyr::ungroup(.data), !!groups, !!qualifiers, !!key, !!value)

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

  nested$qualifiers <- purrr::map(nested$wide_df, function(df) dplyr::select(df, !!qualifiers))
  nested$data <- purrr::map(nested$wide_df, function(df) {
    df <- dplyr::select(df, -!!qualifiers)
    dplyr::mutate_all(df, trans)
  })

  nested$wide_df_original <- NULL
  tibble::as_tibble(dplyr::ungroup(nested))
}

#' Perform an analysis on a nested data matrix
#'
#' @param .data A data frame with a list column of data frames
#' @param data_column The column that contains the data
#' @param model_column The column that will contain the model(s)
#' @param fun A model function
#' @param data_arg The data argument of fun
#' @param ... Passed to fun
#'
#' @return .data with an additional list column of fun output
#' @export
#'
nested_anal <- function(.data, data_column, fun, data_arg, ..., model_column = "model") {
  data_column <- enquo(data_column)

  .data[[model_column]] <- purrr::map(
    dplyr::pull(.data, !!data_column),
    function(df) {
      data_arg_list <- list()
      data_arg_list[[data_arg]] <- df
      purrr::invoke(fun, data_arg_list, ...)
    }
  )

  .data
}

#' @rdname nested_anal
#' @export
nested_pca <- function(.data, data_column = "data", ...) {
  npca <- nested_anal(
    .data,
    data_column = !!enquo(data_column),
    fun = stats::prcomp,
    data_arg = "x",
    scale. = FALSE,
    ...
  )

  npca$variance <- purrr::map(
    npca$model,
    function(model) {
      tibble::tibble(
        component = seq_along(model$sdev),
        sdev = model$sdev,
        var = model$sdev ^ 2,
        prop_var = (model$sdev ^ 2) / sum(model$sdev ^ 2),
        cum_prop_var = cumsum((model$sdev ^ 2) / sum(model$sdev ^ 2))
      )
    }
  )

  npca$loadings <- purrr::map(
    npca$model,
    function(model) {
      df <- tibble::rownames_to_column(
        as.data.frame(model$rotation),
        var = "variable"
      )
      tibble::as_tibble(df)
    }
  )

  npca$scores <- purrr::map(
    npca$model,
    function(model) {
      tibble::as_tibble(stats::predict(model))
    }
  )

  npca
}
