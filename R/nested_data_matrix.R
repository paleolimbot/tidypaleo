
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
  tibble::as_tibble(dplyr::ungroup(nested))
}

#' Perform an analysis on a nested data matrix
#'
#' @param .data A data frame with a list column of data frames, possibly created using
#'   \link{nested_data_matrix}.
#' @param data_column The name of the column that contains the data. Evaluated
#'   like \link[dplyr]{pull}.
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


#' Nested Principal Components Analysis (PCA)
#'
#' Powered by \link[stats]{prcomp}. When creating the \link{nested_data_matrix},
#' the data should be scaled (i.e, \code{trans = scale}) if all variables are not
#' in the same unit.
#'
#' @inheritParams nested_anal
#' @param ... Passed to \link[stats]{prcomp}.
#'
#' @return .data with additional columns 'model', 'loadings', 'variance' and 'scores'
#' @export
#'
#' @examples
#' library(tidyr)
#'
#' nested_pca <- alta_lake_geochem %>%
#'   nested_data_matrix(
#'     key = param,
#'     value = value,
#'     qualifiers = c(depth, zone),
#'     trans = scale
#'   ) %>%
#'   nested_prcomp()
#'
#' # get variance info
#' nested_pca %>% unnest(variance)
#'
#' # get loadings info
#' nested_pca %>% unnest(loadings)
#'
#' # scores, requalified
#' nested_pca %>% unnest(qualifiers, scores)
#'
nested_prcomp <- function(.data, data_column = "data", ...) {
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
        standard_deviation = model$sdev,
        variance = model$sdev ^ 2,
        variance_proportion = (model$sdev ^ 2) / sum(model$sdev ^ 2),
        variance_proportion_cumulative = cumsum((model$sdev ^ 2) / sum(model$sdev ^ 2))
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

#' Nested Constrained hierarchical clustering (CONISS)
#'
#' Powered by \link[rioja]{chclust}; broken stick using \link[rioja]{bstick}.
#'
#' @inheritParams nested_anal
#' @param distance_fun A distance function like \link[stats]{dist} or \link[vegan]{vegdist}.
#' @param qualifiers_column The column that contains the qualifiers
#' @param n_groups The number of groups to use (can be a vector or expression using vars in .data)
#' @param ... Passed to \link[rioja]{chclust}.
#'
#' @return .data with additional columns 'model', 'broken_stick', and 'segments'
#' @export
#'
#' @references
#' Grimm, E.C. (1987) CONISS: A FORTRAN 77 program for stratigraphically constrained cluster
#' analysis by the method of incremental sum of squares. Computers & Geosciences, 13, 13-35.
#' \url{http://doi.org/10.1016/0098-3004(87)90022-7}
#'
#' Juggins, S. (2017) rioja: Analysis of Quaternary Science Data, R package version (0.9-15.1).
#' (\url{http://cran.r-project.org/package=rioja}).
#'
nested_chclust <- function(.data, data_column = "data", qualifiers_column = "qualifiers", distance_fun = stats::dist,
                           n_groups = NULL, ...) {
  data_column <- enquo(data_column)
  qualifiers_column <- enquo(qualifiers_column)
  n_groups <- enquo(n_groups)

  .data$distance <- purrr::map(dplyr::pull(.data, !!data_column), distance_fun)

  npca <- nested_anal(
    .data,
    data_column = "distance",
    fun = rioja::chclust,
    data_arg = "d",
    ...
  )

  # 100 groups is an arbitrary maximum...ensures that a large number of zones
  npca$broken_stick <- purrr::map(
    npca$model,
    function(model) tibble::as_tibble(rioja::bstick(model, plot = FALSE, ng = 1000))
  )
  npca$broken_stick <- purrr::map(npca$broken_stick, dplyr::rename, n_groups = "nGroups", broken_stick_dispersion = "bstick")
  npca$broken_stick <- purrr::map(npca$broken_stick, function(.data) dplyr::filter(.data, is.finite(.data$dispersion)))

  # n_groups based on user input OR default determine_n_groups()
  if(rlang::quo_is_null(n_groups)) {
    npca$n_groups <- purrr::map2(npca$model, npca$broken_stick, determine_n_groups)
  } else {
    npca <- dplyr::mutate(npca, n_groups = !!n_groups)
    npca$n_groups <- as.list(npca$n_groups)
  }

  # zones based on stats::cutree()
  npca$chclust_zone <- purrr::map2(npca$model, npca$n_groups, function(model, n_groups) {
    stats::cutree(model, k = n_groups)
  })
  npca$zone_info <- purrr::pmap(list(npca$chclust_zone, npca$qualifiers, npca$n_groups), group_boundaries)

  # denrogram segments and nodes
  npca$nodes <- purrr::pmap(list(npca$model, dplyr::pull(npca, !!qualifiers_column), npca$chclust_zone), qualify_dendro_data)
  npca$segments <- purrr::map(npca$nodes, function(df) tidyr::unnest(dplyr::select(df, "node_id", "chclust_zone", "segments")))
  npca$nodes <- purrr::map(npca$nodes, function(df) dplyr::select(df, -"segments"))

  npca
}

group_boundaries <- function(chclust_zones, qualifiers, n_groups = 1) {
  stopifnot(
    length(chclust_zones) == nrow(qualifiers),
    is.numeric(n_groups), length(n_groups) == 1, n_groups > 0
  )

  qualifiers$chclust_zone <- chclust_zones
  group_info <- tidyr::nest(
    dplyr::group_by(qualifiers, .data$chclust_zone),
    .key = "data"
  )

  for(var in setdiff(colnames(qualifiers), "chclust_zone")) {
    if(is.numeric(qualifiers[[var]])) {
      group_info[[paste("min", var, sep = "_")]] <- purrr::map_dbl(group_info$data, function(df) min(df[[var]]))
      group_info[[paste("max", var, sep = "_")]] <- purrr::map_dbl(group_info$data, function(df) max(df[[var]]))
    }
  }

  group_info$data <- NULL

  for(var in setdiff(colnames(qualifiers), "chclust_zone")) {
    if(is.numeric(qualifiers[[var]])) {
      group_info[[paste("boundary", var, sep = "_")]] <-
        (group_info[[paste("max", var, sep = "_")]] + dplyr::lead(group_info[[paste("min", var, sep = "_")]])) / 2
    }
  }

  group_info
}

determine_n_groups <- function(model, broken_stick, threshold = 1.1) {
  # ensures there is at least one group greater than broken stick dispersion
  broken_stick <- tibble::add_row(broken_stick, n_groups = 1, dispersion = 0, broken_stick_dispersion = Inf)

  broken_stick$disp_gt_bstick <- broken_stick$dispersion > (broken_stick$broken_stick_dispersion * threshold)
  runlength <- rle(broken_stick$disp_gt_bstick)
  runlength$lengths[1]
}

qualify_dendro_data <- function(model, qualifiers, chclust_zones) {
  qualifiers$dendro_order <- model$order
  numeric_vars <- colnames(qualifiers)[purrr::map_lgl(qualifiers, is.numeric)]
  node_data(stats::as.dendrogram(model), qualifiers, numeric_vars, chclust_zones)
}

node_data <- function(node, qualifiers, numeric_vars, chclust_zones, recursive_level = 1, node_id = 1) {

  if(stats::is.leaf(node)) {
    data1 <- tibble::tibble()
    data2 <- tibble::tibble()

    data <- dplyr::slice(qualifiers, as.numeric(attr(node, "label")))
    data$chclust_zone <- chclust_zones[attr(node, "label")]
    data$is_leaf <- TRUE
    data$segments <- list(tibble::tibble())
    data$dispersion <- attr(node, "height")
  } else {

    # includes rows for all child nodes
    data1 <- node_data(node[[1]], qualifiers, numeric_vars, chclust_zones, recursive_level + 1, node_id = node_id + 1)
    data2 <- node_data(node[[2]], qualifiers, numeric_vars, chclust_zones, recursive_level + 1, node_id = max(data1$node_id) + 1)

    # first row is always the last node
    n1 <- dplyr::slice(data1, 1)
    n2 <- dplyr::slice(data2, 1)

    data <- (n1[numeric_vars] + n2[numeric_vars]) / 2
    data$chclust_zone <- if(n1$chclust_zone == n2$chclust_zone) n1$chclust_zone else NA_integer_
    data$is_leaf <- FALSE
    data$dispersion <- attr(node, "height")

    data$segments <- list(get_dendro_segments(data, n1, n2, numeric_vars))
  }

  data$recursive_level <- recursive_level
  data$node_id <- node_id
  tibble::as_tibble(dplyr::bind_rows(data, data1, data2))
}

get_dendro_segments <- function(data, n1, n2, numeric_vars) {
  n1 <- n1[c(numeric_vars, "dispersion")]
  n2 <- n2[c(numeric_vars, "dispersion")]
  data <- data[c(numeric_vars, "dispersion")]

  end_numeric_vars <- paste(numeric_vars, "end", sep = "_")

  n1_end <- n1
  colnames(n1_end) <- paste(colnames(n1_end), "end", sep = "_")
  n2_end <- n2
  colnames(n2_end) <- paste(colnames(n2_end), "end", sep = "_")
  data_end <- data
  colnames(data_end) <- paste(colnames(data_end), "end", sep = "_")

  dplyr::bind_rows(
    dplyr::bind_cols(n1[numeric_vars], n2_end[end_numeric_vars], data["dispersion"], data_end["dispersion_end"]),
    dplyr::bind_cols(n1[numeric_vars], n1_end[end_numeric_vars], data["dispersion"], n1_end["dispersion_end"]),
    dplyr::bind_cols(n2[numeric_vars], n2_end[end_numeric_vars], data["dispersion"], n2_end["dispersion_end"])
  )
}
