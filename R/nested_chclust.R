
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
#' @examples
#' library(tidyr)
#' library(dplyr)
#'
#' nested_coniss <- keji_lakes_plottable %>%
#'   group_by(location) %>%
#'   nested_data_matrix(taxon, rel_abund, depth, fill = 0) %>%
#'   nested_chclust()
#'
#' # plot broken stick dispersion to verify number of plausible groups
#' library(ggplot2)
#'
#' nested_coniss %>%
#'   unnest(broken_stick) %>%
#'   tidyr::gather(type, value, broken_stick_dispersion, dispersion) %>%
#'   ggplot(aes(x = n_groups, y = value, col = type)) +
#'   geom_line() +
#'   geom_point() +
#'   facet_wrap(vars(location))
#'
nested_chclust <- function(.data, data_column = "data", qualifiers_column = "qualifiers", distance_fun = stats::dist,
                           n_groups = NULL, ...) {
  data_column <- enquo(data_column)
  qualifiers_column <- enquo(qualifiers_column)
  n_groups <- enquo(n_groups)

  .data$distance <- purrr::map(dplyr::pull(.data, !!data_column), distance_fun)

  nchclust <- nested_anal(
    .data,
    data_column = "distance",
    fun = rioja::chclust,
    data_arg = "d",
    reserved_names = c("broken_stick", "n_groups", "chclust_zone", "nodes", "segments"),
    ...
  )

  # 100 groups is an arbitrary maximum...ensures that a large number of zones
  nchclust$broken_stick <- purrr::map(
    nchclust$model,
    function(model) tibble::as_tibble(rioja::bstick(model, plot = FALSE, ng = 1000))
  )
  nchclust$broken_stick <- purrr::map(
    nchclust$broken_stick, dplyr::rename,
    n_groups = "nGroups", broken_stick_dispersion = "bstick"
  )
  nchclust$broken_stick <- purrr::map(
    nchclust$broken_stick,
    function(.data) dplyr::filter(.data, is.finite(.data$dispersion))
  )

  # n_groups based on user input OR default determine_n_groups()
  if(rlang::quo_is_null(n_groups)) {
    nchclust$n_groups <- purrr::map2(nchclust$model, nchclust$broken_stick, determine_n_groups)
  } else {
    nchclust <- dplyr::mutate(nchclust, n_groups = !!n_groups)
    nchclust$n_groups <- as.list(nchclust$n_groups)
  }

  # zones based on stats::cutree()
  nchclust$chclust_zone <- purrr::map2(nchclust$model, nchclust$n_groups, function(model, n_groups) {
    stats::cutree(model, k = n_groups)
  })
  nchclust$zone_info <- purrr::pmap(list(nchclust$chclust_zone, nchclust$qualifiers, nchclust$n_groups), group_boundaries)

  # denrogram segments and nodes
  nchclust$nodes <- purrr::pmap(
    list(nchclust$model, dplyr::pull(nchclust, !!qualifiers_column), nchclust$chclust_zone),
    qualify_dendro_data
  )
  nchclust$segments <- purrr::map(
    nchclust$nodes,
    function(df) tidyr::unnest(dplyr::select(df, "node_id", "chclust_zone", "segments"))
  )
  nchclust$nodes <- purrr::map(nchclust$nodes, function(df) dplyr::select(df, -"segments"))

  nchclust
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
  # at the beginning
  broken_stick <- tibble::add_row(broken_stick, n_groups = 1, dispersion = Inf, broken_stick_dispersion = 0, .before = 1)

  broken_stick$disp_gt_bstick <- broken_stick$dispersion > (broken_stick$broken_stick_dispersion * threshold)
  runlength <- rle(broken_stick$disp_gt_bstick)
  broken_stick$n_groups[runlength$lengths[1]]
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
    data1 <- node_data(
      node[[1]], qualifiers, numeric_vars, chclust_zones,
      recursive_level + 1, node_id = node_id + 1
    )
    data2 <- node_data(
      node[[2]], qualifiers, numeric_vars, chclust_zones,
      recursive_level + 1, node_id = max(data1$node_id) + 1
    )

    # first row is always the last node
    n1 <- dplyr::slice(data1, 1)
    n2 <- dplyr::slice(data2, 1)

    data <- (n1[numeric_vars] + n2[numeric_vars]) / 2
    data$chclust_zone <- dplyr::if_else(
      n1$chclust_zone == n2$chclust_zone,
      true = n1$chclust_zone,
      false = NA_integer_,
      missing = NA_integer_
    )
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
