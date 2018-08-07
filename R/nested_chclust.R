
#' Nested (Constrained) hierarchical clustering
#'
#' Powered by \link[rioja]{chclust} and \link[stats]{hclust}; broken stick using \link[rioja]{bstick}.
#'
#' @inheritParams nested_analysis
#' @param data_column An expression that evalulates to the data object within each row of .data
#' @param distance_fun A distance function like \link[stats]{dist} or \link[vegan]{vegdist}.
#' @param qualifiers_column The column that contains the qualifiers
#' @param n_groups The number of groups to use (can be a vector or expression using vars in .data)
#' @param .fun Function powering the clustering. Must return an hclust object of some kind.
#' @param ... Passed to \link[rioja]{chclust} or \link[stats]{hclust}.
#'
#' @return .data with additional columns
#' @export
#'
#' @references
#'
#' Bennett, K. (1996) Determination of the number of zones in a biostratigraphic sequence.
#' New Phytologist, 132, 155-170.
#' \url{http://doi.org/10.1111/j.1469-8137.1996.tb04521.x} (Broken stick)
#'
#' Grimm, E.C. (1987) CONISS: A FORTRAN 77 program for stratigraphically constrained cluster
#' analysis by the method of incremental sum of squares. Computers & Geosciences, 13, 13-35.
#' \url{http://doi.org/10.1016/0098-3004(87)90022-7}
#'
#' Juggins, S. (2017) rioja: Analysis of Quaternary Science Data, R package version (0.9-15.1).
#' (\url{http://cran.r-project.org/package=rioja}).
#'
#' See \link[stats]{hclust} for hierarchical clustering references
#'
#' @examples
#' library(tidyr)
#' library(dplyr)
#'
#' nested_coniss <- keji_lakes_plottable %>%
#'   group_by(location) %>%
#'   nested_data(depth, taxon, rel_abund, fill = 0) %>%
#'   nested_chclust_coniss()
#'
#' # plot the dendrograms using base graphics
#' plot(nested_coniss, main = location, ncol = 1)
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
nested_hclust <- function(.data, data_column = "data", qualifiers_column = "qualifiers", distance_fun = stats::dist,
                          n_groups = NULL, ..., .fun = stats::hclust, .reserved_names = character(0)) {
  data_column <- enquo(data_column)
  qualifiers_column <- enquo(qualifiers_column)
  n_groups <- enquo(n_groups)

  data_col_obj <- dplyr::pull(.data, !!data_column)
  qualifiers_col_obj <- dplyr::pull(.data, !!qualifiers_column)

  .data$distance <- purrr::map(data_col_obj, distance_fun)
  qualifier_names <- unique(unlist(purrr::map(qualifiers_col_obj, colnames)))

  nchclust <- nested_analysis(
    .data, .fun = .fun, .data$distance, ...,
    .reserved_names = c(
      .reserved_names,

      # names of columns in this object
      "broken_stick", "n_groups", "hclust_zone", "nodes", "segments",

      # names of columns in nested columns
      paste(qualifier_names, "end", sep = "_"), paste(qualifier_names, "boundary", sep = "_"),
      "dispersion", "broken_stick_dispersion", "dispersion_end",
      "dendro_order", "dendro_order_end", "node_id", "is_leaf", "recursive_level"
    )
  )

  # n_groups based on user input OR default function (like determine_n_groups())
  if(rlang::quo_is_null(n_groups)) {
    nchclust$n_groups <- purrr::map(nchclust$model, determine_n_groups)
  } else {
    nchclust <- dplyr::mutate(nchclust, n_groups = !!n_groups)
    nchclust$n_groups <- as.list(nchclust$n_groups)
  }

  # cophonetic correlation coeficient
  nchclust$CCC <- purrr::map2(nchclust$model, nchclust$distance, function(model, dist) {
    stats::cor(stats::cophenetic(model), dist, method = "pearson")
  })

  # zones based on stats::cutree()
  nchclust$dendro_order <- purrr::map(nchclust$model, function(model) match(seq_along(model$order), model$order))
  nchclust$hclust_zone <- purrr::map2(nchclust$model, nchclust$n_groups, function(model, n_groups) {
    stats::cutree(model, k = n_groups)
  })
  nchclust$zone_info <- purrr::pmap(list(nchclust$hclust_zone, qualifiers_col_obj, nchclust$n_groups), group_boundaries)

  # denrogram segments and nodes
  nchclust$nodes <- purrr::pmap(
    list(nchclust$model, qualifiers_col_obj, nchclust$hclust_zone),
    qualify_dendro_data
  )
  nchclust$segments <- purrr::map(
    nchclust$nodes,
    function(df) tidyr::unnest(dplyr::select(df, "node_id", "hclust_zone", "segments"))
  )
  nchclust$nodes <- purrr::map(nchclust$nodes, function(df) dplyr::select(df, -"segments"))

  new_nested_analysis(nchclust, "nested_hclust")
}

#' @rdname nested_hclust
#' @export
nested_chclust_conslink <- function(.data, data_column = "data", qualifiers_column = "qualifiers",
                                    distance_fun = stats::dist,
                                    n_groups = NULL, ...) {
  data_column <- enquo(data_column)
  qualifiers_column <- enquo(qualifiers_column)
  n_groups <- enquo(n_groups)

  nchclust <- nested_hclust(
    .data,
    data_column = !!data_column,
    qualifiers_column = !!qualifiers_column,
    distance_fun = distance_fun,
    n_groups = !!n_groups,
    .fun = rioja::chclust,
    method = "conslink",
    ...
  )

  new_nested_analysis(nchclust, c("nested_chclust_conslink", "nested_chclust"))
}

#' @rdname nested_hclust
#' @export
nested_chclust_coniss <- function(.data, data_column = "data", qualifiers_column = "qualifiers", distance_fun = stats::dist,
                                  n_groups = NULL, ...) {

  data_column <- enquo(data_column)
  qualifiers_column <- enquo(qualifiers_column)
  n_groups <- enquo(n_groups)

  nchclust <- nested_hclust(
    .data,
    data_column = !!data_column,
    qualifiers_column = !!qualifiers_column,
    distance_fun = distance_fun,
    n_groups = !!n_groups,
    .fun = rioja::chclust,
    method = "coniss",
    ...
  )

  # 1000 groups is an arbitrary maximum...ensures that a large number of zones could
  # be plausible
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

  new_nested_analysis(nchclust, c("nested_chclust_coniss", "nested_chclust"))
}

#' @export
#' @importFrom graphics plot
plot.nested_chclust <- function(x, ..., nrow = NULL, ncol = NULL) {
  plot_nested_analysis(x, .fun = graphics::plot, ..., nrow = nrow, ncol = ncol)
}

#' @export
#' @importFrom graphics plot
plot.nested_hclust <- function(x, ..., sub = "", xlab = "", nrow = NULL, ncol = NULL) {
  sub <- enquo(sub)
  xlab <- enquo(xlab)
  plot_nested_analysis(x, .fun = graphics::plot, sub = !!sub, xlab = !!xlab, ..., nrow = nrow, ncol = ncol)
}

#' A ggplot2-based dendrogram plot/layer
#'
#' A quick-and-dirty ggplotter for \link{nested_hclust} objects. Use \code{tidyr::unnest(object, segments)} and
#' \link[ggplot2]{geom_segment} to further customize the apperance of this plot.
#'
#' @param object A \link{nested_hclust}
#' @param mapping A mapping created with \link[ggplot2]{aes}. Must map x and xend. Use flip to flip the axes.
#' @param label An expression evaluated in \code{tidyr::unnest(object, nodes)} that nodes are labelled with by default.
#'   Note that this may label non-leaf nodes.
#' @param segment_geom,node_geom Override the default geometries for segments and nodes, respectively
#' @param ... Ignored
#' @param nrow,ncol Passed to \link[ggplot2]{facet_wrap}
#' @param flip Use to switch x/y aesthetics
#'
#' @importFrom ggplot2 autolayer
#' @export
autoplot.nested_hclust <- function(object, ..., nrow = NULL, ncol = NULL) {
  group_vars <- get_grouping_vars(object)
  ggplot2::ggplot() +
    ggplot2::autolayer(object, ...) +
    if(length(group_vars) > 0) ggplot2::facet_wrap(do.call(ggplot2::vars, rlang::syms(group_vars)), nrow = nrow, ncol = ncol)
}

#' @importFrom ggplot2 autolayer
#' @export
#' @rdname autoplot.nested_hclust
autolayer.nested_hclust <- function(
  object,
  mapping = ggplot2::aes(x = .data$dendro_order, xend = .data$dendro_order_end),
  label = ifelse(.data$is_leaf, .data$row_number, NA),
  segment_geom = ggplot2::geom_segment(),
  node_geom = ggplot2::geom_text(
    ggplot2::aes(label = .data$auto_label),
    angle = ifelse(flip, 0, 90),
    hjust = 1, vjust = 0.5,
    na.rm = TRUE
  ),
  flip = FALSE,
  ...
) {
  label <- enquo(label)

  stopifnot(
    inherits(mapping, "uneval"),
    all(c("x", "xend") %in% names(mapping))
  )

  if(flip) {
    mapping <- mapping[c("x", "xend", setdiff(names(mapping), c("x", "xend")))]
    names(mapping) <- c("y", "yend", setdiff(names(mapping), c("x", "xend")))
    default_mapping <- ggplot2::aes(x = .data$dispersion, xend = .data$dispersion_end)
  } else {
    default_mapping <- ggplot2::aes(y = .data$dispersion, yend = .data$dispersion_end)
  }

  mapping <- c(mapping, default_mapping)
  mapping <- mapping[unique(names(mapping))]
  class(mapping) <- "uneval"

  segments <- tidyr::unnest(object, .data$segments)
  nodes <- tidyr::unnest(object, .data$nodes)
  nodes <- dplyr::mutate(nodes, auto_label = !!label)

  list(
    override_data(segment_geom, data = segments, mapping = mapping),
    override_data(node_geom, data = nodes, mapping = mapping[setdiff(names(mapping), c("xend", "yend"))])
  )
}


group_boundaries <- function(hclust_zones, qualifiers, n_groups = 1) {
  stopifnot(
    length(hclust_zones) == nrow(qualifiers),
    is.numeric(n_groups), length(n_groups) == 1, n_groups > 0
  )

  qualifiers$hclust_zone <- hclust_zones
  group_info <- tidyr::nest(
    dplyr::group_by(qualifiers, .data$hclust_zone),
    .key = "data"
  )

  for(var in setdiff(colnames(qualifiers), "hclust_zone")) {
    if(is.numeric(qualifiers[[var]])) {
      group_info[[paste("min", var, sep = "_")]] <- purrr::map_dbl(group_info$data, function(df) min(df[[var]]))
      group_info[[paste("max", var, sep = "_")]] <- purrr::map_dbl(group_info$data, function(df) max(df[[var]]))
    }
  }

  group_info$data <- NULL

  for(var in setdiff(colnames(qualifiers), "hclust_zone")) {
    if(is.numeric(qualifiers[[var]])) {
      group_info[[paste("boundary", var, sep = "_")]] <-
        (group_info[[paste("max", var, sep = "_")]] + dplyr::lead(group_info[[paste("min", var, sep = "_")]])) / 2
    }
  }

  group_info
}

determine_n_groups <- function(model, threshold = 1.1) {

  if(inherits(model, "chclust") && (model$method == "coniss")) {
    # slightly duplicated work, but allows hclust and chclust to share more code
    broken_stick <- rioja::bstick(model, plot = FALSE, ng = 1000)
    broken_stick <- dplyr::filter(broken_stick, !is.na(.data$dispersion))

    # ensures there is at least one group greater than broken stick dispersion
    # at the beginning
    broken_stick <- tibble::add_row(broken_stick, nGroups = 1, dispersion = Inf, bstick = 0, .before = 1)

    broken_stick$disp_gt_bstick <- broken_stick$dispersion > (broken_stick$bstick * threshold)
    runlength <- rle(broken_stick$disp_gt_bstick)
    broken_stick$nGroups[runlength$lengths[1]]
  } else {
    1
  }
}

qualify_dendro_data <- function(model, qualifiers, hclust_zones) {
  qualifiers$dendro_order <- match(seq_along(model$order), model$order)
  numeric_vars <- colnames(qualifiers)[purrr::map_lgl(qualifiers, is.numeric)]
  node_data(stats::as.dendrogram(model), qualifiers, numeric_vars, hclust_zones)
}

node_data <- function(node, qualifiers, numeric_vars, hclust_zones, recursive_level = 1, node_id = 1) {

  if(stats::is.leaf(node)) {
    data1 <- tibble::tibble()
    data2 <- tibble::tibble()

    data <- dplyr::slice(qualifiers, as.numeric(attr(node, "label")))
    data$hclust_zone <- hclust_zones[attr(node, "label")]
    data$is_leaf <- TRUE
    data$segments <- list(tibble::tibble())
    data$dispersion <- attr(node, "height")
  } else {

    # includes rows for all child nodes
    data1 <- node_data(
      node[[1]], qualifiers, numeric_vars, hclust_zones,
      recursive_level + 1, node_id = node_id + 1
    )
    data2 <- node_data(
      node[[2]], qualifiers, numeric_vars, hclust_zones,
      recursive_level + 1, node_id = max(data1$node_id) + 1
    )

    # first row is always the last node
    n1 <- dplyr::slice(data1, 1)
    n2 <- dplyr::slice(data2, 1)

    data <- (n1[numeric_vars] + n2[numeric_vars]) / 2
    data$hclust_zone <- dplyr::if_else(
      n1$hclust_zone == n2$hclust_zone,
      true = n1$hclust_zone,
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
