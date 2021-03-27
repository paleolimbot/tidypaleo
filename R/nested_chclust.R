
#' Nested (Constrained) hierarchical clustering
#'
#' Powered by [chclust][rioja::chclust] and [hclust][stats::hclust]; broken stick using [bstick][rioja::bstick].
#'
#' @inheritParams nested_analysis
#' @param data_column An expression that evalulates to the data object within each row of .data
#' @param distance_fun A distance function like [dist][stats::dist] or [vegdist][vegan::vegdist].
#' @param qualifiers_column The column that contains the qualifiers
#' @param n_groups The number of groups to use (can be a vector or expression using vars in .data)
#' @param .fun Function powering the clustering. Must return an hclust object of some kind.
#' @param ... Passed to [chclust][rioja::chclust] or [hclust][stats::hclust].
#'
#' @return `.data` with additional columns
#' @export
#'
#' @references
#'
#' Bennett, K. (1996) Determination of the number of zones in a biostratigraphic sequence.
#' New Phytologist, 132, 155-170.
#' \doi{10.1111/j.1469-8137.1996.tb04521.x} (Broken stick)
#'
#' Grimm, E.C. (1987) CONISS: A FORTRAN 77 program for stratigraphically constrained cluster
#' analysis by the method of incremental sum of squares. Computers & Geosciences, 13, 13-35.
#' \doi{10.1016/0098-3004(87)90022-7}
#'
#' Juggins, S. (2017) rioja: Analysis of Quaternary Science Data, R package version (0.9-15.1).
#' (<https://cran.r-project.org/package=rioja>).
#'
#' See [hclust][stats::hclust] for hierarchical clustering references
#'
#' @examples
#' library(tidyr)
#' library(dplyr, warn.conflicts = FALSE)
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
#'   select(location, broken_stick) %>%
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
    function(df) tidyr::unnest(
      dplyr::select(df, "node_id", "hclust_zone", "segments"),
      .data$segments
    )
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

#' Display a dendrogram as a ggplot2 layer
#'
#' @param mapping A mapping created using [aes][ggplot2::aes]. Must map x OR y to a qualifier.
#' @param data A [nested_hclust] object
#' @param position Position adjustment
#' @param geom Any geom that takes x, xend, y, and yend. Probably [geom_segment][ggplot2::geom_segment] is
#'   the only one that makes sense.
#' @param ... Passed to the the stat/geom (see [geom_segment][ggplot2::geom_segment])
#' @param inherit.aes Inherit aesthetics from ggplot()?
#' @param show.legend Show mapped aesthetics in the legend?
#'
#' @return A [ggplot2::Stat]
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' alta_coniss <- nested_data(
#'   alta_lake_geochem,
#'   qualifiers = c(age, depth, zone),
#'   key = param,
#'   value = value,
#'   trans = scale
#' ) %>%
#'   nested_chclust_coniss()
#'
#' ggplot(alta_coniss) +
#'   stat_nested_hclust(aes(model = model, y = depth)) +
#'   scale_y_reverse()
#'
stat_nested_hclust <- function(mapping = NULL, data = NULL,
                               geom = "segment", position = "identity",
                               ...,
                               inherit.aes = TRUE, show.legend = NA) {

  ggplot2::layer(
    geom = geom,
    stat = StatNestedHclust,
    mapping = override_mapping(mapping, ggplot2::aes(model = .data$model, group = .data$..group)),
    data = data,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    position = position,
    params = list(...)
  )
}

#' @rdname stat_nested_hclust
#' @export
StatNestedHclust <- ggplot2::ggproto(
  "StatNestedHclust",
  ggplot2::Stat,
  required_aes = "model",
  default_aes = ggplot2::aes(x = NA_real_, y = NA_real_),
  compute_group = function(self, data, scales) {
    # only compute stat once!
    if(is.null(data$model)) return(data)

    model <- data$model[[1]]
    data$model <- NULL

    if(nrow(data) != length(model$order)) {
      stop("Group size is not equal to the number of observations in the model. Did you need to set the group aesthetic?")
    }

    # propogate aesthetics through the calculation
    aesthetics <- dplyr::slice(data, 1)
    aesthetics$x <- NULL
    aesthetics$y <- NULL

    if(is.null(data$y) || all(is.na(data$y))) {
      if(is.null(data$x) || all(is.na(data$x))) stop("One of 'x' or 'y' must be mapped")

      # x was mapped, y is dispersion
      attributes(data$x) <- NULL # must remove "mapped_discrete" class + attrs, or dplyr may fail
      nodes <- qualify_dendro_data(model, data["x"], 1L)
      segments <- dplyr::bind_rows(nodes$segments)
      segments <- dplyr::rename(segments, y = "dispersion", yend = "dispersion_end", xend = "x_end")

      # map dispersion to y scale
      if(!is.null(scales$y)) {
        segments$y <- scales$y$transform(segments$y)
        segments$yend <- scales$y$transform(segments$yend)
      }
    } else if(is.null(data$x) || all(is.na(data$x))) {

      # y was mapped, x is dispersion
      attributes(data$y) <- NULL # must remove "mapped_discrete" class + attrs, or dplyr may fail
      nodes <- qualify_dendro_data(model, data["y"], 1L)
      segments <- dplyr::bind_rows(nodes$segments)
      segments <- dplyr::rename(segments, x = "dispersion", xend = "dispersion_end", yend = "y_end")

      # map dispersion to x scale
      if(!is.null(scales$x)) {
        segments$x <- scales$x$transform(segments$x)
        segments$xend <- scales$x$transform(segments$xend)
      }
    } else {
      stop("One of 'x' or 'y' must be mapped")
    }

    # recombine aesthetics with data
    dplyr::bind_cols(segments, aesthetics[rep(1, nrow(segments)), ])
  }
)

# this is needed so that the object can be passed into ggplot() or some other geom function
#' @importFrom ggplot2 fortify
#' @export
fortify.nested_hclust <- function(model, data, ...) {
  # need some data pre-processing to be stat_nested_hclust friendly
  data <- model[c(get_grouping_vars(model), "qualifiers", "model")]

  # default group should be by model
  data$..group <- purrr::map_chr(data$model, digest::digest, "md5")

  # add unnested qualifier columns to data prior to mapping
  # model, x, and y column must be mapped
  tidyr::unnest(data, .data$qualifiers)
}

group_boundaries <- function(hclust_zones, qualifiers, n_groups = 1) {
  stopifnot(
    length(hclust_zones) == nrow(qualifiers),
    is.numeric(n_groups), length(n_groups) == 1, n_groups > 0
  )

  qualifiers$hclust_zone <- hclust_zones
  group_info <- tidyr::nest(
    dplyr::group_by(qualifiers, .data$hclust_zone)
  )

  for(var in setdiff(colnames(qualifiers), "hclust_zone")) {
    if(is.numeric(qualifiers[[var]])) {
      group_info[[paste("min", var, sep = "_")]] <- purrr::map_dbl(group_info$data, function(df) min(df[[var]]))
      group_info[[paste("max", var, sep = "_")]] <- purrr::map_dbl(group_info$data, function(df) max(df[[var]]))
      group_info[[paste("first", var, sep = "_")]] <- purrr::map_dbl(group_info$data, function(df) dplyr::first(df[[var]]))
      group_info[[paste("last", var, sep = "_")]] <- purrr::map_dbl(group_info$data, function(df) dplyr::last(df[[var]]))
    }
  }

  group_info$data <- NULL

  for(var in setdiff(colnames(qualifiers), "hclust_zone")) {
    if(is.numeric(qualifiers[[var]])) {
      group_info[[paste("boundary", var, sep = "_")]] <-
        (group_info[[paste("last", var, sep = "_")]] + dplyr::lead(group_info[[paste("first", var, sep = "_")]])) / 2
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
