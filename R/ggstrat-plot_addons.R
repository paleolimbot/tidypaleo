
#' Add scores to a plot
#'
#' @param object A [nested_prcomp] or similar object
#' @param mapping A mapping created with [aes][ggplot2::aes]
#' @param which Which principal components to plot
#' @param key The column name to use for the principal component names
#' @param value The column name to use for the principal component score values
#' @param scores_geom One or more geometries to which scores should be applied.
#' @param sequential_facets TRUE will result in the panel containing the dendrogram added to the right
#'   of the plot.
#' @param ... Passed to layer_scores()
#'
#' @return A `list()` that can be addeed to a [ggplot2::ggplot()]
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr, warn.conflicts = FALSE)
#'
#' alta_pca <- nested_data(
#'   alta_lake_geochem,
#'   qualifiers = c(age, depth, zone),
#'   key = param,
#'   value = value,
#'   trans = scale
#' ) %>%
#'   nested_prcomp()
#'
#' ggplot() +
#'   layer_scores(alta_pca, aes(value, depth), which = "PC1") +
#'   scale_y_reverse()
#'
#' plot_layer_scores(alta_pca, aes(y = depth), which = c("PC1", "PC2")) +
#'   scale_y_reverse()
#'
layer_scores <- function(
  object, mapping = NULL, which = "PC1", key = "param", value = "value",
  scores_geom = list(ggplot2::geom_path(), ggplot2::geom_point()),
  sequential_facets = TRUE
) {
  value <- enquo(value)
  key <- enquo(key)

  object$scores <- purrr::map(object$scores, function(df) dplyr::select(df, !!which))
  scores <- tidyr::unnest(
    drop_list_cols(object, c("qualifiers",  "scores")),
    c(.data$qualifiers, .data$scores)
  )
  scores_long <- tidyr::gather(scores, key = !!key, value = !!value, dplyr::starts_with("PC"))

  list(
    override_data(scores_geom, data = scores_long, mapping = mapping),
    if(sequential_facets) sequential_layer_facets()
  )
}

#' @rdname layer_scores
#' @export
plot_layer_scores <- function(object, mapping, which = "PC1", key = "param", value = "value", ...) {

  stopifnot(
    identical(key, "param"),
    identical(value, "value"),
    xor("x" %in% names(mapping), "y" %in% names(mapping))
  )

  group_vars <- get_grouping_vars(object)
  var_groups <- do.call(ggplot2::vars, rlang::syms(group_vars))

  mapping_vals <- purrr::map(mapping, rlang::quo_name)

  if("y" %in% names(mapping)) {
    mapping <- override_mapping(ggplot2::aes(x = .data$value), mapping)
    facet <- ggplot2::facet_grid(rows = var_groups, cols = ggplot2::vars(!!rlang::sym("param")))
  } else {
    mapping <- override_mapping(ggplot2::aes(y = .data$value), mapping)
    facet <- ggplot2::facet_grid(cols = var_groups, rows = ggplot2::vars(!!rlang::sym("param")))
  }

  ggplot2::ggplot() + layer_scores(object, mapping, which, "param", "value", ...) + facet
}

#' Add a dendrogram as a layer or facet
#'
#' @param object A [nested_hclust] object.
#' @param mapping Map at least one axis (x or y) to a qualifier, like `aes(x = depth)` or similar.
#' @param sequential_facets TRUE will result in the panel containing the dendrogram added to the right
#'   of the plot.
#' @param linetype,alpha,colour,size Customize the apperance of boundary/dendrogram segment lines
#' @param panel_label Use to label a pane on a stanalone dendrogram plot
#' @param ... Use facet_var = "CONISS" or similar to name the panel
#'
#' @return A [ggplot2::layer()]
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
#' ggplot() +
#'   layer_dendrogram(alta_coniss, aes(y = depth)) +
#'   scale_y_reverse()
#'
layer_dendrogram <- function(object, mapping,
                             ...,
                             colour = "black", size = 0.5, linetype = 1, alpha = NA,
                             sequential_facets = TRUE) {
  stopifnot(
    xor("x" %in% names(mapping), "y" %in% names(mapping))
  )

  mutate_args <- quos(...)
  data <- ggplot2::fortify(object)
  data <- dplyr::mutate(data, !!!mutate_args)

  list(
    stat_nested_hclust(mapping = mapping, data = data, colour = colour, size = size,
                       linetype = linetype, alpha = alpha, inherit.aes = FALSE),
    if(sequential_facets) sequential_layer_facets()
  )
}

#' @rdname layer_dendrogram
#' @export
plot_layer_dendrogram <- function(object, mapping, ..., panel_label = "CONISS") {
  group_vars <- get_grouping_vars(object)
  var_groups <- do.call(ggplot2::vars, rlang::syms(group_vars))

  if("x" %in% names(mapping)) {
    facet <- ggplot2::facet_grid(cols = var_groups, rows = ggplot2::vars(!!panel_label))
  } else {
    facet <- ggplot2::facet_grid(rows = var_groups, cols = ggplot2::vars(!!panel_label))
  }

  ggplot2::ggplot() + layer_dendrogram(object, mapping, ...) + facet
}

#' @rdname layer_dendrogram
#' @export
layer_zone_boundaries <- function(object, mapping, ..., linetype = 2, alpha = 0.7, colour = "black", size = 0.5) {
  stopifnot(
    xor("x" %in% names(mapping), "y" %in% names(mapping)),
    "zone_info" %in% names(object), is.list(object$zone_info)
  )

  mutate_args <- quos(...)
  zone_info <- tidyr::unnest(drop_list_cols(object,  "zone_info"), .data$zone_info)
  zone_info <- dplyr::mutate(zone_info, !!!mutate_args)

  if("x" %in% names(mapping)) {
    new_mapping <- ggplot2::aes(xintercept = !!rlang::sym(paste("boundary", rlang::quo_name(mapping$x), sep = "_")))
    mapping$x <- NULL
    geom <- ggplot2::geom_vline
  } else {
    new_mapping <- ggplot2::aes(yintercept = !!rlang::sym(paste("boundary", rlang::quo_name(mapping$y), sep = "_")))
    mapping$y <- NULL
    geom <- ggplot2::geom_hline
  }

  geom(
    mapping = override_mapping(new_mapping, mapping),
    data = zone_info,
    linetype = linetype, alpha = alpha, colour = colour, size = size,
    na.rm = TRUE
  )
}


#' Change facet ordering behaviour
#'
#' Normally, facets are ordered using [as.factor][base::as.factor] on all values that occur
#' within layer data, which means that when adding additional layers, any ordering
#' is not preserved unless the factor levels are identical on all factors. This function
#' changes this behaviour such that facet levels are combined in layer order. This is
#' useful when adding standalone layers to a plot without disturbing the existing order.
#'
#' @param reverse Use TRUE to process layers in reverse order
#'
#' @return An object that can be added to a [ggplot2::ggplot()]
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mapping = aes(x, y)) +
#'   geom_point(data = data.frame(x = 1:5, y = 1:5, facet = "b")) +
#'   geom_point(data = data.frame(x = 1:5, y = 1:5, facet = "a")) +
#'   facet_wrap(vars(facet))
#'
#' p
#' p + sequential_layer_facets()
#'
sequential_layer_facets <- function(reverse = FALSE) {
  structure(list(reverse = reverse), class = "sequential_layer_facet_spec")
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.sequential_layer_facet_spec <- function(object, plot, object_name) {
  force(object)
  facet_super <- plot$facet

  plot$facet <- ggplot2::ggproto(
    NULL,
    facet_super,
    compute_layout = function(self, data, params) {

      # this will only work if there is an existing column that matches the name
      # of the facets (i.e., the user didn't use an expression, just a bare name)
      vars <- params$facets  # facet_wrap
      rows <- params$rows  # facet_grid
      cols <- params$cols  # facet_grid
      all_facet_names <- c(names(vars), names(rows), names(cols))

      if(length(all_facet_names) > 0) {

        # find all facet levels as they will appear in the final data
        # (sorted by default using as.factor(), or existing levels if it is already a factor)
        data_process <- data
        if(identical(object$reverse, TRUE)) {
          data_process <- rev(data)
        } else {
          data_process <- data
        }

        facet_levels <- lapply(data, function(d) {
          layer_facet_factors <- lapply(d[intersect(names(d), all_facet_names)], as.factor)
          lapply(layer_facet_factors, levels)
        })

        names(all_facet_names) <- all_facet_names
        facet_levels_all <- lapply(all_facet_names, function(col) {
          unique(do.call(c, lapply(facet_levels, `[[`, col)))
        })

        data <- lapply(data, function(d) {
          layer_facets <- intersect(names(d), all_facet_names)
          d[layer_facets] <- lapply(
            layer_facets,
            function(col_name) factor(d[[col_name]], levels = facet_levels_all[[col_name]])
          )

          d
        })
      }

      # call in the super
      ggplot2::ggproto_parent(facet_super, self)$compute_layout(data, params)
    }
  )

  plot
}
