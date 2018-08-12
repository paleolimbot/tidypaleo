
#' Add a dendrogram as a layer or facet
#'
#' @param object A \link{nested_hclust} object.
#' @param mapping Map at least one axis (x or y) to a qualifier, like \code{aes(x = depth)} or similar.
#' @param segment_geom Pass some variant of \link[ggplot2]{geom_segment} to customize geometry options
#' @param sequential_facets TRUE will result in the panel containing the dendrogram added to the right
#'   of the plot.
#' @param lty,alpha,colour,size Customize the apperance of boundary lines
#' @param ... Use facet_var = "CONISS" or similar to name the panel
#'
#' @export
#'
layer_dendrogram <- function(object, mapping, ..., segment_geom = ggplot2::geom_segment(inherit.aes = FALSE),
                             sequential_facets = TRUE) {
  stopifnot(
    xor("x" %in% names(mapping), "y" %in% names(mapping))
  )

  mutate_args <- quos(...)

  if("x" %in% names(mapping)) {
    default_mapping <- ggplot2::aes(
      xend = !!rlang::sym(paste(rlang::quo_name(mapping$x), "end", sep = "_")),
      y = .data$dispersion,
      yend = .data$dispersion_end
    )
  } else {
    default_mapping <- ggplot2::aes(
      yend = !!rlang::sym(paste(rlang::quo_name(mapping$y), "end", sep = "_")),
      x = .data$dispersion,
      xend = .data$dispersion_end
    )
  }

  mapping <- override_mapping(mapping, default_mapping)

  segments <- tidyr::unnest(object, .data$segments)
  segments <- dplyr::mutate(segments, !!!mutate_args)

  list(
    override_data(segment_geom, data = segments, mapping = mapping),
    if(sequential_facets) sequential_layer_facets()
  )
}

#' @rdname layer_dendrogram
#' @export
layer_zone_boundaries <- function(object, mapping, ..., lty = 2, alpha = 0.7, colour = "black", size = 0.5) {
  stopifnot(
    xor("x" %in% names(mapping), "y" %in% names(mapping)),
    "zone_info" %in% names(object), is.list(object$zone_info)
  )

  mutate_args <- quos(...)
  zone_info <- tidyr::unnest(object, .data$zone_info)
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
    lty = lty, alpha = alpha, colour = colour, size = size,
    na.rm = TRUE
  )
}


#' Change facet ordering behaviour
#'
#' Normally, facets are ordered using \link[base]{as.factor} on all values that occur
#' within layer data, which means that when adding additional layers, any ordering
#' is not preserved unless the factor levels are identical on all factors. This function
#' changes this behaviour such that facet levels are combined in layer order. This is
#' useful when adding standalone layers to a plot without disturbing the existing order.
#'
#' @param reverse Use TRUE to process layers in reverse order
#'
#' @export
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
