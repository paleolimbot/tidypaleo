
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
