
#' Common plot modifications for stratigraphic plots
#'
#' @param angle The angle at which labels should be rotated
#' @param direction The axes along which the operations should be performed
#' @param remove_label_background Whether or not label backgrounds should be removed along
#'   rotated label axes
#'
#' @return An object or list of objects that can be added to a \link[ggplot2]{ggplot}
#' @export
#'
rotated_facet_labels <- function(angle = 45, direction = "x", remove_label_background = TRUE) {
  stopifnot(
    all(direction %in% c("x", "y")),
    is.numeric(angle), length(angle) == 1,
    is.logical(remove_label_background), length(remove_label_background) == 1
  )

  theme_mods <- ggplot2::theme()

  if("x" %in% direction) {
    theme_mods <- theme_mods +
      theme_modify_paleo(rotate_labels_x = angle, remove_label_background_x = remove_label_background)
  }

  if("y" %in% direction) {
    theme_mods <- theme_mods +
      theme_modify_paleo(rotate_labels_y = angle, remove_label_background_y = remove_label_background)
  }

  list(
    theme_mods,
    remove_label_clip(direction = direction)
  )
}

#' @rdname rotated_facet_labels
#' @export
remove_label_clip <- function(direction) {
  force(direction)
  structure(
    list(
      modify_plot = function(plot) {
        plot$facet <- modify_facet_clip(plot$facet, remove_clip = direction)
        plot
      }
    ),
    class = "paleo_hook"
  )
}

#' @rdname rotated_facet_labels
#' @export
rotated_axis_labels <- function(angle = 90, direction = "x") {

  stopifnot(
    all(direction %in% c("x", "y")),
    is.numeric(angle), length(angle) == 1
  )

  theme_mods <- ggplot2::theme()

  if("x" %in% direction) {
    theme_mods <- theme_mods +
      theme_modify_paleo(rotate_axis_labels_x = angle)
  }

  if("y" %in% direction) {
    theme_mods <- theme_mods +
      theme_modify_paleo(rotate_axis_labels_y = angle)
  }

  theme_mods
}

#' Internal theme modification code
#'
#' @param rotate_labels_x Rotate top/bottom facet labels (degrees counterclockwise)
#' @param rotate_labels_y Rotate right/left facet labels (degrees counterclockwise)
#' @param rotate_axis_labels_x Rotate top/bottom axis labels (degrees counterclockwise)
#' @param rotate_axis_labels_y Rotate right/left axis labels (degrees counterclockwise)
#' @param remove_label_background Remove the background of the facet label
#' @param pad_right_inches Give the plot extra right padding
#'
#' @return A partial \link[ggplot2]{theme} object.
#' @noRd
#'
theme_modify_paleo <- function(rotate_labels_x = NULL, rotate_labels_y = NULL, remove_label_background_x = FALSE,
                               remove_label_background_y = FALSE, rotate_axis_labels_x = NULL,
                               rotate_axis_labels_y = NULL, pad_right_inches = NULL) {
  theme_elements <- list(
    strip.text.x = if(!is.null(rotate_labels_x)) ggplot2::element_text(
      angle = rotate_labels_x,
      hjust = if(rotate_labels_x > 0) 0 else if(rotate_labels_x < 0) 1 else 0.5,
      vjust = if(abs(rotate_labels_x) == 90) 0.5 else if(rotate_labels_x > 0) 0 else if(rotate_labels_x < 0) 1.1 else 0.5
    ),
    strip.text.y = if(!is.null(rotate_labels_y)) ggplot2::element_text(
      angle = rotate_labels_y,
      hjust = if(abs(rotate_labels_y) == 90) 0.5 else 0,
      vjust = if(rotate_labels_y > 0) 0 else if(rotate_labels_y < 0) 0 else 0.5
    ),
    axis.text.x.bottom = if(!is.null(rotate_axis_labels_x)) ggplot2::element_text(
      angle = rotate_axis_labels_x,
      hjust = if(rotate_axis_labels_x > 0) 1 else if(rotate_axis_labels_x < 0) 0 else 0.5,
      vjust = if(abs(rotate_axis_labels_x) == 90) 0.5 else if(abs(rotate_axis_labels_x) != 0) 1 else 0.5
    ),
    axis.text.y.left = if(!is.null(rotate_axis_labels_y)) ggplot2::element_text(
      angle = rotate_axis_labels_y,
      hjust = if(abs(rotate_axis_labels_y) == 90) 0.5 else 1,
      vjust = if(rotate_axis_labels_y == 0) 0.5 else 1
    ),
    axis.text.x.top = if(!is.null(rotate_axis_labels_x)) ggplot2::element_text(
      angle = rotate_axis_labels_x,
      hjust = if(rotate_axis_labels_x > 0) 0 else if(rotate_axis_labels_x < 0) 1 else 0.5,
      vjust = if(abs(rotate_axis_labels_x) == 90) 0.5 else if(abs(rotate_axis_labels_x) != 0) 0 else 0.5
    ),
    axis.text.y.right = if(!is.null(rotate_axis_labels_y)) ggplot2::element_text(
      angle = rotate_axis_labels_y,
      hjust = if(abs(rotate_axis_labels_y) == 90) 0.5 else 0,
      vjust = if(rotate_axis_labels_y == 0) 0.5 else 0
    ),
    strip.background.x = if(remove_label_background_x) ggplot2::element_blank(),
    strip.background.y = if(remove_label_background_y) ggplot2::element_blank(),
    plot.margin = if(!is.null(pad_right_inches)) grid::unit(c(0, pad_right_inches, 0, 0), "inches")
  )

  # purrr::compact() erroneously gets rid of element_blank() objects
  rlang::invoke(ggplot2::theme, theme_elements[!vapply(theme_elements, is.null, logical(1))])
}

#' Internal facet modification code
#'
#' @param facet_super_obj The current facet object
#' @param remove_clip,direction Axis along which label clipping should be removed
#'
#' @return A modified Facet, or hook that will modify the facet
#' @noRd
#'
modify_facet_clip <- function(facet_super_obj, remove_clip = NULL) {

  if(!(inherits(facet_super_obj, "FacetGrid") || inherits(facet_super_obj, "FacetWrap"))) {
    stop(
      "The current facet is not a facet_grid() or facet_wrap(). Rotate the labels after setting the facet!",
      call. = FALSE
    )
  }

  stopifnot(
    all(remove_clip %in% c("x", "y", "b", "l", "r", "t"))
  )

  ggplot2::ggproto(
    NULL,
    facet_super_obj,
    # need to override draw_panels method to remove requested clip items
    draw_panels = function(self, panels, layout, x_scales, y_scales, ranges, coord, data, theme, params) {

      # call parent method
      # super() source:
      # https://github.com/tidyverse/ggplot2/blob/master/R/facet-grid-.r#L293
      # https://github.com/tidyverse/ggplot2/blob/master/R/facet-wrap.r#L204
      #
      panel_table <- ggplot2::ggproto_parent(facet_super_obj, self)$draw_panels(
        panels, layout, x_scales, y_scales, ranges, coord, data, theme, params
      )

      # remove clip on the specified axes
      if("x" %in% remove_clip) {
        panel_table <- set_clip(panel_table, "strip-t", "off")
        panel_table <- set_clip(panel_table, "strip-b", "off")
      }

      if("y" %in% remove_clip) {
        panel_table <- set_clip(panel_table, "strip-l", "off")
        panel_table <- set_clip(panel_table, "strip-r", "off")
      }

      if("b" %in% remove_clip) {
        panel_table <- set_clip(panel_table, "strip-b", "off")
      }

      if("r" %in% remove_clip) {
        panel_table <- set_clip(panel_table, "strip-r", "off")
      }

      if("t" %in% remove_clip) {
        panel_table <- set_clip(panel_table, "strip-t", "off")
      }

      if("l" %in% remove_clip) {
        panel_table <- set_clip(panel_table, "strip-l", "off")
      }

      panel_table
    }
  )
}

#' @noRd
set_clip <- function(panel_table, regex, value) {
  for(i in which(grepl(regex, panel_table$layout$name))){
    panel_table$grobs[[i]]$layout$clip <- value
  }

  panel_table
}

#' Infrastructure for special methods
#'
#' These methods provide the infrastructure necessary to rotate facet labels and set
#' multiple scales.
#'
#' @param plot A ggplot object
#' @param object,object_name ggplot_add arguments
#'
#' @return A ggplot object
#' @importFrom ggplot2 ggplot_add
#' @export
#'
ggplot_add.paleo_hook <- function(object, plot, object_name) {
  object$modify_plot(plot)
}
