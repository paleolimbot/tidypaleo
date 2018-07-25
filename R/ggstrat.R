
#' Facet for relative abundance data
#'
#' @param taxon A call to \link[ggplot2]{vars}, defining the column that identifies the taxon.
#' @param grouping  A call to \link[ggplot2]{vars}, identifying additional grouping columns
#' @param rotate_facet_labels Facet label rotation (degrees)
#'
#' @export
#'
facet_abundanceh <- function(taxon, grouping = NULL, rotate_facet_labels = 45) {
  list(
    scale_x_abundance(),
    ggplot2::facet_grid(rows = grouping, cols = taxon, scales = "free_x", space = "free_x"),
    rotated_facet_labels(angle = rotate_facet_labels, direction = "x")
  )
}

#' @rdname facet_abundanceh
#' @export
facet_abundance <- function(taxon, grouping = NULL, rotate_facet_labels = 0) {
  list(
    scale_y_abundance(),
    ggplot2::facet_grid(rows = taxon, cols = grouping, scales = "free_y", space = "free_y"),
    rotated_facet_labels(angle = rotate_facet_labels, direction = "y")
  )
}

#' @importFrom ggplot2 vars
#' @export
ggplot2::vars

#' Useful geometries for strat diagrams
#'
#' @param mapping,data,stat,position,arrow,arrow.fill,lineend,linejoin,na.rm,show.legend,inherit.aes,... See
#'   \link[ggplot2]{geom_segment}.
#' @param xend,yend The end of the horizontal or vertical segment bars, respectively.
#'
#' @return A ggplot2 layer
#' @export
#'
geom_col_segsh <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            xend = 0,
                            arrow = NULL,
                            arrow.fill = NULL,
                            lineend = "butt",
                            linejoin = "round",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomColSegsh,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      xend = xend,
      ...
    )
  )
}

#' @rdname geom_col_segsh
#' @export
geom_col_segs <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            yend = 0,
                            arrow = NULL,
                            arrow.fill = NULL,
                            lineend = "butt",
                            linejoin = "round",
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomColSegs,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      yend = yend,
      ...
    )
  )
}

#' @rdname geom_col_segsh
#' @export
GeomColSegsh <- ggplot2::ggproto(
  "GeomColSegsh",
  ggplot2::GeomSegment,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(xend = 0, yend = 0, colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    data$yend <- data$y

    ggplot2::ggproto_parent(ggplot2::GeomSegment, self)$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm
    )
  }
)

#' @rdname geom_col_segsh
#' @export
GeomColSegs <- ggplot2::ggproto(
  "GeomColSegs",
  ggplot2::GeomSegment,
  required_aes = c("x", "y"),
  default_aes = ggplot2::aes(xend = 0, yend = 0, colour = "black", size = 0.5, linetype = 1, alpha = NA),

  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    data$xend <- data$x

    ggplot2::ggproto_parent(ggplot2::GeomSegment, self)$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      arrow = arrow,
      arrow.fill = arrow.fill,
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm
    )
  }
)

#' Connect observations in the vertical direction
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... See
#' \link[ggplot2]{geom_line}.
#'
#' @return A ggplot2 layer.
#' @export
#'
geom_lineh <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomLineh,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_col_segsh
#' @export
GeomLineh <- ggplot2::ggproto(
  "GeomLineh",
  ggplot2::GeomPath,
  setup_data = function(data, params) {
    data[order(data$PANEL, data$group, data$y), ]
  }
)

#' @importFrom ggstance position_dodge2v
#' @export
ggstance::position_dodge2v

#' @importFrom ggstance position_dodgev
#' @export
ggstance::position_dodgev

#' @importFrom ggstance geom_colh
#' @export
ggstance::geom_colh

#' Scales for relative abundance values
#'
#' Continuous scales that (1) always start at 0, (2) always have the same breaks, and
#' (3) expand using a constant rather than a percentage. These scales assume that data are
#' in percentages (i.e., range 0 to 100 rather than 0 to 1).
#'
#' @param ... Passed to \link[ggplot2]{scale_y_continuous} or \link[ggplot2]{scale_x_continuous}
#' @param limits Limits for the scale
#' @param breaks Where to place labels on the scale
#' @param minor_breaks Where to place minor breaks
#' @param expand A vector of expantion constants
#'
#' @return A \link[ggplot2]{scale_y_continuous} or \link[ggplot2]{scale_x_continuous}
#' @export
#'
scale_x_abundance <- function(..., limits = c(0, NA), breaks = seq(10, 90, 30),
                              minor_breaks = seq(0, 100, 10), expand = c(0, 1)) {
  ggplot2::scale_x_continuous(..., limits = limits, breaks = breaks, expand = expand, minor_breaks = minor_breaks)
}

#' @rdname scale_x_abundance
#' @export
scale_y_abundance <- function(..., limits = c(0, NA), breaks = seq(10, 90, 30),
                              minor_breaks = seq(0, 100, 10), expand = c(0, 1)) {
  ggplot2::scale_y_continuous(..., limits = limits, breaks = breaks, expand = expand, minor_breaks = minor_breaks)
}



#' Age-depth scales
#'
#' @param model An age-depth model, or NULL to suppress the second axis
#' @param reversed Reverse the primary age axis (for years BP or similar)
#' @param age_name,depth_name Label for the second axis
#' @param age_breaks,depth_breaks Breaks for the second axis
#' @param ... Passed to \link[ggplot2]{scale_y_continuous} or \link[ggplot2]{scale_x_continuous}
#'
#' @return A \link[ggplot2]{scale_y_continuous} or \link[ggplot2]{scale_x_continuous}
#' @export
#' @importFrom ggplot2 waiver
#'
scale_y_depth_age <- function(model = NULL, age_name = "age", age_breaks = waiver(), ...) {
  second_axis <- age_depth_as_sec_axis(model, primary = "depth", name = age_name, breaks = age_breaks)
  ggplot2::scale_y_reverse(..., sec.axis = second_axis)
}

#' @rdname scale_y_depth_age
#' @export
scale_y_age_depth <- function(model = NULL, reversed = FALSE, depth_name = "depth", depth_breaks = waiver(), ...) {
  second_axis <- age_depth_as_sec_axis(model, primary = "age", name = depth_name, breaks = depth_breaks)
  if(reversed) {
    ggplot2::scale_y_reverse(..., sec.axis = second_axis)
  } else {
    ggplot2::scale_y_continuous(..., sec.axis = second_axis)
  }
}

#' @rdname scale_y_depth_age
#' @export
scale_x_depth_age <- function(model = NULL, age_name = "age", age_breaks = waiver(), ...) {
  second_axis <- age_depth_as_sec_axis(model, primary = "depth", name = age_name, breaks = age_breaks)
  ggplot2::scale_x_reverse(..., sec.axis = second_axis)
}

#' @rdname scale_y_depth_age
#' @export
scale_x_age_depth <- function(model = NULL, reversed = FALSE, depth_name = "depth", depth_breaks = waiver(), ...) {
  second_axis <- age_depth_as_sec_axis(model, primary = "age", name = depth_name, breaks = depth_breaks)
  if(reversed) {
    ggplot2::scale_x_reverse(..., sec.axis = second_axis)
  } else {
    ggplot2::scale_x_continuous(..., sec.axis = second_axis)
  }
}

#' Common plot modifications for stratigraphic plots
#'
#' @param angle The angle at which labels should be rotated
#' @param direction The axes along which the operations should be performed
#' @param remove_label_background Whether or not label backgrounds should be removed along
#'   rotated label axes
#'
#' @return An object or list of objects that can be added to a \link[ggplot2]{ggplot}
#' @export
#' @importFrom purrr %||%
#'
rotated_facet_labels <- function(angle = 45, direction = "x", remove_label_background = TRUE) {
  stopifnot(
    all(direction %in% c("x", "y")),
    is.numeric(angle), length(angle) == 1, angle >= -90, angle <= 90,
    is.logical(remove_label_background), length(remove_label_background) == 1
  )

  structure(
    list(
      modify_plot = function(plot) {
        plot$facet <- modify_facet_clip(plot$facet, remove_clip = direction)

        facet_switch <- plot$facet$params$switch %||% plot$facet$params$strip.position %||% "none"

        strip_position_x <- if(facet_switch %in% c("x", "both", "bottom")) "bottom" else "top"
        strip_position_y <- if(facet_switch %in% c("y", "both", "left")) "left" else "right"

        theme_mods <- ggplot2::theme()

        if("x" %in% direction) {
          theme_mods <- theme_mods +
            theme_modify_paleo(
              rotate_labels_x = angle,
              remove_label_background_x = remove_label_background,
              strip_position_x = strip_position_x
            )
        }

        if("y" %in% direction) {
          theme_mods <- theme_mods +
            theme_modify_paleo(
              rotate_labels_y = angle,
              remove_label_background_y = remove_label_background,
              strip_position_y = strip_position_y
            )
        }

        plot + theme_mods
      }
    ),
    class = "paleo_hook"
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
                               rotate_axis_labels_y = NULL, pad_right_inches = NULL,
                               strip_position_x = c("top", "bottom"), strip_position_y = c("left", "right")) {

  strip_position_x <- match.arg(strip_position_x)
  strip_position_y <- match.arg(strip_position_y)

  theme_elements <- list(
    strip.text.x = if(!is.null(rotate_labels_x) && strip_position_x == "top") {
      ggplot2::element_text(
        angle = rotate_labels_x,
        hjust = if(rotate_labels_x > 0) 0 else if(rotate_labels_x < 0) 1 else 0.5,
        vjust = if(abs(rotate_labels_x) == 90) 0.5 else if(rotate_labels_x > 0) 0 else if(rotate_labels_x < 0) 1.1 else 0.5
      )
    } else if(!is.null(rotate_labels_x) && strip_position_x == "bottom") {
      ggplot2::element_text(
        angle = rotate_labels_x,
        hjust = if(rotate_labels_x > 0) 1 else if(rotate_labels_x < 0) 0 else 0.5,
        vjust = if(abs(rotate_labels_x) == 90) 0.5 else if(rotate_labels_x > 0) 1 else if(rotate_labels_x < 0) 0 else 0.5
      )
    },
    strip.text.y = if(!is.null(rotate_labels_y) && strip_position_y == "right") {
      ggplot2::element_text(
        angle = rotate_labels_y,
        hjust = if(abs(rotate_labels_y) == 90) 0.5 else 0,
        vjust = if(rotate_labels_y == 0) 0.5 else 0
      )
    } else if(!is.null(rotate_labels_y) && strip_position_y == "left") {
      rotate_labels_y <- rotate_labels_y + 180
      ggplot2::element_text(
        angle = rotate_labels_y,
        hjust = if(abs(rotate_labels_y %% 180) == 90) 0.5 else 1,
        # rotate_labels_y can be 180 or -180 due to adjustment above
        vjust = if(rotate_labels_y %% 180 == 0) 0.5 else 1
      )
    },
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
