
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
