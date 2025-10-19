
#' Exaggerated geometries that do not train scales
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... See
#'   parent geometries
#' @param exaggerate_x,exaggerate_y The factor by which to exaggerate x or y values
#'
#' @return A subclass of [ggplot2::Geom].
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(keji_lakes_plottable, aes(x = rel_abund, y = depth)) +
#'   geom_lineh_exaggerate(exaggerate_x = 2, lty = 2) +
#'   geom_col_segsh() +
#'   scale_y_reverse() +
#'   facet_abundanceh(vars(taxon), grouping = vars(location)) +
#'   labs(y = "Depth (cm)")
#'
geom_point_exaggerate <- function(mapping = NULL, data = NULL,
                                  stat = "identity", position = "identity",
                                  ...,
                                  exaggerate_x = 1,
                                  exaggerate_y = 1,
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = exaggerated_geom(ggplot2::GeomPoint, "GeomPointExaggerate"),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      exaggerate_x = exaggerate_x,
      exaggerate_y = exaggerate_y,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_point_exaggerate
#' @export
geom_line_exaggerate <- function(mapping = NULL, data = NULL,
                                  stat = "identity", position = "identity",
                                  ...,
                                  exaggerate_x = 1,
                                  exaggerate_y = 1,
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = exaggerated_geom(ggplot2::GeomLine, "GeomLineExaggerate"),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      exaggerate_x = exaggerate_x,
      exaggerate_y = exaggerate_y,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_point_exaggerate
#' @export
geom_lineh_exaggerate <- function(mapping = NULL, data = NULL,
                                 stat = "identity", position = "identity",
                                 ...,
                                 exaggerate_x = 1,
                                 exaggerate_y = 1,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = exaggerated_geom(GeomLineh, "GeomLinehExaggerate"),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      exaggerate_x = exaggerate_x,
      exaggerate_y = exaggerate_y,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_point_exaggerate
#' @export
geom_area_exaggerate <- function(mapping = NULL, data = NULL,
                                  stat = "identity", position = "identity",
                                  ...,
                                  exaggerate_x = 1,
                                  exaggerate_y = 1,
                                  na.rm = FALSE,
                                  show.legend = NA,
                                  inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = exaggerated_geom(ggplot2::GeomArea, "GeomAreaExaggerate"),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      exaggerate_x = exaggerate_x,
      exaggerate_y = exaggerate_y,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_point_exaggerate
#' @export
geom_areah_exaggerate <- function(mapping = NULL, data = NULL,
                                 stat = "identity", position = "identity",
                                 ...,
                                 exaggerate_x = 1,
                                 exaggerate_y = 1,
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = exaggerated_geom(GeomAreah, "GeomAreahExaggerate"),
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      exaggerate_x = exaggerate_x,
      exaggerate_y = exaggerate_y,
      na.rm = na.rm,
      ...
    )
  )
}

exaggerated_geom <- function(geom_obj, geom_name = NULL) {
  ggplot2::ggproto(
    geom_name,
    geom_obj,
    draw_panel = function(self, data, panel_params, coord, exaggerate_x = 1, exaggerate_y = 1, ...) {
      x_vars <- intersect(c("x", "xmin", "xmax", "xintercept"), colnames(data))
      y_vars <- intersect(c("y", "ymin", "ymax", "yintercept"), colnames(data))
      data[x_vars] <- data[x_vars] * exaggerate_x
      data[y_vars] <- data[y_vars] * exaggerate_y

      ggplot2::ggproto_parent(geom_obj, self)$draw_panel(data, panel_params, coord, ...)
    },

    parameters = function(self, ...) {
      unique(c(ggplot2::ggproto_parent(geom_obj, self)$parameters(...), "exaggerate_x", "exaggerate_y"))
    }
  )
}


#' Useful geometries for strat diagrams
#'
#' @param mapping,data,stat,position,arrow,arrow.fill,lineend,linejoin,na.rm,show.legend,inherit.aes,... See
#'   [geom_segment][ggplot2::geom_segment].
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
  default_aes = ggplot2::aes(xend = 0, yend = 0, colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),

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
  default_aes = ggplot2::aes(xend = 0, yend = 0, colour = "black", linewidth = 0.5, linetype = 1, alpha = NA),

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
#' [geom_line][ggplot2::geom_line].
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


#' Vertical ribbons and area plots
#'
#' @param mapping,data,stat,position,na.rm,show.legend,inherit.aes,... See
#'   [geom_ribbon][ggplot2::geom_ribbon].
#'
#' @export
#' @examples
#' library(ggplot2)
#'
#' # Generate data
#' huron <- data.frame(year = 1875:1972, level = as.vector(LakeHuron))
#' h <- ggplot(huron, aes(y = year))
#'
#' h + geom_ribbonh(aes(xmin=0, xmax=level))
#' h + geom_areah(aes(x = level))
#'
#' # Add aesthetic mappings
#' h +
#'   geom_ribbonh(aes(xmin = level - 1, xmax = level + 1), fill = "grey70") +
#'   geom_lineh(aes(x = level))
#'
geom_ribbonh <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRibbonh,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_ribbonh
#' @export
GeomRibbonh <- ggplot2::ggproto(
  "GeomRibbonh",
  ggplot2::Geom,
  default_aes = ggplot2::aes(colour = NA, fill = "grey20", linewidth = 0.5, linetype = 1, alpha = NA),

  required_aes = c("y", "xmin", "xmax"),

  draw_key = ggplot2::draw_key_polygon,

  handle_na = function(data, params) {
    data
  },

  draw_group = function(data, panel_params, coord, na.rm = FALSE) {
    if (na.rm) data <- data[stats::complete.cases(data[c("y", "xmin", "xmax")]), ]
    data <- data[order(data$group, data$y), ]

    # Check that aesthetics are constant
    aes <- unique(data[c("colour", "fill", "linewidth", "linetype", "alpha")])
    if (nrow(aes) > 1) {
      stop("Aesthetics can not vary with a ribbon")
    }
    aes <- as.list(aes)

    # Instead of removing NA values from the data and plotting a single
    # polygon, we want to "stop" plotting the polygon whenever we're
    # missing values and "start" a new polygon as soon as we have new
    # values.  We do this by creating an id vector for polygonGrob that
    # has distinct polygon numbers for sequences of non-NA values and NA
    # for NA values in the original data.  Example: c(NA, 2, 2, 2, NA, NA,
    # 4, 4, 4, NA)
    missing_pos <- !stats::complete.cases(data[c("y", "xmin", "xmax")])
    ids <- cumsum(missing_pos) + 1
    ids[missing_pos] <- NA

    positions <- plyr::summarise(data, y = c(y, rev(y)), x = c(xmax, rev(xmin)), id = c(ids, rev(ids)))
    munched <- ggplot2::coord_munch(coord, positions, panel_params)

    ggname("geom_ribbonh", grid::polygonGrob(
      munched$x, munched$y, id = munched$id,
      default.units = "native",
      gp = grid::gpar(
        fill = ggplot2::alpha(aes$fill, aes$alpha),
        col = aes$colour,
        lwd = aes$linewidth * ggplot2::.pt,
        lty = aes$linetype)
    ))
  }
)

#' @rdname geom_ribbonh
#' @export
geom_areah <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "stackv", na.rm = FALSE, show.legend = NA,
                      inherit.aes = TRUE, ...) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAreah,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_ribbonh
#' @export
GeomAreah <- ggplot2::ggproto(
  "GeomAreah",
  GeomRibbonh,
  default_aes = ggplot2::aes(colour = NA, fill = "grey20", linewidth = 0.5, linetype = 1, alpha = NA),

  required_aes = c("x", "y"),

  setup_data = function(data, params) {
    transform(data, xmin = 0, xmax = x)
  }
)

#' @importFrom ggstance position_fillv
#' @export
ggstance::position_fillv

#' @importFrom ggstance PositionFillv
#' @export
ggstance::PositionFillv

#' @importFrom ggstance position_stackv
#' @export
ggstance::position_stackv

#' @importFrom ggstance PositionStackv
#' @export
ggstance::PositionStackv

#' @importFrom ggstance position_dodge2v
#' @export
ggstance::position_dodge2v

#' @importFrom ggstance PositionDodge2v
#' @export
ggstance::PositionDodge2v

#' @importFrom ggstance position_dodgev
#' @export
ggstance::position_dodgev

#' @importFrom ggstance PositionDodgev
#' @export
ggstance::PositionDodgev

#' @importFrom ggstance geom_colh
#' @export
ggstance::geom_colh

#' @importFrom ggstance GeomColh
#' @export
ggstance::GeomColh

# copied from ggplot2, used by geometries
ggname <- function (prefix, grob) {
  grob$name <- grid::grobName(grob, prefix)
  grob
}
