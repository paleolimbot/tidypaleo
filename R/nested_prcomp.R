
#' Nested Principal Components Analysis (PCA)
#'
#' Powered by \link[stats]{prcomp}. When creating the \link{nested_data},
#' the data should be scaled (i.e, \code{trans = scale}) if all variables are not
#' in the same unit.
#'
#' @inheritParams nested_analysis
#' @param data_column An expression that evalulates to the data object within each row of .data
#' @param ... Passed to \link[stats]{prcomp}.
#'
#' @return .data with additional columns 'model', 'loadings', 'variance' and 'scores'
#' @export
#'
#' @examples
#' library(tidyr)
#'
#' nested_pca <- alta_lake_geochem %>%
#'   nested_data(
#'     qualifiers = c(depth, zone),
#'     key = param,
#'     value = value,
#'     trans = scale
#'   ) %>%
#'   nested_prcomp()
#'
#' # get variance info
#' nested_pca %>% unnest(variance)
#'
#' # get loadings info
#' nested_pca %>% unnest(loadings)
#'
#' # scores, requalified
#' nested_pca %>% unnest(qualifiers, scores)
#'
nested_prcomp <- function(.data, data_column = .data$data, ...) {
  data_column <- enquo(data_column)

  npca <- nested_analysis(
    .data, stats::prcomp, !!data_column, ...,
    .reserved_names = c(
      "variance", "loadings", "scores",
      paste0("PC", 1:100),
      "component", "component_text", "standard_deviation", "variance",
      "variance_proportion", "variance_proportion_cumulative",
      "variable"
    )
  )

  npca$variance <- purrr::map(
    npca$model,
    function(model) {
      tibble::tibble(
        component = seq_along(model$sdev),
        component_text = paste0("PC", seq_along(model$sdev)),
        standard_deviation = model$sdev,
        variance = model$sdev ^ 2,
        variance_proportion = (model$sdev ^ 2) / sum(model$sdev ^ 2),
        variance_proportion_cumulative = cumsum((model$sdev ^ 2) / sum(model$sdev ^ 2))
      )
    }
  )

  npca$loadings <- purrr::map(
    npca$model,
    function(model) {
      df <- tibble::rownames_to_column(
        as.data.frame(model$rotation),
        var = "variable"
      )
      tibble::as_tibble(df)
    }
  )

  npca$scores <- purrr::map(
    npca$model,
    function(model) {
      tibble::as_tibble(stats::predict(model))
    }
  )

  new_nested_analysis(npca, "nested_prcomp")
}

#' @importFrom stats biplot
#' @export
#' @rdname plot.nested_analysis
biplot.nested_prcomp <- function(x, ..., nrow = NULL, ncol = NULL) {
  plot_nested_analysis(x, .fun = stats::biplot, ..., nrow = nrow, ncol = ncol)
}


#' A ggplot2-based PCA biplot
#'
#' @param object A \link{nested_prcomp} object
#' @param mapping A mapping, created with \link[ggplot2]{aes}.
#' @param which Which components to plot
#' @param ... Passed to \link[ggplot2]{facet_wrap} if there are grouping variables, otherwise ignored
#' @param label An expression evaluated within unnest(object, qualifiers, scores) that defines the label for the object.
#' @param score_geom,score_label_geom,var_geom,var_label_geom,var_buffer_geom Override these to customize the appearence of
#'   each layer, or use NULL to suppress.
#' @param default_theme Use NULL to ignore the default theme for this plot, \link[ggplot2]{theme_bw}.
#' @param env Plot environment
#'
#' @return A \link[ggplot2]{ggplot} object.
#' @importFrom ggplot2 autoplot
#' @export
#'
autoplot.nested_prcomp <- function(
  object, mapping = NULL,  which = c("PC1", "PC2"), ...,
  label = .data$row_number,
  score_geom = ggplot2::geom_point(),
  score_label_geom = ggrepel::geom_text_repel(ggplot2::aes(label = .data$auto_label), alpha = 0.5),
  var_geom = list(
    ggplot2::geom_segment(ggplot2::aes(xend = 0 , yend = 0), color = "red"),
    ggplot2::geom_point(color = "red")
  ),
  var_label_geom = ggplot2::geom_label(
    ggplot2::aes(label = .data$variable), color = "red", hjust = "outward", vjust = "outward", fill = NA, label.size = NA
  ),
  var_buffer_geom = ggplot2::geom_blank(),
  default_theme = ggplot2::theme_bw(),
  env = parent.frame()
) {

  label <- enquo(label)

  stopifnot(
    is.character(which), length(which) == 2
  )

  # extract essential components
  group_vars <- get_grouping_vars(object)
  scores <- tidyr::unnest(object, .data$qualifiers, .data$scores)
  loadings <- tidyr::unnest(object, .data$loadings)
  variance <- tidyr::unnest(object, .data$variance)
  variance_percent <- rlang::set_names(variance$variance_proportion * 100, variance$component_text)

  # add user-specified label
  scores <- dplyr::mutate(scores, auto_label = !!label)

  stopifnot(
    all(which %in% colnames(scores)),
    all(which %in% colnames(loadings))
  )

  # let user override mappings
  mapping <- c(
    mapping,
    ggplot2::aes(x = !!rlang::sym(which[1]), y = !!rlang::sym(which[2]))
  )[unique(c("x", "y", names(mapping)))]

  class(mapping) <- "uneval"

  # find scale for loadings
  # auto hjust and vjust for labels
  x <- loadings[[which[1]]]
  y <- loadings[[which[2]]]
  xscore <- scores[[which[1]]]
  yscore <- scores[[which[2]]]

  loading_scale <- min(
    diff(range(xscore)) / diff(range(x)),
    diff(range(yscore)) / diff(range(y))
  )

  loadings <- dplyr::mutate_at(loadings, dplyr::vars(dplyr::starts_with("PC")), `*`, loading_scale)
  loadings_buffer <- dplyr::mutate_at(loadings, dplyr::vars(dplyr::starts_with("PC")), `*`, 1.2)
  var_buffer_geom$data <- loadings_buffer

  loadings$auto_vjust <- -y / sqrt(x^2 + y^2) / 2 + 0.5
  loadings$auto_hjust <- -x / sqrt(x^2 + y^2) / 2 + 0.5

  var_geom <- override_data(var_geom, loadings)
  var_label_geom <- override_data(var_label_geom, loadings)

  other_axes <- if(!is.null(var_geom) || !is.null(var_label_geom)) {
    list(
      ggplot2::scale_x_continuous(sec.axis = ggplot2::sec_axis(~ . / loading_scale, name = sprintf("%s Loading", which[1]))),
      ggplot2::scale_y_continuous(sec.axis = ggplot2::sec_axis(~ . / loading_scale, name = sprintf("%s Loading", which[2]))),
      ggplot2::theme(
        axis.text.x.top = ggplot2::element_text(color = "red"),
        axis.text.y.right = ggplot2::element_text(color = "red"),
        axis.ticks.x.top = ggplot2::element_line(color = "red"),
        axis.ticks.y.right = ggplot2::element_line(color = "red"),
        axis.title.x.top = ggplot2::element_text(color = "red"),
        axis.title.y.right = ggplot2::element_text(color = "red")
      )
    )
  }

  # return plot object
  ggplot2::ggplot(scores, mapping, environment = env) +  score_geom + score_label_geom +
    var_geom + var_label_geom + var_buffer_geom +
    ggplot2::labs(
      x = sprintf("%s (%0.1f%%)", which[1], variance_percent[which[1]]),
      y = sprintf("%s (%0.1f%%)", which[2], variance_percent[which[2]])
    ) +
    default_theme +
    other_axes +
    if(length(group_vars) > 0) ggplot2::facet_wrap(do.call(ggplot2::vars, rlang::syms(group_vars)), ...)
}

# places data and a default mapping behind a previously specified layer
override_data <- function(layer, data = NULL, mapping = NULL) {
  if(inherits(layer, "Layer")) {
    if(!is.null(data)) {
      layer$data <- data
    }
    if(!is.null(mapping)) {
      layer$mapping <- override_mapping(layer$mapping, mapping)
    }
  } else if(is.list(layer)) {
    layer <- lapply(layer, override_data, data = data, mapping = mapping)
  }

  layer
}

override_mapping <- function(mapping, default_mapping = ggplot2::aes()) {
  mapping <- c(mapping, default_mapping)
  mapping <- mapping[unique(names(mapping))]
  class(mapping) <- "uneval"
  mapping
}


