
#' Nested Principal Components Analysis (PCA)
#'
#' Powered by [prcomp][stats::prcomp]. When creating the [nested_data],
#' the data should be scaled (i.e, `trans = scale`) if all variables are not
#' in the same unit.
#'
#' @inheritParams nested_analysis
#' @param data_column An expression that evalulates to the data object within each row of .data
#' @param ... Passed to [prcomp][stats::prcomp].
#'
#' @return .data with additional columns 'model', 'loadings', 'variance' and 'scores'
#' @export
#'
#' @examples
#' library(dplyr, warn.conflicts = FALSE)
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
#' nested_pca %>% unnested_data(variance)
#'
#' # get loadings info
#' nested_pca %>% unnested_data(loadings)
#'
#' # scores, requalified
#' nested_pca %>% unnested_data(c(qualifiers, scores))
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
