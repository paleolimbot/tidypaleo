
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
