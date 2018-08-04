
#' Nested Principal Components Analysis (PCA)
#'
#' Powered by \link[stats]{prcomp}. When creating the \link{nested_data_matrix},
#' the data should be scaled (i.e, \code{trans = scale}) if all variables are not
#' in the same unit.
#'
#' @inheritParams nested_anal
#' @param ... Passed to \link[stats]{prcomp}.
#'
#' @return .data with additional columns 'model', 'loadings', 'variance' and 'scores'
#' @export
#'
#' @examples
#' library(tidyr)
#'
#' nested_pca <- alta_lake_geochem %>%
#'   nested_data_matrix(
#'     key = param,
#'     value = value,
#'     qualifiers = c(depth, zone),
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
nested_prcomp <- function(.data, data_column = "data", ...) {
  npca <- nested_anal(
    .data,
    data_column = !!enquo(data_column),
    fun = stats::prcomp,
    data_arg = "x",
    scale. = FALSE,
    reserved_names = c("variance", "loadings", "scores"),
    ...
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

  npca
}
