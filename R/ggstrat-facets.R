
#' Facet for relative abundance data
#'
#' Provides a number of modifications to the plot that are necessary for relative abundance plots
#' of a number of species. See [scale_x_abundance], [facet_grid][ggplot2::facet_grid],
#' [facet_grid][ggplot2::facet_grid], [label_species], [label_geochem],
#' and [rotated_facet_labels] [rotated_axis_labels]
#' for examples of how to customize the default behaviour.
#'
#' @param taxon,param A call to [vars][ggplot2::vars], defining the column that identifies the taxon (parameter).
#' @param grouping  A call to [vars][ggplot2::vars], identifying additional grouping columns
#' @param rotate_facet_labels,rotate_axis_labels Facet (axis) label rotation (degrees)
#' @param labeller Labeller to process facet names. Use [label_species] to italicize
#'   species names, [label_geochem] to perform common formatting and units,
#'   or [label_value][ggplot2::label_value] to suppress.
#' @param space,scales Modify default scale freedom behaviour
#' @param ... Passed to [facet_grid][ggplot2::facet_grid] (abundance) or [facet_wrap][ggplot2::facet_wrap] (geochem).
#' @inheritParams label_geochem
#' @inheritParams label_species
#'
#' @return A subclass of [ggplot2::facet_grid()] or [ggplot2::facet_wrap()].
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(keji_lakes_plottable, aes(x = rel_abund, y = depth)) +
#'   geom_col_segsh() +
#'   scale_y_reverse() +
#'   facet_abundanceh(vars(taxon), grouping = vars(location)) +
#'   labs(y = "Depth (cm)")
#'
#' ggplot(keji_lakes_plottable, aes(y = rel_abund, x = depth)) +
#'   geom_col_segs() +
#'   scale_x_reverse() +
#'   facet_abundance(vars(taxon), grouping = vars(location)) +
#'   labs(x = "Depth (cm)")
#'
#' ggplot(alta_lake_geochem, aes(x = value, y = depth)) +
#'   geom_lineh() +
#'   geom_point() +
#'   scale_y_reverse() +
#'   facet_geochem_wrap(vars(param), units = c(C = "%", Cu = "ppm", Ti = "ppm"), nrow = 1) +
#'   labs(x = NULL, y = "Depth (cm)")
#'
#' ggplot(alta_lake_geochem, aes(x = value, y = depth)) +
#'   geom_lineh() +
#'   geom_point() +
#'   scale_y_reverse() +
#'   facet_geochem_gridh(vars(param), units = c(C = "%", Cu = "ppm", Ti = "ppm")) +
#'   labs(x = NULL, y = "Depth (cm)")
#'
#' ggplot(alta_lake_geochem, aes(y = value, x = depth)) +
#'   geom_line() +
#'   geom_point() +
#'   scale_x_reverse() +
#'   facet_geochem_grid(vars(param), units = c(C = "%", Cu = "ppm", Ti = "ppm")) +
#'   labs(y = NULL, x = "Depth (cm)")
#'
facet_abundanceh <- function(taxon, grouping = NULL, rotate_facet_labels = 45, labeller = label_species,
                             scales = "free_x", space = "free_x",
                             dont_italicize = c("\\(.*?\\)", "spp?\\.", "-complex", "[Oo]ther"), ...) {

  check_groupings(taxon, grouping)

  labeller <- evaluate_labeller(
    labeller,
    label_species,
    species_facet = rlang::quo_name(taxon[[1]]),
    dont_italicize = dont_italicize
  )

  list(
    scale_x_abundance(),
    ggplot2::facet_grid(rows = grouping, cols = taxon, scales = scales, space = space, labeller = labeller, ...),
    rotated_facet_labels(angle = rotate_facet_labels, direction = "x"),
    ggplot2::labs(x = "Relative abundance (%)")
  )
}

#' @rdname facet_abundanceh
#' @export
facet_abundance <- function(taxon, grouping = NULL, rotate_facet_labels = 0, labeller = label_species,
                            scales = "free_y", space = "free_y",
                            dont_italicize = c("\\(.*?\\)", "spp?\\.", "-complex", "[Oo]ther"), ...) {

  check_groupings(taxon, grouping)

  labeller <- evaluate_labeller(
    labeller,
    label_species,
    species_facet = rlang::quo_name(taxon[[1]]),
    dont_italicize = dont_italicize
  )

  list(
    scale_y_abundance(),
    ggplot2::facet_grid(rows = taxon, cols = grouping, scales = scales, space = space, labeller = labeller, ...),
    rotated_facet_labels(angle = rotate_facet_labels, direction = "y"),
    ggplot2::labs(y = "Relative abundance (%)")
  )
}

#' @rdname facet_abundanceh
#' @export
facet_geochem_wraph <- function(param, grouping = NULL, rotate_axis_labels = 90, scales = "free_x",
                               labeller = label_geochem,
                               renamers = c(
                                 "^d([0-9]+)([HCNOS])$" = "paste(delta ^ \\1, \\2)",
                                 "^210Pb$" = "paste({}^210, Pb)",
                                 "^Pb210$" = "paste({}^210, Pb)"
                               ),
                               units = character(0), default_units = NA_character_, ...) {

  check_groupings(param, grouping)

  labeller <- evaluate_labeller(
    labeller,
    label_geochem,
    geochem_facet = rlang::quo_name(param[[1]]),
    units = units,
    renamers = renamers,
    default_units = default_units
  )

  list(
    ggplot2::facet_wrap(c(param, grouping), scales = scales, labeller = labeller, ...),
    rotated_axis_labels(angle = rotate_axis_labels, direction = "x")
  )
}

#' @rdname facet_abundanceh
#' @export
facet_geochem_wrap <- function(param, grouping = NULL, scales = "free_y",
                                labeller = label_geochem,
                                renamers = c(
                                  "^d([0-9]+)([HCNOS])$" = "paste(delta ^ \\1, \\2)",
                                  "^210Pb$" = "paste({}^210, Pb)",
                                  "^Pb210$" = "paste({}^210, Pb)"
                                ),
                                units = character(0), default_units = NA_character_, ...) {

  check_groupings(param, grouping)

  labeller <- evaluate_labeller(
    labeller,
    label_geochem,
    geochem_facet = rlang::quo_name(param[[1]]),
    units = units,
    renamers = renamers,
    default_units = default_units
  )

  ggplot2::facet_wrap(c(param, grouping), scales = scales, labeller = labeller, ...)
}

#' @rdname facet_abundanceh
#' @export
facet_geochem_grid <- function(param, grouping = NULL, rotate_axis_labels = 0, scales = "free_y",
                               space = "fixed", labeller = label_geochem,
                               renamers = c(
                                 "^d([0-9]+)([HCNOS])$" = "paste(delta ^ \\1, \\2)",
                                 "^210Pb$" = "paste({}^210, Pb)",
                                 "^Pb210$" = "paste({}^210, Pb)"
                               ),
                               units = character(0), default_units = NA_character_, ...) {

  check_groupings(param, grouping)

  labeller <- evaluate_labeller(
    labeller,
    label_geochem,
    geochem_facet = rlang::quo_name(param[[1]]),
    units = units,
    renamers = renamers,
    default_units = default_units
  )

  list(
    ggplot2::facet_grid(rows = param, cols = grouping, scales = scales, space = space, labeller = labeller, ...),
    rotated_axis_labels(angle = rotate_axis_labels, direction = "y")
  )
}

#' @rdname facet_abundanceh
#' @export
facet_geochem_gridh <- function(param, grouping = NULL, rotate_axis_labels = 90, scales = "free_x",
                                space = "fixed", labeller = label_geochem,
                                renamers = c(
                                  "^d([0-9]+)([HCNOS])$" = "paste(delta ^ \\1, \\2)",
                                  "^210Pb$" = "paste({}^210, Pb)",
                                  "^Pb210$" = "paste({}^210, Pb)"
                                ),
                                units = character(0), default_units = NA_character_, ...) {

  check_groupings(param, grouping)

  labeller <- evaluate_labeller(
    labeller,
    label_geochem,
    geochem_facet = rlang::quo_name(param[[1]]),
    units = units,
    renamers = renamers,
    default_units = default_units
  )

  list(
    ggplot2::facet_grid(cols = param, rows = grouping, scales = scales, space = space, labeller = labeller, ...),
    rotated_axis_labels(angle = rotate_axis_labels, direction = "x")
  )
}

check_groupings <- function(param, grouping) {
  stopifnot(
    all(vapply(param, rlang::is_quosure, logical(1))), length(param) == 1,
    is.null(grouping) || all(vapply(grouping, rlang::is_quosure, logical(1)))
  )
}

evaluate_labeller <- function(labeller, default_labeller, ...) {
  if(identical(labeller, default_labeller)) {
    default_labeller_args <- list(...)
    function(...) rlang::exec(default_labeller, !!!default_labeller_args, ...)
  } else {
    labeller
  }
}

#' @importFrom ggplot2 vars
#' @export
ggplot2::vars
