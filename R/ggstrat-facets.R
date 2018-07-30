
#' Facet for relative abundance data
#'
#' Provides a number of modifications to the plot that are necessary for relative abundance plots
#' of a number of species. See \link{scale_x_abundance}, \link[ggplot2]{facet_grid},
#' \link[ggplot2]{facet_grid}, \link{label_species}, \link{label_geochem},
#' and \link{rotated_facet_labels} \link{rotated_axis_labels}
#' for examples of how to customize the default behaviour.
#'
#' @param taxon,param A call to \link[ggplot2]{vars}, defining the column that identifies the taxon (parameter).
#' @param grouping  A call to \link[ggplot2]{vars}, identifying additional grouping columns
#' @param rotate_facet_labels,rotate_axis_labels Facet (axis) label rotation (degrees)
#' @param labeller Labeller to process facet names. Use \link{label_species} to italicize
#'   species names, or \link[ggplot2]{label_value} to suppress.
#' @param space,scales Modify default scale freedom behaviour
#' @param units A named vector of units to apply to parameter labels (NA for unitless)
#' @param default_units The default unit name (NA for unitless)
#' @param ... Passed to \link[ggplot2]{facet_grid} (abundance) or \link[ggplot2]{facet_wrap} (geochem).
#'
#' @export
#'
facet_abundanceh <- function(taxon, grouping = NULL, rotate_facet_labels = 45, labeller = label_species,
                             scales = "free_x", space = "free_x", ...) {

  # must be created by vars()
  stopifnot(
    all(vapply(taxon, rlang::is_quosure, logical(1)))
  )

  # set the labeller to partially italicise species names
  if(identical(labeller, label_species)) {
    taxon_facet_name <- vapply(taxon, rlang::quo_name, character(1))
    labeller <- function(...) label_species(..., species_facet = taxon_facet_name)
  }

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
                            scales = "free_y", space = "free_y", ...) {

  # must be created by vars()
  stopifnot(
    all(vapply(taxon, rlang::is_quosure, logical(1)))
  )

  # set the labeller to partially italicise species names
  if(identical(labeller, label_species)) {
    taxon_facet_name <- vapply(taxon, rlang::quo_name, character(1))
    labeller <- function(...) label_species(..., species_facet = taxon_facet_name)
  }

  list(
    scale_y_abundance(),
    ggplot2::facet_grid(rows = taxon, cols = grouping, scales = scales, space = space, labeller = labeller, ...),
    rotated_facet_labels(angle = rotate_facet_labels, direction = "y"),
    ggplot2::labs(x = "Relative abundance (%)")
  )
}

#' @rdname facet_abundanceh
#' @export
facet_geochem <- function(param, grouping = NULL, rotate_axis_labels = 90, scales = "free_x",
                          labeller = label_geochem,
                          units = character(0), default_units = NA_character_, ...) {

  # must be created by vars()
  stopifnot(
    all(vapply(param, rlang::is_quosure, logical(1)))
  )

  # set the labeller to partially italicise species names
  if(identical(labeller, label_geochem)) {
    geochem_facet_name <- vapply(param, rlang::quo_name, character(1))
    labeller <- function(...) label_geochem(..., geochem_facet = geochem_facet_name, units = units,
                                            default_units = default_units)
  }

  list(
    ggplot2::facet_wrap(c(param, grouping), scales = scales, labeller = labeller, ...),
    rotated_axis_labels(angle = rotate_axis_labels, direction = "x")
  )
}

#' @importFrom ggplot2 vars
#' @export
ggplot2::vars
