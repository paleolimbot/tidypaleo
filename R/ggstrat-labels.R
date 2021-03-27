
#' Species facet labellers
#'
#' Use these to label species with partial italic formatting. See [label_parsed][ggplot2::label_parsed].
#'
#' @param labels A data.frame of facet label values
#' @param dont_italicize Regular expressions that should not be italicized
#' @param species_facet Which facet(s) contain species values
#' @param multi_line See [label_parsed][ggplot2::label_parsed]
#'
#' @return A [ggplot2::labeller()]
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(keji_lakes_plottable, aes(x = rel_abund, y = depth)) +
#'   geom_col_segsh() +
#'   scale_y_reverse() +
#'   facet_grid(
#'     cols = vars(taxon),
#'     rows = vars(location),
#'     scales = "free_x",
#'     space = "free_x",
#'     labeller = purrr::partial(label_species, species_facet = "taxon")
#'   ) +
#'   labs(y = "Depth (cm)")
#'
label_species <- function(labels, dont_italicize = c("\\(.*?\\)", "spp?\\.", "-complex", "[Oo]ther"),
                          species_facet = 1, multi_line = TRUE) {
  stopifnot(
    is.character(dont_italicize),
    is.logical(multi_line), length(multi_line) == 1
  )

  if(is.character(species_facet)) {
    all_facets <- colnames(labels)
    # ignore if labels doesn't contain the target facet
    species_facet <- intersect(species_facet, all_facets)
  } else if(is.numeric(species_facet)) {
    all_facets <- seq_along(labels)
    stopifnot(all(species_facet %in% seq_along(labels)))
  } else {
    stop("species_facet must be numeric or character")
  }

  # apply italic() around specific components
  for(facet in species_facet) {
    vals <- labels[[facet]]
    exprs <- partial_italic_expr(unique(as.character(vals)), dont_italicize = dont_italicize)

    if(is.factor(vals)) {
      levs <- levels(vals)
      labels[[facet]] <- factor(exprs[as.character(vals)], levels = exprs[levs])
    } else {
      labels[[facet]] <- exprs[vals]
    }
  }

  # wrap other facets in "" so that label_parsed() doesn't try to parse non-parseable items
  for(facet in setdiff(all_facets, species_facet)) {
    escaped <- stringr::str_replace_all(labels[[facet]], '"', '\\\\"')
    labels[[facet]] <- paste0('"', escaped, '"')
  }

  ggplot2::label_parsed(labels, multi_line = multi_line)
}

# workhorse behind partial italicizing
partial_italic_expr <- function(labs, dont_italicize) {
  not_italics_regex <- paste0("(\\s*", dont_italicize, "\\s*)", collapse = "|")

  locs <- stringr::str_locate_all(labs, not_italics_regex)
  names(locs) <- labs
  inv_locs <- lapply(locs, stringr::invert_match)
  names(inv_locs) <- labs

  locs_df <- dplyr::bind_rows(lapply(locs, as.data.frame), .id = "label")
  locs_df$pattern <- rep_len('"%s"', nrow(locs_df))
  inv_locs_df <- dplyr::bind_rows(lapply(inv_locs, as.data.frame), .id = "label")
  inv_locs_df$pattern <- rep_len('italic("%s")', nrow(inv_locs_df))

  labs_df <- dplyr::bind_rows(locs_df, inv_locs_df)
  labs_df$match <- stringr::str_sub(labs_df$label, labs_df$start, labs_df$end)
  labs_df <- labs_df[order(labs_df$label, labs_df$start), , drop = FALSE]
  labs_df <- labs_df[stringr::str_length(labs_df$match) > 0, , drop = FALSE]

  labs_df$match_esc <- stringr::str_replace_all(labs_df$match, '"', '\\\\"')
  labs_df$label_expr <- sprintf(labs_df$pattern, labs_df$match_esc)

  final_split <- split(labs_df$label_expr, labs_df$label)

  final_chr <- sprintf(
    "paste(%s)",
    vapply(
      final_split,
      paste,
      collapse = ", ",
      FUN.VALUE = character(1)
    )
  )
  names(final_chr) <- names(final_split)
  final_chr
}

#' Geochem facet labelers
#'
#' @param labels A data.frame of facet label values
#' @param units A named list of values = unit
#' @param default_units The default units to apply
#' @param geochem_facet Which facet to apply formatting
#' @param renamers Search and replace operations to perform in the form
#'   search = replace. Replace text can (should) contain backreferences,
#'   and will be parsed as an expression (see [plotmath][grDevices::plotmath]). Use
#'   NULL to suppress renaming.
#' @param multi_line See [label_parsed][ggplot2::label_parsed]
#'
#' @return A [ggplot2::labeller()]
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#'
#' ggplot(alta_lake_geochem, aes(x = value, y = depth)) +
#'   geom_lineh() +
#'   geom_point() +
#'   scale_y_reverse() +
#'   facet_wrap(
#'     vars(param),
#'     labeller = purrr::partial(label_geochem, geochem_facet = "param"),
#'     nrow = 1,
#'     scales = "free_x"
#'   ) +
#'   labs(x = NULL, y = "Depth (cm)")
#'
label_geochem <- function(
  labels,
  units = character(0),
  default_units = NA_character_,
  geochem_facet = 1,
  renamers = c(
    "^d([0-9]+)([HCNOS])$" = "paste(delta ^ \\1, \\2)",
    "^210Pb$" = "paste({}^210, Pb)",
    "^Pb210$" = "paste({}^210, Pb)"
  ),
  multi_line = TRUE
) {

  stopifnot(
    (length(renamers) == 0) || (is.character(renamers) && !is.null(names(renamers))),
    is.character(units),
    is.character(default_units), length(default_units) == 1,
    is.logical(multi_line), length(multi_line) == 1
  )

  if(is.character(geochem_facet)) {
    all_facets <- colnames(labels)
    # ignore if labels doesn't contain the target facet
    geochem_facet <- intersect(geochem_facet, all_facets)
  } else if(is.numeric(geochem_facet)) {
    all_facets <- seq_along(labels)
    stopifnot(all(geochem_facet %in% seq_along(labels)))
  } else {
    stop("geochem_facet must be numeric or character")
  }

  for(facet in geochem_facet) {
    vals <- labels[[facet]]
    new_vals <- search_replace_expr(
      as.character(vals),
      renamers = renamers,
      units = units,
      default_units = default_units
    )

    if(is.factor(vals)) {
      labels[[facet]] <- factor(
        new_vals,
        levels = search_replace_expr(
          levels(vals),
          renamers = renamers,
          units = units,
          default_units = default_units
        )
      )
    } else {
      labels[[facet]] <- new_vals
    }
  }

  for(facet in setdiff(all_facets, geochem_facet)) {
    escaped <- stringr::str_replace_all(labels[[facet]], '"', '\\\\"')
    labels[[facet]] <- paste0('"', escaped, '"')
  }

  ggplot2::label_parsed(labels, multi_line = multi_line)
}

search_replace_expr <- function(vals, renamers, units, default_units) {

  if(!is.null(names(units))) {
    units <- units[vals]
    units[is.na(units) & is.na(names(units))] <- default_units

    unit_add <- dplyr::if_else(
      is.na(units),
      "",
      paste0('~("', stringr::str_replace_all(units, '"', '\\\\"'), '")')
    )
  } else {
    unit_add <- rlang::rep_along(vals, "")
  }

  replaced <- rlang::rep_along(vals, FALSE)
  for(i in seq_along(renamers)) {
    new_vals <- stringr::str_replace(vals, names(renamers)[[i]], renamers[[i]])
    new_replaced <- replaced | stringr::str_detect(vals, names(renamers)[[i]])
    vals[!replaced] <- new_vals[!replaced]
    replaced <- new_replaced
  }

  vals[!replaced] <- paste0('"', stringr::str_replace_all(vals[!replaced], '"', '\\\\"'), '"')

  paste0(vals, unit_add)
}
