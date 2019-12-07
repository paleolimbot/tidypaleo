
#' Halifax lakes water chemistry and top/bottom diatom counts
#'
#' A subset of well-labeled, clean diatom count data for 44 Halifax-area (Nova Scotia) lakes,
#' an analysis of which has been published by Ginn et al. (2015). These data are
#' in the form of a [mudata][mudata2::mudata] object, from which location
#' information, parameter information, and data can be extracted using
#' [tbl_locations][mudata2::tbl_locations], [tbl_params][mudata2::tbl_params], and
#' [tbl_data][mudata2::tbl_data] (respectively). Also, a subset of these
#' data amenable to plotting have been made available.
#'
#' @source
#' Neotoma paleoecology database (<http://neotomadb.org/>)
#'
#' @references
#' Ginn, Brian K., Thiyake Rajaratnam, Brian F. Cumming, and John P.
#' Smol. "Establishing Realistic Management Objectives for Urban Lakes Using
#' Paleolimnological Techniques: An Example from Halifax Region (Nova Scotia,
#' Canada)." Lake and Reservoir Management 31, no. 2 (April 3, 2015): 92-108.
#' <https://doi.org/10.1080/10402381.2015.1013648>.
#'
#' @examples
#' library(mudata2)
#' data(halifax_lakes)
#'
#' halifax_lakes %>%
#'   tbl_data()
#'
"halifax_lakes"

#' @rdname halifax_lakes
"halifax_lakes_plottable"

#' Keji lakes core diatom counts
#'
#' A subset of well-labeled, clean diatom count data for 3 Keji-area (Nova
#' Scotia) lakes, which form part of the analysis in Ginn et al. (2007).
#' These data are in the form of a [mudata][mudata2::mudata] object, from which
#' location information, parameter information, and data can be extracted using
#' [tbl_locations][mudata2::tbl_locations], [tbl_params][mudata2::tbl_params], and
#' [tbl_data][mudata2::tbl_data] (respectively). Also, a subset of these
#' data amenable to plotting have been made available.
#'
#' @source Neotoma paleoecology database (<http://neotomadb.org/>)
#'
#' @references
#' Ginn, Brian K., Brian F. Cumming, and John P. Smol. "Long-Term
#' Lake Acidification Trends in High- and Low-Sulphate Deposition Regions from
#' Nova Scotia, Canada." Hydrobiologia 586, no. 1 (July 1, 2007): 261-75.
#' <https://doi.org/10.1007/s10750-007-0644-3>.
#'
#' @examples
#' library(mudata2)
#' data(keji_lakes)
#'
#' keji_lakes %>%
#'   tbl_data()
#'
"keji_lakes"

#' @rdname keji_lakes
"keji_lakes_plottable"

#' Alta Lake Lead-210 and Carbon-14 Ages
#'
#' The `alta_lake_210Pb_ages` object contains raw depths and ages for Alta
#' Lake (Whistler, British Columbia, Canada) core AL-GC2 (Dunnington et al.
#' 2016; Dunnington 2015). For these values, ages were calculated from Lead-210
#' activities using the constant rate of supply (CRS) model (Appleby and
#' Oldfield 1983). The `alta_lake_14C_ages` object contains one
#' uncalibrated Carbon-14 measurement from the same core.
#' The `alta_lake_bacon_ages` object contains the combined result of the
#' Lead-210 and the Carbon-14 ages as modelled by [rbacon-package][rbacon::rbacon-package]
#' (Blaauw and Christen 2011).
#'
#' @references
#' Appleby, P. G., and F. Oldfield. "The Assessment of 210Pb Data from Sites
#' with Varying Sediment Accumulation Rates." Hydrobiologia 103, no. 1 (July 1,
#' 1983): 29–35. <https://doi.org/10.1007/BF00028424>.
#'
#' Blaauw, Maarten, and J. Andrés Christen. "Flexible Paleoclimate Age-Depth
#' Models Using an Autoregressive Gamma Process." Bayesian Analysis 6, no. 3
#' (September 2011): 457–74. <https://doi.org/10.1214/ba/1339616472>.
#'
#' Dunnington, Dewey W., Ian S. Spooner, Chris E. White, R. Jack Cornett, Dave
#' Williamson, and Mike Nelson. "A Geochemical Perspective on the Impact of
#' Development at Alta Lake, British Columbia, Canada." Journal of
#' Paleolimnology 56, no. 4 (November 2016): 315–330.
#' <https://doi.org/10.1007/s10933-016-9919-x>.
#'
#' Dunnington, Dewey W. "A 500-Year Applied Paleolimnological Assessment of
#' Environmental Change at Alta Lake, Whistler, British Columbia, Canada." M.Sc.
#' Thesis, Acadia University, 2015.
#'
#' @seealso
#' The [alta_lake][mudata2::alta_lake] dataset (mudata2 package) for geochemical data.
#'
#' @examples
#' data(alta_lake_210Pb_ages)
#' data(alta_lake_14C_ages)
#' data(alta_lake_bacon_ages)
#'
#' library(mudata2)
#' data(alta_lake)
#'
"alta_lake_210Pb_ages"

#' @rdname alta_lake_210Pb_ages
"alta_lake_14C_ages"

#' @rdname alta_lake_210Pb_ages
"alta_lake_bacon_ages"

#' @rdname alta_lake_210Pb_ages
"alta_lake_geochem"

#' Long Lake Carbon-14 Ages
#'
#' This object contains several uncalibrated Carbon-14 measurements from Long
#' Lake (Nova Scotia-New Brunswick Border Region, Canada) core LL-PC2
#' (Dunnington et al. 2017; White 2012). The `long_lake_bacon_ages` object
#' contains the result of the Carbon-14 ages as
#' modelled by [rbacon-package][rbacon::rbacon-package] (Blaauw and Christen 2011).
#'
#' @references
#' Blaauw, Maarten, and J. Andrés Christen. "Flexible Paleoclimate Age-Depth
#' Models Using an Autoregressive Gamma Process." Bayesian Analysis 6, no. 3
#' (September 2011): 457–74. <https://doi.org/10.1214/ba/1339616472>.
#'
#' Dunnington, Dewey W., Hilary White, Ian S. Spooner, Mark L. Mallory, Chris
#' White, Nelson J. O’Driscoll, and Nic R. McLellan. "A Paleolimnological
#' Archive of Metal Sequestration and Release in the Cumberland Basin Marshes,
#' Atlantic Canada." FACETS 2, no. 1 (May 23, 2017): 440–60.
#' <https://doi.org/10.1139/facets-2017-0004>.
#'
#' White, Hilary E. "Paleolimnological Records of Post-Glacial Lake
#' and Wetland Evolution from the Isthmus of Chignecto Region, Eastern Canada."
#' M.Sc. Thesis, Acadia University, 2012.
#'
#' @seealso
#' The [long_lake][mudata2::long_lake] dataset (mudata2 package) for geochemical data.
#'
#' @examples
#' data(long_lake_14C_ages)
#' data(long_lake_bacon_ages)
#'
#' library(mudata2)
#' data(long_lake)
#'
"long_lake_14C_ages"

#' @rdname long_lake_14C_ages
"long_lake_bacon_ages"
