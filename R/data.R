
#' Alta Lake Lead-210 and Carbon-14 Ages
#'
#' The \code{alta_lake_210Pb_ages} object contains raw depths and ages for Alta
#' Lake (Whistler, British Columbia, Canada) core AL-GC2 (Dunnington et al.
#' 2016; Dunnington 2015). For these values, ages were calculated from Lead-210
#' activities using the constant rate of supply (CRS) model (Appleby and
#' Oldfield 1983). The \code{alta_lake_14C_ages} object contains one
#' uncalibrated Carbon-14 measurement from the same core.
#' The \code{alta_lake_bacon_ages} object contains the combined result of the
#' Lead-210 and the Carbon-14 ages as modelled by \link[rbacon]{rbacon-package}
#' (Blaauw and Christen 2011).
#'
#' @references
#' Appleby, P. G., and F. Oldfield. "The Assessment of 210Pb Data from Sites
#' with Varying Sediment Accumulation Rates." Hydrobiologia 103, no. 1 (July 1,
#' 1983): 29–35. \url{https://doi.org/10.1007/BF00028424}.
#'
#' Blaauw, Maarten, and J. Andrés Christen. "Flexible Paleoclimate Age-Depth
#' Models Using an Autoregressive Gamma Process." Bayesian Analysis 6, no. 3
#' (September 2011): 457–74. \url{https://doi.org/10.1214/ba/1339616472}.
#'
#' Dunnington, Dewey W., Ian S. Spooner, Chris E. White, R. Jack Cornett, Dave
#' Williamson, and Mike Nelson. "A Geochemical Perspective on the Impact of
#' Development at Alta Lake, British Columbia, Canada." Journal of
#' Paleolimnology 56, no. 4 (November 2016): 315–330.
#' \url{https://doi.org/10.1007/s10933-016-9919-x}.
#'
#' Dunnington, Dewey W. "A 500-Year Applied Paleolimnological Assessment of
#' Environmental Change at Alta Lake, Whistler, British Columbia, Canada." M.Sc.
#' Thesis, Acadia University, 2015.
#' \url{http://openarchive.acadiau.ca/cdm/singleitem/collection/Theses/id/1074/rec/1}.
#'
#'
"alta_lake_210Pb_ages"

#' @rdname alta_lake_210Pb_ages
"alta_lake_14C_ages"

#' @rdname alta_lake_210Pb_ages
"alta_lake_bacon_ages"

#' Long Lake Carbon-14 Ages
#'
#' This object contains several uncalibrated Carbon-14 measurements from Long
#' Lake (Nova Scotia-New Brunswick Border Region, Canada) core LL-PC2
#' (Dunnington et al. 2017; White 2012). The \code{long_lake_bacon_ages} object
#' contains the result of the Carbon-14 ages as
#' modelled by \link[rbacon]{rbacon-package} (Blaauw and Christen 2011).
#'
#' @references
#' Blaauw, Maarten, and J. Andrés Christen. "Flexible Paleoclimate Age-Depth
#' Models Using an Autoregressive Gamma Process." Bayesian Analysis 6, no. 3
#' (September 2011): 457–74. \url{https://doi.org/10.1214/ba/1339616472}.
#'
#' Dunnington, Dewey W., Hilary White, Ian S. Spooner, Mark L. Mallory, Chris
#' White, Nelson J. O’Driscoll, and Nic R. McLellan. "A Paleolimnological
#' Archive of Metal Sequestration and Release in the Cumberland Basin Marshes,
#' Atlantic Canada." FACETS 2, no. 1 (May 23, 2017): 440–60.
#' \url{https://doi.org/10.1139/facets-2017-0004}.
#'
#' White, Hilary E. "Paleolimnological Records of Post-Glacial Lake
#' and Wetland Evolution from the Isthmus of Chignecto Region, Eastern Canada."
#' M.Sc. Thesis, Acadia University, 2012.
#' \url{http://openarchive.acadiau.ca/cdm/singleitem/collection/Theses/id/645/rec/80}.
#'
#'
"long_lake_14C_ages"

#' @rdname long_lake_14C_ages
"long_lake_bacon_ages"
