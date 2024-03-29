% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/age_depth_model.R
\name{age_depth_interpolate}
\alias{age_depth_interpolate}
\alias{age_depth_extrapolate}
\alias{age_depth_exact}
\alias{age_depth_na}
\title{Age-depth model interpolators/extrapolators}
\usage{
age_depth_interpolate(x, y)

age_depth_extrapolate(x, y, x0 = last, y0 = last, slope = NULL)

age_depth_exact(x, y)

age_depth_na(x, y)
}
\arguments{
\item{x}{A paired vector of x values}

\item{y}{A paired vector of y values}

\item{x0}{The x value to anchor the transform}

\item{y0}{The y value to anchor the transform}

\item{slope}{The slope (in units of y/x) to use for the transform}
}
\value{
A list with component functions \code{trans} and \code{inverse}
}
\description{
Age-depth model interpolators/extrapolators
}
\examples{
age_depth_model(
  alta_lake_210Pb_ages,
  depth = depth_cm, age = age_year_ad,
  age_max = age_year_ad + age_error_yr,
  age_min = age_year_ad - age_error_yr,
  extrapolate_age_below = ~age_depth_extrapolate(
    tail(.x, 3), tail(.y, 3), x0 = dplyr::last, y0 = dplyr::last
  ),
  extrapolate_age_above = ~age_depth_extrapolate(
    head(.x, 3), head(.y, 3), x0 = dplyr::first, y0 = dplyr::first
  )
)

}
