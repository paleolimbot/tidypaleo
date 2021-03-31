
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidypaleo

<!-- badges: start -->

![R-CMD-check](https://github.com/paleolimbot/tidypaleo/workflows/R-CMD-check/badge.svg)
[![Coverage
Status](https://img.shields.io/codecov/c/github/paleolimbot/tidypaleo/master.svg)](https://codecov.io/github/paleolimbot/tidypaleo?branch=master)
<!-- badges: end -->

Working with multi-proxy paleo-archive data can be difficult. There are
multiple locations, multiple parameters, and a lot of
discipline-specific norms for plot layout and notation. This package
simplifies a few workflows to promote the use of R for reproducible
documents in paleo-based studies.

## Installation

You can install the released versio of tidypaleo from
[CRAN](https://cran.r-project.org/) with:

``` r
install.packages("tidypaleo")
```

You can install the development version from
[GitHub](https://github.com) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/tidypaleo")
```

## Examples

### Strat diagrams

This package exposes a number of functions useful when creating
stratigraphic diagrams, including `facet_abundanceh()`, which combines
several other functions to help create stratigraphic plots using
**ggplot2**. The `geom_col_segsh()` geometry draws horizontal segments,
which are commonly used to show species abundance data.

``` r
library(ggplot2)
library(tidypaleo)
theme_set(theme_paleo())

ggplot(keji_lakes_plottable, aes(x = rel_abund, y = depth)) +
  geom_col_segsh() +
  scale_y_reverse() +
  facet_abundanceh(vars(taxon), grouping = vars(location)) +
  labs(y = "Depth (cm)")
```

<img src="man/figures/README-keji-strat-1.png" width="100%" />
