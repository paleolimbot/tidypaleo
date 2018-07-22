
library(tidyverse)
library(mudata2)

# from Dunnington et al. 2016, doi: 10.1007/s10933-016-9919-x
# and thesis version, Dunnington 2015,
# http://openarchive.acadiau.ca/cdm/singleitem/collection/Theses/id/1074/rec/1

alta_lake_210Pb_ages <- tibble(
  core = "Dunnington et al. 2016 / AL-GC2",
  depth_cm = c(0, 1, 1.5, 2:7),
  age_year_ad = c(2014.6, 2008.0, 2003.4, 1998.1, 1981.8, 1965.6, 1947.2, 1922.3, 1896.0),
  age_error_yr = c(0.00, 0.34, 0.56, 0.86, 2.25, 4.73, 8.21, 27.02, 57.19),
  type = "Sediment Lead-210 CRS"
)

alta_lake_14C_ages <- tibble(
  core = "Dunnington et al. 2015 / AL-GC2",
  depth_cm = 29.5,
  age_14C = 340,
  age_error_14C = 30,
  type = "Carbon-14 age / fir needle"
)

alta_lake_bacon_ages <- read_delim("data-raw/AL-GC2_30_ages.txt", delim = "\t",
                                   col_types = cols(.default = col_double())) %>%
  rename(depth_cm = depth, age_min_year_BP = min, age_max_year_BP = max,
         age_median_year_BP = median, age_weighted_mean_year_BP = wmean) %>%
  rlang::set_attrs(spec = NULL)

alta_lake_geochem <- alta_lake %>%
  select_params("C", "C/N", "d13C", "d15N", "Ti", "Cu") %>%
  tbl_data() %>%
  select(-dataset)

devtools::use_data(alta_lake_geochem, overwrite = TRUE)
devtools::use_data(alta_lake_210Pb_ages, overwrite = TRUE)
devtools::use_data(alta_lake_14C_ages, overwrite = TRUE)
devtools::use_data(alta_lake_bacon_ages, overwrite = TRUE)
