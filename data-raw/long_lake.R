
library(tidyverse)
library(mudata2)

# from Dunnington et al. 2017 / doi:10.1139/facets-2017-0004
# and White 2012 /
# http://openarchive.acadiau.ca/cdm/singleitem/collection/Theses/id/645/rec/80

long_lake_14C_ages <- long_lake %>%
  select_params(`14C_age`) %>%
  tbl_data() %>%
  arrange(depth) %>%
  mutate(core = "Dunnington et al. 2017 / LL-PC2",
         type = c("twig fragment", "plant", "wood fragment", "wood", "wood fragment")) %>%
  mutate(type = paste0("Carbon-14 age / ", type)) %>%
  select(core, depth_cm = depth, age_14C = value, age_error_14C = sd, type)

long_lake_bacon_ages <- read_delim("data-raw/LL-PC2_43_ages.txt", delim = "\t",
                                   col_types = cols(.default = col_double())) %>%
  rename(depth_cm = depth, age_min_year_BP = min, age_max_year_BP = max,
         age_median_year_BP = median, age_weighted_mean_year_BP = wmean) %>%
  rlang::set_attrs(spec = NULL)

devtools::use_data(long_lake_14C_ages, overwrite = TRUE)
devtools::use_data(long_lake_bacon_ages, overwrite = TRUE)
