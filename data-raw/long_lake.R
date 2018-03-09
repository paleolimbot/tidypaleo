
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

devtools::use_data(long_lake_14C_ages, overwrite = TRUE)
