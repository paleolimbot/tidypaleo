
library(tidyverse)
library(mudata2)

raw <- read_mudata("data-raw/kellys_lake.mudata")

ages <- raw %>%
  select_params(contains("age_ad")) %>%
  tbl_data() %>%
  select(-dataset, -n, -n_detect, -param) %>%
  rename(age_ad = value)

rename_param <- . %>%
  str_remove("/SINLAB") %>%
  str_remove("/XRF/Vanta") %>%
  str_remove("/RelAbund") %>%
  str_remove("/MyCore") %>%
  str_remove("/CRS") %>%
  str_remove("/210Pb") %>%
  str_replace("C_N", "C/N") %>%
  str_replace("210Pb/Unsupported", "Unsupported 210Pb")

# rename  params
raw$params$param <- rename_param(raw$params$param)
raw$data$param <- rename_param(raw$data$param)

# add ages
adm <- tidypaleo::age_depth_model(ages, depth, age_ad)
raw$data$age_ad <- raw$data$depth %>% tidypaleo::predict_age(object = adm)
raw$data <- raw$data %>%
  select(dataset, location, param, depth, age_ad, everything()) %>%
  arrange(param, depth)
attr(raw, "x_columns")  <- c("depth", "age_ad")

# make sure we didn't invalidate the object
validate_mudata(raw)

# use data
kellys_lake <- raw
usethis::use_data(kellys_lake, overwrite = TRUE)

kellys_lake_geochem <- kellys_lake %>%
  select_params(-Fe, -Mn,-age_ad, -`Unsupported 210Pb`, -Zn, -Ti) %>%
  select_params(-starts_with("Cladocera")) %>%
  tbl_data() %>%
  select(-dataset) %>%
  filter(!is.na(value))

# ggplot(kellys_lake_geochem, aes(x = value, y = depth)) +
#   geom_point() +
#   facet_wrap(vars(param),  scales = "free_x", nrow = 1)

usethis::use_data(kellys_lake_geochem, overwrite = TRUE)

kellys_lake_cladocera <- kellys_lake %>%
  select_params(starts_with("Cladocera")) %>%
  tbl_data() %>%
  select(-c(dataset, error, error_type, n_detect, n)) %>%
  rename(rel_abund = value, taxon = param) %>%
  mutate(taxon = taxon %>% str_remove("Cladocera/")) %>%
  # select only some taxa to include in toy data set
  mutate(
    taxon = fct_lump(taxon, 12, w = rel_abund) %>%
      fct_reorder(rel_abund) %>%
      fct_recode("D. pulex-complex" = "D. pulex complex")
  ) %>%
  filter(taxon != "Other")

ggplot(kellys_lake_cladocera, aes(x = rel_abund, y = depth)) +
  tidypaleo::geom_col_segsh() +
  tidypaleo::facet_abundanceh(vars(taxon))

usethis::use_data(kellys_lake_cladocera, overwrite = TRUE)

kellys_lake_ages <- ages
usethis::use_data(kellys_lake_ages, overwrite = TRUE)
