
library(tidyverse)
# library(neotoma)
library(mudata2)

ginn_ns_search <- read_csv(
  "data-raw/neotoma_ginn_nova_scotia.csv",
  col_types = cols(
    SiteID = col_integer(),
    SiteName = col_character(),
    Latitude = col_double(),
    Longitude = col_double(),
    DatasetID = col_integer(),
    DatasetType = col_character(),
    AgeOldest = col_integer(),
    AgeYoungest = col_integer()
  )
)

# skip download from neotoma
# datasets_download_flat <- neotoma::get_download(ginn_ns_search$DatasetID) %>%
#   write_rds("data-raw/neotoma_ginn_nova_scotia.rds")
datasets_download_flat <- read_rds("data-raw/neotoma_ginn_nova_scotia.rds")

site_info <- map(datasets_download_flat, neotoma::get_site) %>%
  bind_rows(.id = "dataset.id") %>%
  mutate(dataset.id = suppressWarnings(as.numeric(dataset.id))) %>%
  filter(!is.na(dataset.id))

# leaflet::leaflet(site_info) %>%
#   leaflet::addTiles() %>%
#   leaflet::addMarkers(lng = ~long, lat = ~lat)

dataset_info <- map(datasets_download_flat, c("dataset", "dataset.meta")) %>%
  bind_rows()

taxon_info <- map(datasets_download_flat, "taxon.list") %>%
  map(mutate_if, is.factor, as.character) %>%
  bind_rows(.id = "dataset.id") %>%
  mutate(dataset.id = as.numeric(dataset.id))

sample_info <- map(datasets_download_flat, "sample.meta") %>%
  map(mutate_if, is.factor, as.character) %>%
  bind_rows()

counts <- tibble(
  sample.id = map(datasets_download_flat, c("sample.meta", "sample.id")),
  counts = map(datasets_download_flat, "counts") %>% map(as_tibble),
  long_data = map2(counts, sample.id, ~mutate(.x, sample.id = .y)) %>%
    map(~gather(.x, key = "taxon.name", value = "value", -sample.id))
) %>%
  unnest(long_data)


# cores: PSKWSK07B (17953), BEAVERS07B (17958), PESKAWA07B (17957)
keji_lakes_datasets <- dataset_info %>%
  filter(dataset.id %in% c(17953, 17958, 17957))

keji_lakes_locations <- site_info %>%
  filter(dataset.id %in% keji_lakes_datasets$dataset.id) %>%
  select(location = site.name, neotoma_dataset_id = dataset.id, everything()) %>%
  rename_all(str_replace_all, "\\.", "_")

keji_lakes_params <- taxon_info %>%
  filter(dataset.id %in% keji_lakes_datasets$dataset.id) %>%
  select(-dataset.id) %>%
  distinct() %>%
  rename(param = taxon.name) %>%
  rename_all(str_replace_all, "\\.", "_")

keji_lakes_data <- counts %>%
  # two samples have (probably erroneous) duplicate depth information for causes mudata() validation fail
  filter(sample.id != 166477, sample.id != 166546) %>%
  left_join(sample_info %>% select(depth, unit.name, sample.id, dataset.id), by = "sample.id") %>%
  left_join(taxon_info %>% select(taxon.name, dataset.id, variable.units), by = c("taxon.name", "dataset.id")) %>%
  left_join(site_info %>% select(dataset.id, site.name), by = "dataset.id") %>%
  filter(dataset.id %in% keji_lakes_datasets$dataset.id) %>%
  rename(neotoma_dataset_id = dataset.id, location = site.name, param = taxon.name, neotoma_sample_id = sample.id) %>%
  rename_all(str_replace_all, "\\.", "_") %>%
  select(location, param, depth, value, everything())

keji_lakes <- mudata(
  data = keji_lakes_data,
  params = keji_lakes_params,
  locations = keji_lakes_locations,
  dataset_id = "neotoma_keji_lakes",
  x_columns = "depth"
) %>%
  update_datasets(source = "Neotoma", url = "https://www.neotomadb.org/")

devtools::use_data(keji_lakes, overwrite = TRUE)

# diatom samples from Halifax Lakes:
# (Banook has some unfortunate labeling inconsistencies, and thus can't be included)
halifax_lakes_datasets <- dataset_info %>%
  filter(str_detect(dataset.name, "Halifax Lakess?"), !str_detect(collection.handle, "BANOOK")) %>%
  spread(dataset.type, dataset.id) %>%
  filter(!is.na(diatom), !is.na(`water chemistry`)) %>%
  gather(dataset.type, dataset.id, diatom, `water chemistry`) %>%
  arrange(collection.handle)

halifax_lakes_locations <- site_info %>%
  filter(dataset.id %in% halifax_lakes_datasets$dataset.id) %>%
  group_by_at(vars(-dataset.id)) %>%
  summarise(neotoma_dataset_ids = paste(dataset.id, collapse = ", ")) %>%
  ungroup() %>%
  select(location = site.name, everything()) %>%
  rename_all(str_replace_all, "\\.", "_")

halifax_lakes_params <- taxon_info %>%
  filter(dataset.id %in% halifax_lakes_datasets$dataset.id) %>%
  select(-dataset.id) %>%
  distinct() %>%
  group_by(taxon.name) %>%
  mutate(variable.units = paste(variable.units, collapse = "; ")) %>%
  ungroup() %>%
  # there is an issue with units: some params are both in ug/L and mg/L
  distinct() %>%
  rename(param = taxon.name) %>%
  rename_all(str_replace_all, "\\.", "_")

halifax_lakes_data <- counts %>%
  left_join(sample_info %>% select(depth, unit.name, sample.id, dataset.id), by = "sample.id") %>%
  left_join(taxon_info %>% select(taxon.name, dataset.id, variable.units), by = c("taxon.name", "dataset.id")) %>%
  left_join(dataset_info %>% select(dataset.id, dataset.type), by = "dataset.id") %>%
  left_join(site_info %>% select(dataset.id, site.name), by = "dataset.id") %>%
  filter(dataset.id %in% halifax_lakes_datasets$dataset.id) %>%
  # convert ug/L to mg/L
  mutate(
    value = if_else(!is.na(variable.units) & variable.units == "µg/L", value / 1000, value),
    variable.units = if_else(!is.na(variable.units) & variable.units == "µg/L", "mg/L", variable.units)
  ) %>%

  # set sample_type to 'top', 'bottom', or 'water chemistry'
  mutate(
    sample_type = case_when(
      dataset.type == "water chemistry" ~ "water chemistry",
      str_detect(unit.name, "top$") ~ "top",
      str_detect(unit.name, "tom$") ~ "bottom",
      # several mislabeled units
      unit.name == "Bisset_wch" & dataset.type == "diatom" ~ "top",
      unit.name == "Major_wch" & dataset.type == "diatom" ~ "top",
      TRUE ~ NA_character_
    )
  ) %>%
  rename(neotoma_dataset_id = dataset.id, location = site.name, param = taxon.name, neotoma_sample_id = sample.id) %>%
  rename_all(str_replace_all, "\\.", "_") %>%
  select(location, param, sample_type, value, everything())

halifax_lakes <- mudata(
  data = halifax_lakes_data,
  params = halifax_lakes_params,
  locations = halifax_lakes_locations,
  dataset_id = "neotoma_halifax_lakes",
  x_columns = "sample_type"
) %>%
  update_datasets(source = "Neotoma", url = "https://www.neotomadb.org/")

devtools::use_data(halifax_lakes, overwrite = TRUE)
