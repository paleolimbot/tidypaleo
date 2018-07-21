
library(tidyverse)
# library(neotoma)

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

all_data <- counts %>%
  left_join(sample_info, by = "sample.id") %>%
  left_join(taxon_info, by = c("taxon.name", "dataset.id")) %>%
  left_join(dataset_info, by = "dataset.id") %>%
  left_join(site_info, by = "dataset.id")
