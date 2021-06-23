# Towns in England & Wales #

library(tidyverse) ; library(httr) ; library(readxl) ; library(sf)

# ------------------------------------------------------------------------------
# Retrieve vector boundaries for towns 
# ------------------------------------------------------------------------------

# Towns
# Source: Centre for Subnational Analysis, ONS 
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/understandingtownsinenglandandwalesspatialanalysis
tmp <- tempfile(fileext = ".xlsx")
GET(url = url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/understandingtownsinenglandandwalesspatialanalysis/2020/referencetable.xlsx",
    write_disk(tmp))
towns <- read_xlsx(tmp, sheet = "Towns_data") %>% 
  select(town_code = TOWN_2011CODE, town_name = TOWN_2011NAME, country_region = `REGION/COUNTRY`) %>% 
  mutate(town_name = str_trim(str_remove_all(town_name, "SD|BUA|BUASD")))

# Built-up Areas
# Source: ONS Open Geography Portal
# https://geoportal.statistics.gov.uk/datasets/built-up-areas-december-2011-boundaries-v2
bua <- st_read("https://opendata.arcgis.com/datasets/f6684981be23404e83321077306fa837_0.geojson") %>% 
  select(town_code = bua11cd)

# Built-up Area Sub Divisions
# Source: ONS Open Geography Portal
# https://geoportal.statistics.gov.uk/datasets/built-up-area-sub-divisions-december-2011-boundaries-2
buasd <- st_read("https://opendata.arcgis.com/datasets/30858d02474b4f5a85916acac0f45168_0.geojson") %>% 
  select(town_code = BUASD11CD)

# bind all built-up areas and sub divisions
built_up_areas <- left_join(bind_rows(bua, buasd), towns, by = "town_code") %>% 
  filter(!is.na(town_name))

# ------------------------------------------------------------------------------
# Intersect towns and cities with local authority district boundaries
# ------------------------------------------------------------------------------

# Local authority districts (England and Wales)
# Source: ONS Open Geography Portal
# URL: https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2020-uk-bgc
local_authority_districts <- st_read("https://opendata.arcgis.com/datasets/db23041df155451b9a703494854c18c4_0.geojson") %>% 
  select(area_code = LAD20CD, area_name = LAD20NM) %>% 
  filter(str_detect(area_code, "^E|^W"))

# extract centroids of towns and city boundaries
centroids <- built_up_areas %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_drop_geometry() %>% 
  st_as_sf(crs = 4326, coords = c("lon", "lat")) %>% 
  st_set_precision(100) %>% 
  st_intersection(st_make_valid(local_authority_districts)) %>% 
  st_drop_geometry() %>% 
  select(town_code, area_code, area_name)

# ------------------------------------------------------------------------------
# Join local authority information to towns and cities and extract centroids
# ------------------------------------------------------------------------------

left_join(built_up_areas, centroids, by = "town_code") %>% 
  select(town_code, town_name, area_code, area_name, country_region) %>% 
  mutate(area_code = case_when(town_name == "Berwick-upon-Tweed" ~ "E06000057",
                               town_name == "Holyhead" ~ "W06000001",
                               TRUE ~ area_code),
         area_name = case_when(town_name == "Berwick-upon-Tweed" ~ "Northumberland",
                               town_name == "Holyhead" ~ "Isle of Anglesey",
                               TRUE ~ area_name),
         lon = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  st_write("towns.geojson")

