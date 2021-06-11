# Town indicators #

library(tidyverse) ; library(httr) ; library(readxl)

# Nomis unique ID needed to return over 25,000 rows
unique_id <- ""

# Various indicators for towns
# Source: Centre for Subnational Analysis, ONS 
# URL: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/understandingtownsinenglandandwalesspatialanalysis
tmp <- tempfile(fileext = ".xlsx")
GET(url = url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/understandingtownsinenglandandwalesspatialanalysis/2020/referencetable.xlsx",
    write_disk(tmp))
indicators <- read_xlsx(tmp, sheet = "Towns_data") %>% 
  select(town_code = TOWN_2011CODE, 
         town_name = TOWN_2011NAME, 
         Size = `SIZE FLAG`,
         `Job Density`,
         `Income deprivation percentile`,
         `Population Growth 2009-2019`, 
         `Population growth flag`,
         `Employment Growth 2009-2019`,
         `Employment growth flag`) %>% 
  mutate(`Population growth flag` = case_when(
    `Population growth flag` == "Above 2 x E&W average growth" ~ "more than twice the England & Wales average (8%)", 
    `Population growth flag` == "Above E&W average growth" ~ "above the average for England & Wales (8%)", 
    `Population growth flag` == "Below E&W average growth" ~ "below the England & Wales average (8%)", 
    `Population growth flag` == "Declining Population" ~ "declining at less than 0%"),
    `Employment growth flag` = case_when(
      `Employment growth flag` == "Above 2 x E&W average growth" ~ "more than twice the England & Wales average (12%)",
      `Employment growth flag` == "Above Average Growth" ~ "above the average for England & Wales (12%)",
      `Employment growth flag` == "Below E&W average growth" ~ "below the England & Wales average (12%)",
      `Employment growth flag` == "Declining Employment" ~ "declining at less than 0%"))

# Mid-2019 population estimates ------------------------------------------------
# Source: ONS
# URL: https://www.nomisweb.co.uk/datasets/pestsyoaoa
population <- read_csv(paste0("http://www.nomisweb.co.uk/api/v01/dataset/NM_2010_1.data.csv?uid=", unique_id, "&geography=1119879169...1119882228,1119882230...1119885230,1119885232...1119885236,1119885238...1119885256,1119885263...1119885265,1119885267,1119885257...1119885262,1119885266,1119885268...1119885792&date=latest&gender=0&c_age=200,201,203,209&measures=20100&select=geography_code,c_age_name,obs_value")) %>% 
  rename(town_code = GEOGRAPHY_CODE) %>% 
  pivot_wider(names_from = C_AGE_NAME, values_from = OBS_VALUE) %>% 
  rename(`Total population` = `All Ages`) %>% 
  mutate(`Aged 0 to 15` = round(`Aged 0 to 15`/`Total population`*100,1),
         `Aged 16 to 64` = round(`Aged 16 to 64`/`Total population`*100,1),
         `Aged 65+` = round(`Aged 65+`/`Total population`*100,1))

# join and write datasets
left_join(indicators, population, by = "town_code") %>% 
  mutate(town_name = str_trim(str_remove_all(town_name, "SD|BUA|BUASD"))) %>% 
  filter(town_code %in% st_read("geospatial/towns.geojson")$town_code) %>%
  write_csv("indicators.csv")
