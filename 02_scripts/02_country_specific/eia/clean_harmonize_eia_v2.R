#### Harmonization of API v2 data from EIA

  #### Please note: coalRankDescription == "Preparation Plant" is removed below, because no production data and no coal type.
    #### But coordinates might be of interest



library(tidyverse)

# clear R environment
rm(list = ls())




### read files

## data

# get the latest file
source("./02_scripts/00_functions/lastfile.R")

lastfile <- my_lastfile("./03_intermediate/02_country_specific/eia/", "eia_v2_data_")

data <- read_rds(lastfile)




## id and concordance tables
mat_ids <- read_delim("./01_input/02_lists_and_concordance_tables/material_ids.csv", delim = ";")

source_mat_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_unit_ids.csv", delim = ";")

source_country_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_country_ids.csv", delim = ";")

source_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_ids.csv", delim = ";")

source_type_mining_ids <- read_delim("./01_input/02_lists_and_concordance_tables/02_country_specific/source_type_mining.csv", delim = ";")

states <- read_delim("./01_input/02_lists_and_concordance_tables/02_country_specific/eia/list_of_us_states.csv", delim = ";")




### clean data ----

# clean stateDescription
data <- data %>%
  mutate(stateDescription = gsub("\\(.*)", "", stateDescription))

# remove leading and trailing white spaces from variables if necessary
data <- data %>%
  mutate(stateDescription = str_trim(stateDescription, side = c("both")))





## check distinct types of entries (manually)
# i.e. if there are any odd entries
data %>%
  distinct(mineTypeDescription)

data %>%
  distinct(coalRankDescription)

data %>%
  distinct(`production-units`)

data %>%
  distinct(stateDescription)



## remove coal preparation plants
data <- data %>%
  filter(coalRankDescription != "Preparation Plant")



## select relevant columns
data <- data %>%
  select(period, mineMSHAId, mineName, stateDescription, mineCountyName, coalRankDescription,
         mineTypeDescription, mineStatusDescription, latitude, longitude, `operating-company`, production, `production-units`)



## add mat_ids
data <- data %>% 
  left_join(.,
            source_mat_ids %>% 
              filter(source_cat == "eia") %>% 
              select(-source_cat),
            by = c("coalRankDescription" = "source_material_id")
  )

# no_match table
no_match_mat <- data %>%
  filter(is.na(material_id)) %>%
  distinct(coalRankDescription)



## adjust type_mining
data <- data %>% 
  left_join(.,
            source_type_mining_ids %>% 
              filter(source_cat == "eia") %>% 
              select(-source_cat),
            by = c("mineTypeDescription" = "source_type_mining")
  )

# no_match table
no_match_type_min <- data %>%
  filter(is.na(type_mining)) %>%
  distinct(mineTypeDescription)



## add unit_ids
data <- data %>% 
  left_join(.,
            source_unit_ids %>% 
              filter(source_cat == "eia") %>% 
              select(-source_cat, -unit_cat),
            by = c("production-units" = "source_unit")
  )

# no_match table
no_match_unit <- data %>%
  filter(is.na(unit_id)) %>%
  distinct(`production-units`)



## add country
data <- data %>%
  mutate(country = "United States of America")



## add source_url
data <- data %>%
  mutate(source = "EIA, U.S. Energy Information Administration") %>%
  mutate(source_url = "https://www.eia.gov/opendata/")



## rename
data <- data %>%
  rename(
    year = period,
    mine_fac = mineName,
    state = stateDescription,
    region = mineCountyName,
    owners = `operating-company`,
    value = production
  )




### consolidate tables ----

# production
sheet_coal <- data %>%
  select(mineMSHAId, material_id, type_mining, year, unit_id, value, source, source_url)

# facilities
facilities <- data %>%
  distinct(mineMSHAId, mine_fac, country, state, region, latitude, longitude, source, source_url) %>%
  select(mineMSHAId, mine_fac, country, state, region, latitude, longitude, source, source_url)

# ownership
other_info <- data %>%
  distinct(mineMSHAId, owners, source, source_url) %>%
  select(mineMSHAId, owners, source, source_url)


# check for NAs in production table (by one variable is sufficient)
sheet_coal %>%
  filter(is.na(value))


### save files ----

# combine tables in list
eia <- list(facilities = facilities, sheet_coal = sheet_coal, other_info = other_info)

# save
write_rds(eia, file = "./03_intermediate/02_country_specific/eia/eia_harmonized.rds")


