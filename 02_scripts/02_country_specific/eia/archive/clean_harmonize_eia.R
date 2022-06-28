#### Harmonization of data from EIA


library(tidyverse)

# clear R environment
rm(list = ls())




### read files

## data

# get the latest files (general info and data)
source("./02_scripts/00_functions/lastfile.R")

lastfile <- my_lastfile("./03_intermediate/02_country_specific/eia/archive/", "general_raw")

general <- read_rds(lastfile)

lastfile <- my_lastfile("./03_intermediate/02_country_specific/eia/archive/", "values_raw")

production <- read_rds(lastfile)


## id and concordance tables
mat_ids <- read_delim("./01_input/02_lists_and_concordance_tables/material_ids.csv", delim = ";")

source_mat_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_unit_ids.csv", delim = ";")

source_country_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_country_ids.csv", delim = ";")

source_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_ids.csv", delim = ";")

source_type_mining_ids <- read_delim("./01_input/02_lists_and_concordance_tables/02_country_specific/source_type_mining.csv", delim = ";")

states <- read_delim("./01_input/02_lists_and_concordance_tables/02_country_specific/eia/list_of_us_states.csv", delim = ";")




### clean general sheet ----

# clean mine_fac
general <- general %>%
  mutate(mine_fac = gsub("\\(.*)", "", mine_fac)) %>%
  mutate(mine_fac = str_trim(mine_fac, side = c("both")))

# remove leading and trailing white spaces from various variables
general <- general %>%
  mutate(type_production = str_trim(type_production, side = c("both"))) %>%
  mutate(type_coal = str_trim(type_coal, side = c("both"))) %>%
  mutate(type_mining = str_trim(type_mining, side = c("both"))) %>%
  mutate(frequency = str_trim(frequency, side = c("both")))




## check distinct types of entries (manually)
  # i.e. if there are any odd entries
general %>%
  distinct(type_production)

general %>%
  distinct(type_coal)

general %>%
  distinct(type_mining)

general %>%
  distinct(frequency)

general %>%
  distinct(f)

general %>%
  distinct(units)

general %>%
  distinct(copyright)

general %>%
  distinct(source)

general %>%
  distinct(geography)



## remove obsolete columns (based on the above check)
general <- general %>%
  select(-type_production, -frequency, -f, -copyright)








## add mat_ids
general <- general %>% 
  left_join(.,
            source_mat_ids %>% 
              filter(source_cat == "eia") %>% 
              select(-source_cat),
            by = c("type_coal" = "source_material_id")
  )

# no_match table
no_match_mat <- general %>%
  filter(is.na(material_id)) %>%
  distinct(type_coal)



## adjust type_mining
general <- general %>% 
  rename(source_type_mining = type_mining) %>%
  left_join(.,
            source_type_mining_ids %>% 
              filter(source_cat == "eia") %>% 
              select(-source_cat),
            by = c("source_type_mining" = "source_type_mining")
  )

# no_match table
no_match_type_min <- general %>%
  filter(is.na(type_mining)) %>%
  distinct(source_type_mining)



## add unit_ids
general <- general %>% 
  left_join(.,
            source_unit_ids %>% 
              filter(source_cat == "eia") %>% 
              select(-source_cat, -unit_cat),
            by = c("units" = "source_unit")
  )

# no_match table
no_match_unit <- general %>%
  filter(is.na(unit_id)) %>%
  distinct(units)



## split coordinates
general <- general %>%
  separate(
    col = latlon,
    into = c("latitude", "longitude"),
    sep = ","
    )



## clean states 
general <- general %>%
  mutate(geography = str_remove(geography, "USA-")) %>%
  left_join(.,
            states,
            by = c("geography" = "code")
            ) %>%
  select(-geography)



## add country
general <- general %>%
  mutate(country = "United States of America")



## add source_url
general <- general %>%
  mutate(source_url = "https://www.eia.gov/opendata/v1/qb.php")







### format production sheet ----

production <- production %>%
  mutate(year = as.integer(year)) %>%
  mutate(value = as.numeric(value))






### check ----

# number of same mine_fac with same type_mining, but different
# showing necessity of keeping eia series_id
similar_mines <- general %>%
  distinct(mine_fac, type_mining, material_id, series_id) %>%
  group_by(mine_fac, type_mining, material_id) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)






### consolidate tables ----

# production
production <- production %>%
  left_join(.,
            general %>%
              select(series_id, material_id, type_mining, unit_id, source, source_url)
            )

# facilities
facilities <- general %>%
  select(series_id, mine_fac, state, latitude, longitude, source, source_url)

# check for NAs in production table (by one variable is sufficient)
production %>%
  filter(is.na(unit_id))



### save files ----

# combine tables in list
eia <- list(facilities = facilities, production = production)

# save
write_rds(eia, file = "./03_intermediate/02_country_specific/eia/eia_harmonized.rds")


