#### Harmonization of data from EIA


library(tidyverse)

# clear R environment
rm(list = ls())




### read files

## data

# get the latest files (general info and data)
source("./02_scripts/00_functions/lastfile.R")

lastfile <- my_lastfile("./03_intermediate/02_country_specific/eia/", "general_raw")

general <- read_rds(lastfile)

lastfile <- my_lastfile("./03_intermediate/02_country_specific/eia/", "values_raw")

production <- read_rds(lastfile)


## concordance tables
mat_ids <- read_delim("./01_input/02_lists_and_concordance_tables/material_ids.csv", delim = ";")

source_mat_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_unit_ids.csv", delim = ";")

source_country_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_country_ids.csv", delim = ";")

source_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_ids.csv", delim = ";")





### clean ----

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
eia <- eia %>% 
  left_join(.,
            source_mat_ids %>% 
              filter(source == "EIA") %>% 
              select(-source),
            by = c("commodity" = "source_material_id")
  )

## no_match table
no_match_mat <- eia %>%
  filter(is.na(material_id)) %>%
  distinct(commodity)





## add unit_ids
eia <- eia %>% 
  left_join(.,
            source_unit_ids %>% 
              filter(source == "EIA") %>% 
              select(-source),
            by = c("units" = "source_unit_id")
  )

# no_match table
no_match_unit <- eia %>%
  filter(is.na(unit_id)) %>%
  distinct(units)







# remove NAs
eia <- eia %>%
  filter(!is.na(alphanumiso)) %>%
  filter(!is.na(material_id)) %>%
  filter(!is.na(unit_id))



# aggregate
eia <- eia %>%
  filter(!is.na(value)) %>%
  group_by(alphanumiso, material_id, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()



# save files
write_rds(general, file = "./03_intermediate/")

write_rds(production, file = "./03_intermediate/")

