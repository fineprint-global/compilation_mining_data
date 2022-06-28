###### Harmonize data from MINEM (Peru)



# Load packages
library(tidyverse)


# clear R environment
rm(list = ls())



### files ----

## data

# latest file
source("./02_scripts/00_functions/lastfile.R")

lastfile <- my_lastfile("./03_intermediate/02_country_specific/peru/", "formatted")

data <- read_rds(lastfile)


## id and concordance tables
mat_ids <- read_delim("./01_input/02_lists_and_concordance_tables/material_ids.csv", delim = ";")

source_mat_ids <- read_delim(
  "./01_input/02_lists_and_concordance_tables/source_material_ids.csv",
  delim = ";",
  locale = locale(encoding = "ISO-8859-1")
  )

source_unit_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_unit_ids.csv", delim = ";")

source_type_facility <- read_delim(
  "./01_input/02_lists_and_concordance_tables/02_country_specific/source_type_facility.csv",
  delim = ";",
  locale = locale(encoding = "ISO-8859-1")
  )



### check and correct ----

## check for NAs in PRODUCTO and remove ----

# check
  # so far it have been only entries which relate to artisanal gold production
    # only with relation to a region, but no naturally no company or mine, wherefore no applicable data
na_prod <- data %>%
  filter(is.na(PRODUCTO))

# remove
data <- data %>%
  filter(!is.na(PRODUCTO))



## remove other "artesanal production"
data <- data %>%
  filter(!grepl("PRODUCTORES ARTESANALES", ETAPA))

data <- data %>%
  filter(!grepl("OTROS", ETAPA))





### add ids

## add mat_ids
data <- data %>% 
  left_join(.,
            source_mat_ids %>% 
              filter(source_cat == "peru_minem") %>% 
              select(-source_cat),
            by = c("PRODUCTO" = "source_material_id")
  )

# no_match table
  # no need to be matched: HORMIGON, SALMUERA (LIQUIDO), YESO, OTROS
no_match_mat <- data %>%
  filter(is.na(material_id)) %>%
  distinct(PRODUCTO)

# remove NAs
data <- data %>%
  filter(!is.na(material_id))



## add unit_ids
data <- data %>% 
  left_join(.,
            source_unit_ids %>% 
              filter(source_cat == "peru_minem") %>% 
              select(-source_cat, -unit_cat),
            by = c("unit" = "source_unit")
  )

# no_match table
no_match_unit <- data %>%
  filter(is.na(unit_id)) %>%
  distinct(unit)



## add type_facility
data <- data %>% 
  left_join(.,
            source_type_facility %>% 
              filter(source == "peru_minem") %>% 
              select(-source),
            by = c("ETAPA" = "source_facility")
            )

# no_match table
no_match_fac <- data %>%
  filter(is.na(type_facility)) %>%
  distinct(ETAPA)



## add source and source_url
data <- data %>%
  mutate(
    source = "MINEM - Ministry of Energy and Mines of Peru",
    source_url = "https://www.minem.gob.pe/_estadisticaSector.php?idSector=1"
  )







### final formatting and correction ----

# rename columns and remove obsolete columns
data <- data %>%
  rename(
    owners = TITULAR,
    mine_fac = UNIDAD,
    region = REGION,
    province = PROVINCIA,
    district = DISTRITO,
    value = `TOTAL GENERAL`
  ) %>%
  select(-PRODUCTO, -ETAPA, -unit)

# only capitalize first letter of each word for several variables
  # due to differences in upper and lower case throughout different years
data <- data %>%
  mutate(
    owners = str_to_title(owners),
    mine_fac = str_to_title(mine_fac),
    region = str_to_title(region),
    province = str_to_title(province),
    district = str_to_title(district),
    )



## check and correct for consistent geographical info
  # e.g. mine_fac not having two different provinces or districts across different years
check_geo_1 <- data %>%
  distinct(mine_fac, region, province, district) %>%
  group_by(mine_fac) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  left_join(.,
            data %>%
              distinct(mine_fac, region, province, district, year, material_id)
            )

# similar, but without district
check_geo_2 <- data %>%
  distinct(mine_fac, region, province) %>%
  group_by(mine_fac, region) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  left_join(.,
            data %>%
              distinct(mine_fac, region, province)
  )

# check for same mine name in different regions
check_geo_3 <- data %>%
  distinct(mine_fac, region) %>%
  group_by(mine_fac) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  left_join(.,
            data %>%
              distinct(mine_fac, region)
  )


## check for consistent ownership
check_owner <- data %>%
  distinct(mine_fac, region, owners) %>%
  group_by(mine_fac, region) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1)





## add temporary ids ----
  # otherwise, after splitting tables, mines with same name in different regions would lead to production values of both being allocated to both
    # i.e. creating false double entries
  # adding only based on mine_fac and region, because all other variables vary across years
data <- data %>%
  mutate(temp_fac_id = paste(mine_fac, region))




### split tables ----
facilities <- data %>%
  distinct(temp_fac_id, mine_fac, region, province, district, type_facility, source, source_url)


sheet_min <- data %>%
  filter(type_facility == "Mine") %>%
  select(temp_fac_id, material_id, year, value, unit_id, source, source_url) %>%
  left_join(.,
            mat_ids %>%
              select(material_category, material_category_2, material_id)
            ) %>%
  filter((material_category %in% c("mineral", "ff_mineral") & !(material_category_2 %in% c("coal & peat")))) %>%
  select(-material_category, -material_category_2)

sheet_coal <- data %>%
  filter(type_facility == "Mine") %>%
  select(temp_fac_id, material_id, year, value, unit_id, source, source_url) %>%
  left_join(.,
            mat_ids %>%
              select(material_category, material_category_2, material_id)
  ) %>%
  filter(material_category_2 %in% c("coal & peat")) %>%
  select(-material_category, -material_category_2)

sheet_com <- data %>%
  filter(type_facility == "Mine") %>%
  select(temp_fac_id, material_id, year, value, unit_id, source, source_url) %>%
  left_join(.,
            mat_ids %>%
              select(material_category, material_id)
  ) %>%
  filter(material_category %in% c("commodity")) %>%
  select(-material_category)

# check for missing allocations
  # apparently only leaving out aggregate material for sulphates which is fine for now
check_mat_cat <- data %>%
  left_join(.,
            mat_ids %>%
              select(material_category, material_id)
  ) %>%
  filter(is.na(material_category))

processing <- data %>%
  filter(type_facility %in% c("Refinery", "Smelter")) %>%
  select(temp_fac_id, material_id, year, value, unit_id, source, source_url)

other_info <- data %>%
  distinct(temp_fac_id, owners, year, source, source_url)



### aggregate tables ----

# aggregate facilities -> multiple provinces and districts into a combined one 
  # sample check has revealed that differences in regions in most cases refer to different mines with different commodities
    # while differences in provinces/districts in most cases refer to the same mine with the same commodities and production
facilities <- facilities %>%
  group_by(temp_fac_id, mine_fac, type_facility, region, source, source_url) %>%
  summarise(
    province = paste(unique(province), collapse = ", "),
    district = paste(unique(district), collapse = ", ")
  ) %>%
  ungroup()


# aggregate production
  # due to mine production having been reported for different owners
sheet_min <- sheet_min %>%
  group_by(temp_fac_id, material_id, year, unit_id, source, source_url) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()

sheet_coal <- sheet_coal %>%
  group_by(temp_fac_id, material_id, year, unit_id, source, source_url) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()

sheet_com <- sheet_com %>%
  group_by(temp_fac_id, material_id, year, unit_id, source, source_url) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()

processing <- processing %>%
  group_by(temp_fac_id, material_id, year, unit_id, source, source_url) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()


# aggregate ownership
  # using maximum year for each as latest reported year (similar to detailed data)
other_info <- other_info %>%
  group_by(temp_fac_id, owners, source, source_url) %>%
  summarise(year = max(year, na.rm = TRUE)) %>%
  ungroup()





### save data

# combine tables as list
peru_minem <- list(
  facilities = facilities,
  sheet_min = sheet_min,
  sheet_coal = sheet_coal,
  sheet_com = sheet_com,
  processing = processing,
  other_info = other_info
)


# save
write_rds(peru_minem, file = "./03_intermediate/02_country_specific/peru/peru_harmonized.rds")
