#### compile estimation factors from snl data



library(tidyverse)


# clear R environment
rm(list = ls())



# concordance tables
source_cou_ids <- read_delim("./snl/01_input/02_concordance_tables/source_country_ids.csv", delim = ";")

source_mat_ids <- read_delim("./snl/01_input/02_concordance_tables/source_material_ids.csv", delim = ";")

source_unit_ids <- read_delim("./snl/01_input/02_concordance_tables/source_unit_ids.csv", delim = ";")

# factors
conv_fac <- read_delim("./snl/01_input/03_factors/conversion_factors.csv", ";") %>%
  distinct(unit_cat, unit_id, conv_factor_t)

# metal prices
met_price <- read_delim("./detailed_data/01_input/05_other_data/average_prices_1990-2020.csv", delim = ";")







### read data

## ores
load("./snl/01_input/01_data/production/Ore.Rdata")

ores <- Ore
rm(Ore)

ores <- ores %>% filter(value > 0)



## commodities

# define files
file_list <- list.files(
  "./snl/01_input/01_data/production/",
  pattern = "production.Rdata"
  )

# remove obsolete files
file_list <- file_list[!file_list %in% c(
  "Heavy Mineral Sands_production.Rdata", 
  "Ferromolybdenum_production.Rdata",
  "Coal_production.Rdata",
  "Ferrochrome_production.Rdata",
  "Graphite_production.Rdata",
  "Bauxite_production.Rdata",
  "Chromite_production.Rdata",
  "Ilmenite_production.Rdata",
  "Iron Ore_production.Rdata",
  "Potash_production.Rdata",
  "Rutile_production.Rdata",
  "Zircon_production.Rdata"
  )]

# commodity names
file_list <- gsub("_production.Rdata", "", file_list)


snl_data <- list()

for (com in file_list) {
  load(
    paste0("./snl/01_input/01_data/production/", 
           com, 
           "_production.Rdata"
           )
    )
  
  snl_data[[com]] <- x %>% 
    as_tibble() %>%
    dplyr::filter(value > 0) # only mines where grade is available
  
}

rm(x)


# merge data
commodities <- do.call("rbind", snl_data)
  



### harmonize


# add mat_ids
commodities <- commodities %>% 
  left_join(.,
            source_mat_ids,
            by = c("commodity" = "source_material_id")
  )

# no_match table
no_match_mat <- commodities %>%
  filter(is.na(material_id)) %>%
  distinct(commodity)




# units: ores
ores <- ores %>% 
  filter(unit != "m") %>%
  mutate(unit_id = "t") %>%
  select(-unit)

## units: commodities
commodities <- commodities %>%
  mutate(
    unit = ifelse(
      unit == "tonne",
      "t",
      unit
    )
  ) %>%
  rename(unit_id = unit)

# add factor and convert 
commodities <- commodities %>% 
  left_join(.,
            conv_fac %>% 
              select("unit_id","conv_factor_t"), 
            by = c("unit_id")
  ) %>% 
  mutate(
    value = ifelse(
      is.na(conv_factor_t),
      value, 
      value * conv_factor_t
    ),
    unit_id = ifelse(
      is.na(conv_factor_t), 
      unit_id, 
      "t" )
  ) %>% 
  select(-conv_factor_t)




# aggregate
commodities <- commodities %>%
  group_by(snl_id, material_id, year, unit_id) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()




# combine ores and commodities
data <- commodities %>%
  left_join(.,
            ores %>%
              select(-type),
            by = c("snl_id", "year", "unit_id"),
            suffix = c(".com", ".ore")
            ) %>%
  filter(!is.na(value.ore))


# include prices
data <- data %>%
  left_join(.,
            met_price %>% 
              select(material_id, average_price)
  )






### calculation

# calculate value of metal
data <- data %>%
  mutate(met_value = value.com * 1000 * average_price) # taking into account proper units: i.e. t vs USD/kg

# calculating total value per ore, join that into table
data <- data %>%
  left_join(
    data %>%
      group_by(snl_id, year) %>%
      summarise(total_met_value = sum(met_value, na.rm = TRUE))
  )

# calculate ratio (t/t) of ore per metal (i.e. share of metal value per ore value times amount of ore, and then "ore share" per amount of metal)
data <- data %>%
  mutate(ore_share = (met_value / total_met_value * value.ore)) %>%
  mutate(ratio_ore_per_met = ore_share / value.com)



# remove everything without a ratio (e.g. commodities like alloys or just years lacking data)
  # + everything where value.com and value.min are NA
est_fac <- data %>%
  filter(
    !is.na(ratio_ore_per_met),
    !is.na(value.com),
    !is.na(value.ore)
  ) %>%
  select(snl_id, material_id, year, value.com, ratio_ore_per_met)





# include countries
est_fac <- est_fac %>% 
  left_join(.,
            snl_mines %>% 
              select(snl_id, country)
  )


## add country_ids
est_fac <- est_fac %>% 
  left_join(.,
            source_cou_ids %>% 
              distinct(source_country_id, alphanumiso),
            by = c("country" = "source_country_id")
  )

# no_match table
no_match_cou <- est_fac %>%
  filter(is.na(alphanumiso)) %>%
  distinct(country)





# Combine both tables and consolidate by country, i.e. group by country and summarize ore ratio with weighted average (weighted by amount of metal)
est_fac <- est_fac %>%
  group_by(alphanumiso, material_id, year) %>%
  summarise(ratio_ore_per_met = weighted.mean(ratio_ore_per_met, value.com, na.rm = TRUE)) %>%
  ungroup()




# remove years > 2019
est_fac <- est_fac %>%
  filter(year <= 2019)




# save data as csv
write_delim(est_fac, 
            "./snl/04_output/est_fac/estimation_factors_snl.csv",
            delim = ";", 
            na = "")

















#


















































commodities <- c("Copper", "Gold", "Zinc")
countries <- "Brazil"

# load SNL data from Geoserver --------------------------------------------
fineprint_geoserver <- "http://fineprint.wu.ac.at:8080/geoserver/"
snl_wfs_request <- "snl-2018/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=snl-2018:snl_metals&outputFormat=application/json"
snl_mines_wfs <- stringr::str_glue(fineprint_geoserver, snl_wfs_request)
snl_mines <- httr::GET(url = snl_mines_wfs, httr::authenticate(Sys.getenv("GS_USER"), Sys.getenv("GS_PASS"))) %>%
  sf::st_read(quiet = TRUE) %>%
  sf::st_transform(crs = sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# geometry to column to make object smaller
snl_mines <- dplyr::bind_cols(snl_mines, data.frame(st_coordinates(snl_mines))) %>%
  sf::st_set_geometry(NULL)

# ore grade data from CKAN -----------------------------------------------

# Connect to CKAN using your API key.
ck_setup()

# filter for snl ore grade datasets and download them
fineprintutils::ck_get_datasets() %>%
  dplyr::filter(name.organization == "snl", name == "snl_l1_v1_recovery_rate") %>%
  fineprintutils::ck_download(destpath = "./03_intermediate/01_data_retrieval/snl/")

snl_data <- list()
for (com in commodities){
  load(paste0("destpath/", com, "_recovery_rate.Rdata"))
  snl_data[[com]] <- x %>% dplyr::filter(value > 0) # only mines where rate is available
}
rm(x)


# merge and subset data ---------------------------------------------------

snl_data <- do.call("rbind", snl_data)
data <- snl_data %>% dplyr::left_join(snl_mines %>% dplyr::select(snl_id, country), by = "snl_id") %>%
  dplyr::filter(country %in% countries)



