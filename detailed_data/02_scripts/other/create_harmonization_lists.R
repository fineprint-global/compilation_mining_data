###### This script is only used at the beginning to set up lists and concordance tables for harmonization

library(tidyverse)
library(readxl)

#### read excel and id/concordance tables
path <- "./detailed_data/01_input/01_data/detailed_data_mining.xlsx"
detailed <- lapply(excel_sheets(path), read_excel, path = path)
names(detailed) <- excel_sheets(path)

mat_ids <- read_delim("./detailed_data/01_input/02_lists_and_concordance_tables/material_ids.csv", delim = ";")
source_mat_ids <- read_delim("./detailed_data/01_input/02_lists_and_concordance_tables/source_material_ids.csv", delim = ";")

unit_ids <- read_delim("./detailed_data/01_input/02_lists_and_concordance_tables/unit_ids.csv", delim = ";")

country_ids <- read_delim("./detailed_data/01_input/02_lists_and_concordance_tables/country_ids.csv", delim = ";") %>%
  mutate(country = ifelse(alphanumiso == "CUW531", "Curaçao", country),
         iso_country = ifelse(alphanumiso == "CUW531", "Curaçao", iso_country),
         wu_former_name = ifelse(alphanumiso == "CUW531", "Curaçao", wu_former_name))

sheets_columns <- read_delim("./detailed_data/01_input/02_lists_and_concordance_tables/sheets_columns.csv", delim = ";")



## Create list of all sheets and columns and column types -----------------------
  ## Afterwards adjusted by hand in Excel

if(exists("sheets_columns")) rm(sheets_columns)

sheets_columns <- tibble(sheet = NA, variable = NA)
sheets_columns <- sheets_columns %>% filter(!is.na(sheet))

for (i in names(detailed)) {

  a <- tibble(
    sheet = i,
    variable = names(detailed[[i]]),
    current_type = sapply(detailed[[i]], typeof)
  )
  
  sheets_columns <- sheets_columns %>%
    rbind(., a)
  
}

write_delim(sheets_columns, "./detailed_data/01_input/02_lists_and_concordance_tables/sheets_columns_initial.csv",
            delim = ";", na = "")



## Create source concordance tables -----------------------

# Create source_material_id table ----

if(exists("source_materials")) rm(source_materials)

sheet_columns_mat <- sheets_columns %>% filter(check == "material_id")

source_materials <- tibble(material = NA)
class(source_materials[[1]]) <- "character"

for (i in 1:nrow(sheet_columns_mat)) {
  
  sheet_name <- sheet_columns_mat[i,1] %>% pull()
  variable_name <- sheet_columns_mat[i,2] %>% pull()

  a <- tibble(
    material = detailed[[sheet_name]] %>%
      select(all_of(variable_name)) %>%
      distinct() %>%
      filter(!is.na(.)) %>%
      pull()
  )

  source_materials <- source_materials %>%
    dplyr::union(., a)
  
}

source_materials <- source_materials %>% filter(!is.na(material))

# "pre-match" with our material_names and material_ids where possible
source_materials <- source_materials %>%
  left_join(
    mat_ids %>%
      select(material_id, material_name),
    by = c("material" = "material_name")
  )

write_delim(source_materials, "./detailed_data/01_input/02_lists_and_concordance_tables/source_materials.csv", delim = ";")



# Create source_unit_id table ----

if(exists("source_units")) rm(source_units)

sheet_columns_unit <- sheets_columns %>% filter(check == "unit_id")

source_units <- tibble(unit = NA)
class(source_units[[1]]) <- "character"

for (i in 1:nrow(sheet_columns_unit)) {
  
  sheet_name <- sheet_columns_unit[i,1] %>% pull()
  variable_name <- sheet_columns_unit[i,2] %>% pull()
  
  a <- tibble(
    unit = detailed[[sheet_name]] %>%
      select(all_of(variable_name)) %>%
      distinct() %>%
      filter(!is.na(.)) %>%
      pull() %>%
      as.character()
  )
  
  source_units <- source_units %>%
    union(., a)
  
}

source_units <- source_units %>% filter(!is.na(unit))

# "pre-match" with our unit_names and unit_ids where possible
source_units <- source_units %>%
  left_join(
    unit_ids %>%
      select(unit_id, unit_name),
    by = c("unit" = "unit_name")
  )

write_delim(source_units, "./detailed_data/01_input/02_lists_and_concordance_tables/source_units.csv", delim = ";")



# Create table for check of possible entries -----------------------
if(exists("poss_ent")) rm(poss_ent)

poss_ent <- tibble(variable = NA, entries = NA)

class(poss_ent[[1]]) <- "character"
class(poss_ent[[2]]) <- "character"

sheet_columns_poss <- sheets_columns %>% filter(check == "poss_ent")

i <- 2

for (i in 1:nrow(sheet_columns_poss)) {
  
  sheet_name <- sheet_columns_poss[i,1] %>% pull()
  variable_name <- sheet_columns_poss[i,2] %>% pull()
  
  a <- tibble(
    variable = variable_name,
    entries = detailed[[sheet_name]] %>%
      select(all_of(variable_name)) %>%
      distinct() %>%
      filter(!is.na(.)) %>%
      pull() %>%
      as.character()
  )

  poss_ent <- poss_ent %>%
    dplyr::union(., a)
  
}

poss_ent <- poss_ent %>% filter(!is.na(variable) | !is.na(entries))

write_delim(poss_ent, "./detailed_data/01_input/02_lists_and_concordance_tables/possible_entries_draft.csv", delim = ";")



# Create country_id table ----

country_entries <- tibble(
  sheet_country = detailed[["general"]] %>%
    select(all_of("country")) %>%
    distinct() %>%
    filter(!is.na(.)) %>%
    pull() %>%
    as.character()
)

# "pre-match" with our alphanumisos and country names where possible
country_entries <- country_entries %>%
  full_join(
    country_ids %>%
      select(alphanumiso, country),
    by = c("sheet_country" = "country")
  ) %>%
  left_join(
    country_ids %>%
      select(alphanumiso, country)
  ) %>%
  arrange(sheet_country)

write_delim(country_entries, "./detailed_data/01_input/02_lists_and_concordance_tables/country_entries.csv", delim = ";")




