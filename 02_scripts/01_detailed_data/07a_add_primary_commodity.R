# In this script, the column primary commodity is added.
# for most non-poly-metallic mines, the primary commodity is typically unambiguous and is determined by the commodities produced
# for poly-metallic mines, the primary metal based on the mine's aggregated production, 
# multiplied with the average commodity price determines the most valuable - and thus primary - commodity.

# clear R environment
rm(list = ls())

library(sf)
library(tidyverse)
library(readr)

source("./02_scripts/00_functions/get_other_material_ids.R")


# read in data
general <- st_read("./03_intermediate/01_detailed_data/sheet_general/07_general.gpkg")
detailed <- read_rds("./03_intermediate/01_detailed_data/gaps_filled.rds")
mat_ids <- read_csv2("./01_input/02_lists_and_concordance_tables/material_ids.csv")
prices <- read_csv2("./01_input/05_other_data/average_prices_2000-2020.csv")



# classify mines according to their materials mined ############

# get all commodities that are produced by every mine
unique_min <- detailed$minerals_ores_conce %>%
  group_by(mine_fac) %>%
  summarize(material_ids = min_ore_con %>% unique()) %>%
  ungroup() %>% 
  filter(!(material_ids %in% "O.bulk")) # filter out entries with bulk ore and concentrate, as it does not help in specifing the primary material

unique_com <- detailed$commodities %>% 
  group_by(mine_fac) %>%
  summarize(material_ids = commodity %>% unique()) %>%
  ungroup()

unique_coal <- detailed$coal %>% 
  group_by(mine_fac) %>%
  summarize(material_ids = min_ore_con %>% unique()) %>%
  ungroup() 

unique_all <- bind_rows(unique_min, unique_com, unique_coal) %>% 
  group_by(mine_fac) %>%
  summarize(materials_produced = paste(unique(material_ids), collapse = "; ")) %>%
  ungroup() %>% 
  arrange(mine_fac)

# add column materials_produced to table general
general <- general %>% 
  left_join(unique_all, by = "mine_fac")



# define materials which can be included in materials_produced to belong to a category primary_material
coal_ids <- c(aggregate_mat_ids("material_category_2", "coal & peat", mat_ids), "F.stc")

iron_ids <- c("O.Fe", "Me.Fe", "Con.Fe", "Con.FeN", "Oth.sif")

aluminium_ids <- c("O.Al", "Me.Al2O3")

polymetallic_ids <- c(aggregate_mat_ids("material_category_2", "metal ore", mat_ids),
                      aggregate_mat_ids("material_category_2", "ore concentrate", mat_ids),
                      aggregate_mat_ids("material_category_2", "metal", mat_ids),
                      aggregate_mat_ids("material_category_2", "metal compound", mat_ids),
                      aggregate_mat_ids("material_category_2", "metal product", mat_ids),
                      aggregate_mat_ids_agg("material_category_2", "metal", mat_ids))

# assign primary commodity
general <- general %>% 
  mutate(primary_commodity = case_when(grepl("Refinery|Smelter", mine_or_processing) & 
                                         !(grepl("Mine", mine_or_processing)) ~ "Processing",
                                       grepl(paste(coal_ids, collapse = "|"), materials_produced) ~ "Coal",
                                       grepl(paste(iron_ids, collapse = "|"), materials_produced) ~ "Iron",
                                       grepl(paste(aluminium_ids, collapse = "|"), materials_produced) ~ "Aluminium",
                                       grepl(paste(polymetallic_ids, collapse = "|"), materials_produced) ~ "Other (poly)-metallic",
                                       TRUE ~ "Other mine"))

# allocate polymetallic mines to their economically most valuable commodity mined #######

# get total production for each commodity for each mine
total_com <- detailed$commodities %>%
  mutate(commodity = ifelse(commodity == "Oth.cuca", "Me.Cu", commodity)) %>% 
  group_by(mine_fac, commodity) %>%
  summarize(total_value = sum(value)) %>%
  ungroup() %>% 
  rename(material_id = commodity)

# join commodity prices and calculate total price/value per commodity
total_com <- total_com %>% 
  left_join(prices %>% 
              select(material_id, average_price, material_name),
            by = "material_id") %>% 
  mutate(average_price = average_price %>% as.numeric()) %>% 
  mutate(total_price = 1000 * average_price * total_value)

# only select commodity with highest value
highest_value <- total_com %>% 
  group_by(mine_fac) %>% 
  slice_max(total_price) %>% 
  select(mine_fac, material_id, material_name) %>% 
  rename(prim_com_price = material_id,
         prim_com_price_name = material_name)

# join commodity with highest value, and replace primary commodity
general <- general %>% 
  left_join(highest_value, by = "mine_fac") %>% 
  mutate(primary_commodity = ifelse(!(is.na(prim_com_price_name)), prim_com_price_name, primary_commodity))

# aggregate some categories again
categories_to_poly <- general %>%
  st_drop_geometry() %>%
  group_by(primary_commodity) %>%
  summarize(n = n()) %>% 
  filter(n < 10) %>% 
  select(primary_commodity) %>% 
  pull()
  
general <- general %>% 
  mutate(primary_commodity = ifelse(!(is.na(prim_com_price_name)) & primary_commodity %in% categories_to_poly, "Other (poly)-metallic", primary_commodity)) %>% 
  select(-prim_com_price, -prim_com_price_name, materials_produced) %>% 
  relocate(primary_commodity, .after = commodities_products)


# export again
st_write(general, "./03_intermediate/01_detailed_data/sheet_general/07a_general.gpkg", append = FALSE)




# # checking
# 
# # check how many entries per category
# general %>%
#   st_drop_geometry() %>%
#   group_by(primary_commodity) %>%
#   summarize(n())
# 
# # check what unique combinations of commodities correspond to one category primary commodity
# general %>%
#   st_drop_geometry() %>%
#   filter(primary_commodity == "Other (poly)-metallic") %>%
#   select(materials_produced) %>%
#   pull() %>%
#   unique()
