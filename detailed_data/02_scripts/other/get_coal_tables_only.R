# Export only the relevant tables with coal mining for the World Bank

library(tidyverse)
library(xlsx)

### read files and load data

## harmonized data file (detailed data)
detailed <- read_rds("./detailed_data/03_intermediate/gaps_filled.rds")


## select only the mines that have coal as commodity produced
sheet_min <- detailed$minerals_ores_conce %>% 
  filter(., type_mineral %in% c("Coal mined", "Clean coal")) %>% 
  select(c(mine_fac, type_mineral, min_ore_con, year, unit, value, amount_sold, comment, source_id))

#get list of all mines that produce coal
mine_list <- unique(sheet_min$mine_fac)


general <- detailed$general %>% filter(., mine_fac %in% mine_list)

sub_sites <- detailed$sub_sites %>% filter(., mine_fac %in% mine_list)

waste <- detailed$waste %>% 
  filter(., mine_fac %in% mine_list) %>%
  select(!c(commodity, production_share))

other_info <- detailed$other_info %>% filter(., mine_fac %in% mine_list)

reserves <- detailed$capacity_reserves %>% 
  filter(., mine_fac %in% mine_list) %>%
  select(!c(processing_capacity_year,	processing_capacity_unit,	processing_capacity_value, reserves_share,
            grade_unit,	grade, reserves_commodity_unit, reserves_commodity_value))


write.xlsx(data.frame(general), file="./detailed_data/04_output/07_other/coal_tables.xlsx", sheetName="general", row.names=FALSE, showNA=FALSE)
write.xlsx(data.frame(sub_sites), file="./detailed_data/04_output/07_other/coal_tables.xlsx", sheetName="sub_sites", append=TRUE, row.names=FALSE, showNA=FALSE)
write.xlsx(data.frame(sheet_min), file="./detailed_data/04_output/07_other/coal_tables.xlsx", sheetName="sheet_min", append=TRUE, row.names=FALSE, showNA=FALSE)
write.xlsx(data.frame(reserves), file="./detailed_data/04_output/07_other/coal_tables.xlsx", sheetName="reserves", append=TRUE, row.names=FALSE, showNA=FALSE)
write.xlsx(data.frame(waste), file="./detailed_data/04_output/07_other/coal_tables.xlsx", sheetName="waste", append=TRUE, row.names=FALSE, showNA=FALSE)
write.xlsx(data.frame(other_info), file="./detailed_data/04_output/07_other/coal_tables.xlsx", sheetName="other_info", append=TRUE, row.names=FALSE, showNA=FALSE)


########## This part is not finished yet and it is not sure if it is even needed

# ## compile a large excel sheet with all the different sheets joined
# 
# #first, create a sheet describing the mine in general. 
# #there should be a unique row for each mine_fac (aggregated), but also for each sub_site.
# #therefore, mine_fac and sub_site together form the unique primary key
# #an empty (NA) value in sub_site indicates that the mine as a whole (aggregated) is referred to.
# 
# #first, create a column "sub_site" which is empty for the two sheets where no such column is specified. 
# #this ensures that the aggregated mine_fac is referred to
# general <- add_column(general, sub_site = NA, .after = "mine_fac")
# sheet_min <- add_column(sheet_min, sub_site = NA, .after = "mine_fac")
# 
# 
# #second, construct a table with a row for each subsite, as well as for each mine aggregated
# #as specified above, the aggregated mine is meant when sub_site is empty (NA)
# 
# #include info from sheet general in sheet sub_site
# sub_sites_additional_info <- left_join(sub_sites, 
#                                        general %>% select(., mine_fac, country, alphanumiso, state, region, 
#                                                           province, district, sector, location_municipality, comment, source_id),
#                                        by = "mine_fac",
#                                        suffix = c(".general", ".sub_site"))
# 
# #then join the sheet general and sheet sub_site
# describing_table <- full_join(general, sub_sites_additional_info, 
#                                 by=c("mine_fac","sub_site", "country", "alphanumiso", "state", "region", "province", "district", "sector", 
#                                      "location_municipality", "latitude", "longitude", "surface_area", "unit_surface_area", "claim_concession_area", 
#                                      "unit_concession_area", "mine_or_processing", "commodities_products", "production_start", "production_end", 
#                                      "activity_status", "year_activity_status"), 
#                                 suffix = c(".general", ".sub_site"))
# 
# 
# describing_table
# filter(describing_table, mine_fac == "Tuhup")
# 
# #third, join the mine-describing table with the other tables
# coal_tables_joined <- full_join(describing_table, sheet_min, by = c("mine_fac", "sub_site"))
# coal_tables_joined
# 
# 


