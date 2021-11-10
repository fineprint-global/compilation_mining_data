# Export only the relevant tables with coal mining

library(tidyverse)
library(xlsx)
library(sf)
library(sp)

### read files and load data

## harmonized data file (detailed data)
detailed <- read_rds("./03_intermediate/01_detailed_data/gaps_filled.rds")
general <- st_read("./03_intermediate/01_detailed_data/general_georeferenced.gpkg")

## select only the mines that have coal as commodity produced
sheet_min <- detailed$minerals_ores_conce %>% 
  filter(., type_mineral == "Clean coal") %>% 
  select(c(mine_fac, type_mineral, min_ore_con, year, unit, value, comment, source_id)) %>% 
  group_by(mine_fac, type_mineral, min_ore_con, year) %>% 
  summarize(value = sum(value))


# get list of all mines that produce coal and combine it with list of mines that are only entered in sheet general to produce coal, 
# but which do not have production
mine_list <- unique(sheet_min$mine_fac)
mine_list_general <- general[grepl(c("coal|lignite"), general$commodities_products, ignore.case = TRUE),]
mine_list <- union(mine_list, mine_list_general$mine_fac)


general <- general %>% 
  filter(., mine_fac %in% mine_list)

waste <- detailed$waste %>% 
  filter(., mine_fac %in% mine_list) %>%
  select(!c(commodity, production_share))

other_info <- detailed$other_info %>% filter(., mine_fac %in% mine_list)

reserves <- detailed$capacity_reserves %>% 
  filter(., mine_fac %in% mine_list) %>%
  select(!c(processing_capacity_year,	processing_capacity_unit,	processing_capacity_value, reserves_share,
            grade_unit,	grade, reserves_commodity_unit, reserves_commodity_value))


# write sheet general as geopackage
write_delim(sheet_min, "./04_output/01_detailed_data/07_other/coal_production_georeferenced.csv", delim = ";")

# write sheet general as geopackage
st_write(general, "./04_output/01_detailed_data/07_other/general_coal.gpkg", append = FALSE)

# # write other sheets (reserves, waste, other_info) to excel if needed
# write.xlsx(data.frame(reserves), file="./04_output/01_detailed_data/07_other/coal_tables.xlsx", sheetName="reserves", append=TRUE, row.names=FALSE, showNA=FALSE)
# write.xlsx(data.frame(waste), file="./04_output/01_detailed_data/07_other/coal_tables.xlsx", sheetName="waste", append=TRUE, row.names=FALSE, showNA=FALSE)
# write.xlsx(data.frame(other_info), file="./04_output/01_detailed_data/07_other/coal_tables.xlsx", sheetName="other_info", append=TRUE, row.names=FALSE, showNA=FALSE)

