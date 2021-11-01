# Export only the relevant tables with coal mining for the World Bank

library(tidyverse)
library(xlsx)

### read files and load data

## harmonized data file (detailed data)
detailed <- read_rds("./03_intermediate/01_detailed_data/gaps_filled.rds")
general <- st_read("./03_intermediate/01_detailed_data/general_georeferenced.gpkg")

## select only the mines that have coal as commodity produced
sheet_min <- detailed$minerals_ores_conce %>% 
  filter(., type_mineral == "Clean coal") %>% 
  select(c(mine_fac, type_mineral, min_ore_con, year, unit, value, amount_sold, comment, source_id))

# add the variable mine_id to sheet_min (in this case it is just identical to mine_fac)
sheet_min$mine_id <- sheet_min$mine_fac
sheet_min <- relocate(sheet_min, mine_id, .before = "mine_fac")

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

#join production with coordinates from sheet general
coal_production <- merge(sheet_min, select(general, mine_id, country, alphanumiso, commodities_products, mining_facility_types, geom), by = "mine_id")

# write sheet general as geopackage
st_write(coal_production, "./04_output/01_detailed_data/07_other/coal_production_georeferenced.gpkg", append = FALSE)

# write sheet general as geopackage
st_write(general, "./04_output/01_detailed_data/07_other/general_coal.gpkg", append = FALSE)

# # write production data as excel file
# write.xlsx(data.frame(sheet_min), file="./04_output/01_detailed_data/07_other/coal_tables.xlsx", sheetName="sheet_min", row.names=FALSE, showNA=FALSE)
# write.xlsx(data.frame(reserves), file="./04_output/01_detailed_data/07_other/coal_tables.xlsx", sheetName="reserves", append=TRUE, row.names=FALSE, showNA=FALSE)
# write.xlsx(data.frame(waste), file="./04_output/01_detailed_data/07_other/coal_tables.xlsx", sheetName="waste", append=TRUE, row.names=FALSE, showNA=FALSE)
# write.xlsx(data.frame(other_info), file="./04_output/01_detailed_data/07_other/coal_tables.xlsx", sheetName="other_info", append=TRUE, row.names=FALSE, showNA=FALSE)

