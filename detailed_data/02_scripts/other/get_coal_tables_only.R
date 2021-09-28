# Export only the relevant tables with coal mining for the World Bank

library(tidyverse)
library(xlsx)
library(stringr)

### read files and load data

## harmonized data file (detailed data)
detailed <- read_rds("./detailed_data/03_intermediate/gaps_filled.rds")


## select only the mines that have coal as commodity produced
sheet_min <- detailed$minerals_ores_conce %>% 
  filter(., type_mineral %in% c("Coal mined", "Clean coal")) %>% 
  select(c(mine_fac, type_mineral, min_ore_con, year, unit, value, amount_sold, comment, source_id))

# get list of all mines that produce coal and combine it with list of mines that are only entered in sheet general to produce coal, 
# but which do not have production
mine_list <- unique(sheet_min$mine_fac)
mine_list_general <- detailed$general[grepl(c("coal|lignite"), detailed$general$commodities_products, ignore.case = TRUE),]
mine_list <- union(mine_list, mine_list_general$mine_fac)


general <- detailed$general %>% filter(., mine_fac %in% mine_list)
coords_before <- sum(!is.na(general$longitude))

sub_sites <- detailed$sub_sites %>% filter(., mine_fac %in% mine_list)

waste <- detailed$waste %>% 
  filter(., mine_fac %in% mine_list) %>%
  select(!c(commodity, production_share))

other_info <- detailed$other_info %>% filter(., mine_fac %in% mine_list)

reserves <- detailed$capacity_reserves %>% 
  filter(., mine_fac %in% mine_list) %>%
  select(!c(processing_capacity_year,	processing_capacity_unit,	processing_capacity_value, reserves_share,
            grade_unit,	grade, reserves_commodity_unit, reserves_commodity_value))


## compile two large excel sheets:
## one which describes the mines in general, which is a combination of sheet_general and sheet_sub_sites
## the second which describes production, reserves, waste, and other info (possibly do this sheet in the future? not yet implemented!!)

#first, create a sheet describing the mine in general.
#there should be a unique row for each mine_fac (aggregated), but also for each sub_site.
#therefore, mine_fac and sub_site together form the unique primary key
#an empty (NA) value in sub_site indicates that the mine as a whole (aggregated) is referred to.

#first, create a column "sub_site" which is empty for the sheet general
general <- add_column(general, sub_site = NA, .after = "mine_fac")

#then, bind the sheet general and sheet_sub_sites "beneath" each other
general <- bind_rows(general, sub_sites)

# the only column that has a different naming in the two sheets but refers to the same is:
# mining_facility_types (in sheet_general) and mine_types (in sheet_sub_sites).
# therefore, they are put together into one column, named mining_facility_types
general$mining_facility_types <- coalesce(general$mining_facility_types, general$mine_types)
general <- select(general, -mine_types)

# now as described above, the column mine_fac and sub_site together act as the primary key.
# to simplify, a new column mine_id is created, which simply is the two strings bound together. 
# this leads to the (aggregated) mine always having its original mine_fac string as mine_id, while sub_sites have 
# their mine_id as a composite of mine_fac and sub_site. The columns mine_fac and sub_site will be kept in order to easily
# identify which parent mine_fac a sub_site belongs to.
general$mine_id <- paste(general$mine_fac, general$sub_site, sep = " ")
general <- relocate(general, mine_id, .before = "mine_fac")
general$mine_id <- gsub(pattern = " NA", replacement = "", general$mine_id) #cleaning
coords_after <- sum(!is.na(general$longitude))

#check how many coordinates got added by the addition of sub_sites
coords_after - coords_before


write.xlsx(data.frame(general), file="./detailed_data/04_output/07_other/coal_tables.xlsx", sheetName="general", row.names=FALSE, showNA=FALSE)
write.xlsx(data.frame(sheet_min), file="./detailed_data/04_output/07_other/coal_tables.xlsx", sheetName="sheet_min", append=TRUE, row.names=FALSE, showNA=FALSE)
write.xlsx(data.frame(reserves), file="./detailed_data/04_output/07_other/coal_tables.xlsx", sheetName="reserves", append=TRUE, row.names=FALSE, showNA=FALSE)
write.xlsx(data.frame(waste), file="./detailed_data/04_output/07_other/coal_tables.xlsx", sheetName="waste", append=TRUE, row.names=FALSE, showNA=FALSE)
write.xlsx(data.frame(other_info), file="./detailed_data/04_output/07_other/coal_tables.xlsx", sheetName="other_info", append=TRUE, row.names=FALSE, showNA=FALSE)

