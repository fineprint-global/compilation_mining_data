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

