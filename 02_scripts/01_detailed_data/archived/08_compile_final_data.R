### In this script, the final data output as CSV file is prepared.
# there are several formats in which the final data is published
# In one, heet_min and sheet_com are merged and published as a CSV.
# Alternatively, they are also published as separate files.


# read in data ####

# read in sheet_min and sheet_com
detailed <- read_rds("./03_intermediate/01_detailed_data/gaps_filled.rds")

#read in converted data to get sheet_coal including sub_sites
converted <- read_rds("./03_intermediate/01_detailed_data/all_tables_converted.rds")

# read in sheet_general
sheet_general <- read_csv("./04_output/01_detailed_data/08_final_data/general_sub_sites_georeferenced.csv")
sheet_general_mine_fac_only <- read_csv("./04_output/01_detailed_data/08_final_data/general_georeferenced.csv")


# modify data ####

# get sheet_coal with sub_sites from the converted data
sheet_coal <- converted$minerals_ores_conce %>% 
  filter(type_mineral %in% c("Coal mined", "Clean coal"))

# code copied from 04b_gap_filling.R START ####

# mine_fac/year with "Coal mined" or "Clean coal"
coal_mined <- sheet_coal %>%
  filter(type_mineral == "Coal mined") %>%
  distinct(mine_fac, year)

coal_clean <- sheet_coal %>%
  filter(type_mineral == "Clean coal") %>%
  distinct(mine_fac, year)

# mines with reported coal mined for a year, but no Clean coal for that year
mine_list <- setdiff(coal_mined, coal_clean)

# add "Clean coal" where not reported
sheet_coal <- sheet_coal %>% 
  union(
    mine_list %>%
      left_join(sheet_coal %>% filter(type_mineral == "Coal mined")) %>%
      mutate(type_mineral = "Clean coal")
  )

# mines with reported Clean coal, but no coal mined for that year
mine_list <- setdiff(coal_clean, coal_mined)

# add "Coal mined" where not reported
sheet_coal <- sheet_coal %>% 
  union(
    mine_list %>%
      left_join(sheet_coal %>% filter(type_mineral == "Clean coal")) %>%
      mutate(type_mineral = "Coal mined")
  )

# use amount_sold where value = NA in sheet_min
sheet_coal <- sheet_coal %>% 
  mutate(
    value = ifelse(is.na(value) & !is.na(amount_sold), amount_sold, value))

# code copied from 04b_gap_filling.R END ####





# add mine_id to all sheets
ids <- sheet_general %>% select(mine_id, mine_fac, sub_site)
ids_mine_fac_only <- sheet_general_mine_fac_only %>% select(mine_id, mine_fac)


# for the sheets processing, transport, capacity_reserves, waste, and other info, it can be joined by mine_fac and sub_site
for (i in c(5:9)){
  detailed[[i]] <- left_join(detailed[[i]], ids, by = c("mine_fac", "sub_site")) %>%
    relocate(mine_id, .before = mine_fac)
}


# for the sheet_coal, it can also be joined by mine_fac and sub_site
sheet_coal <- left_join(sheet_coal, ids, by = c("mine_fac", "sub_site"))


# the sheet_min and sheet_com can only be joined with mine_fac, and also only with IDs referring to mine_fac, not sub_site.
sheet_min <- left_join(detailed$minerals_ores_conce, ids_mine_fac_only, by = "mine_fac")
sheet_com <- left_join(detailed$commodities, ids_mine_fac_only, by = "mine_fac")


# delete variables, they do not add value, as it should be at 100%, in tonnes, and in ppm. 
sheet_coal <- sheet_coal %>% 
  select(-production_share, -unit, -grade_unit, -no_estimation, -all_metals_overall_grade, -mine_processing) %>% 
  relocate(mine_id, .before = mine_fac) %>% 
  arrange(mine_id, year, type_mineral)

sheet_min <- sheet_min %>% 
  select(-production_share, -unit, -grade_unit, -no_estimation) %>% 
  relocate(mine_id, .before = mine_fac) %>% 
  arrange(mine_id, year, type_mineral)

sheet_com <- sheet_com %>% 
  select(-production_share, -unit, -grade_or_yield_unit, -no_estimation) %>% 
  relocate(mine_id, .before = mine_fac) %>% 
  arrange(mine_id, year, commodity)





# Format 1: sheet_mineral_coal, sheet_commodity ####

### insert joining ID tbd.


# reorder columns and modify names 
sheet_min_coal1 <- sheet_min %>%
  select(mine_id, year, type_mineral, min_ore_con, value, amount_sold, all_metals_overall_grade, 
         mine_processing, reporting_period, source_id, comment) %>% 
  rename(value_tonnes = value,
         all_metals_overall_grade_ppm = all_metals_overall_grade)


sheet_com1 <- sheet_com %>%
  select(mine_id, year, min_ore_con, commodity, value, grade, recovery_rate, yield, amount_sold, 
         metal_payable, mine_processing, reporting_period, source_id, comment) %>% 
  rename(value_tonnes = value, 
         grade_ppm = grade)


# export the sheets
dir.create("./04_output/01_detailed_data/08_final_data/format_1", showWarnings = FALSE, recursive = TRUE)
write_csv(sheet_min_coal1, "./04_output/01_detailed_data/08_final_data/format_1/sheet_minerals_coal.csv", na = "")
write_csv(sheet_com1, "./04_output/01_detailed_data/08_final_data/format_1/sheet_commodities.csv", na = "")





# Format 2: sheet_coal, sheet_mineral, sheet_commodity ####

# separate coal mines from sheet_min
sheet_min2 <- sheet_min %>% filter(., !(type_mineral %in% c("Coal mined", "Clean coal")))

### insert joining ID tbd.



# reorder columns and modify names 
sheet_coal2 <- sheet_coal %>% 
  select(mine_id, year, type_mineral, min_ore_con, value, amount_sold, reporting_period, source_id, comment) %>% 
  rename(value_tonnes = value)


sheet_min2 <- sheet_min2 %>%
  select(mine_id, year, type_mineral, min_ore_con, value, amount_sold, all_metals_overall_grade, 
         mine_processing, reporting_period, source_id, comment) %>% 
  rename(value_tonnes = value, 
         all_metals_overall_grade_ppm = all_metals_overall_grade)


# export the sheets
dir.create("./04_output/01_detailed_data/08_final_data/format_2", showWarnings = FALSE, recursive = TRUE)
write_csv(sheet_coal2, "./04_output/01_detailed_data/08_final_data/format_2/sheet_coal.csv", na = "")
write_csv(sheet_min2, "./04_output/01_detailed_data/08_final_data/format_2/sheet_minerals.csv", na = "")
write_csv(sheet_com1, "./04_output/01_detailed_data/08_final_data/format_2/sheet_commodities.csv", na = "") #sheet_com is the same as in format 1, just copy it





# Format 3: sheet_coal, sheet_mineral_commodity ####

# join sheet_min and sheet_com where it is possible and it makes sense (i.e. where type_mineral is "Ore processed" or "Concentrate")
sheet_min_com3 <- full_join(
  sheet_min2 %>% # sheet_min2 does not inlcude coal any more
    filter((type_mineral %in% c("Ore processed", "Concentrate"))), # exclude where type_mining is "Ore mined" or "NM mineral"
  sheet_com1, # sheet_com1 already has the new varaible names including unit (e.g. tonnes, ppm)
  by = c("mine_id", "min_ore_con", "year", "mine_processing"), 
  suffix = c("_min", "_com")
)


# cleaning 
### the treatment of sources is likely to be changed
### add that if the sources are equivalent, just keep one entry
sheet_min_com3 <- sheet_min_com3 %>% 
  unite("source_id1", c(source_id_com, source_id_min), sep = "; ", na.rm = TRUE, remove = TRUE) 


# add again the remaining rows (only rows with "Ore mined and NM mineral"). 
sheet_min_com3 <- bind_rows(
  sheet_min_com3, 
  sheet_min2 %>% # sheet_min2 does not inlcude coal any more
    filter(!(type_mineral %in% c("Ore processed", "Concentrate")))) # equivalent to: filter(type_mineral %in% c("Ore mined", "NM mineral"))


# cleaning
### the treatment of sources is likely to be changed
### add that if the sources are equivalent, just keep one entry
sheet_min_com3 <- sheet_min_com3 %>% 
  unite("source_ids", c(source_id1, source_id), sep = "; ", na.rm = TRUE, remove = TRUE)


# checking if the reporting periods match for all entries joined
# think about if this check should be moved to the script intermediate_checks.R 
###
# filter(sheet_production2, !is.na(value_min_tonnes) & !is.na(value_com_tonnes)
#        & !is.na(reporting_period_min) & is.na(reporting_period_com)) %>% nrow() == 0
# 
# filter(sheet_production2, !is.na(value_min_tonnes) & !is.na(value_com_tonnes)
#        & is.na(reporting_period_min) & !is.na(reporting_period_com)) %>% nrow() == 0
###


# reorder columns and rows
sheet_min_com3 <- sheet_min_com3 %>% 
  select(mine_id, year, type_mineral, min_ore_con, value_tonnes_min, amount_sold_min, commodity, value_tonnes_com, amount_sold_com, grade_ppm, recovery_rate, yield, #up to here are the most important variables
         all_metals_overall_grade_ppm,   metal_payable, mine_processing, source_ids, reporting_period_min, reporting_period_com, comment_min, comment_com) %>%
  arrange(mine_id, year, type_mineral) # arranges rows by mine name, year, and type_mineral descending


# export the sheets
dir.create("./04_output/01_detailed_data/08_final_data/format_3", showWarnings = FALSE, recursive = TRUE)
write_csv(sheet_coal2, "./04_output/01_detailed_data/08_final_data/format_3/sheet_coal.csv", na = "")
write_csv(sheet_min_com3, "./04_output/01_detailed_data/08_final_data/format_3/sheet_minerals_commodities.csv", na = "")





# Format 4: sheet_mineral_coal_commodity ####

sheet_min_coal_com4 <- bind_rows(sheet_min_com3, sheet_coal2)

# export the sheet
dir.create("./04_output/01_detailed_data/08_final_data/format_4", showWarnings = FALSE, recursive = TRUE)
write_csv(sheet_coal2, "./04_output/01_detailed_data/08_final_data/format_4/sheet_minerals_coal_commodities.csv", na = "")

