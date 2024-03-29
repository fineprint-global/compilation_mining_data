####### Gap filling based on commodity production values & grades & ore production values (+ partly recovery rates)



library(tidyverse)
library(rlang)


# clear R environment
rm(list = ls())



## read files
# harmonized data file
detailed <- read_rds("./03_intermediate/01_detailed_data/aggregated.rds")

# list of columns
sheets_columns <- read_delim("./01_input/02_lists_and_concordance_tables/01_detailed_data/sheets_columns.csv", delim = ";")

# source IDs
source_ids <- read_delim("./01_input/02_lists_and_concordance_tables/source_ids.csv", delim = ";")



# sheets
sheet_min <- detailed$minerals_ores_conce
sheet_coal <- detailed$coal
sheet_com <- detailed$commodities
sheet_cap <- detailed$capacity
sheet_res <- detailed$reserves







## add duplicates for type_mineral in sheet_min -----------------
  ## e.g. "Ore processed" from "Ore mined" and vice versa (+ same for coal in sheet_coal)


# mine_fac/year with "Ore mined" or "Ore processed"
ore_mined <- sheet_min %>%
  filter(type_mineral == "Ore mined") %>%
  distinct(mine_fac, year)

ore_proc <- sheet_min %>%
  filter(type_mineral == "Ore processed") %>%
  distinct(mine_fac, year)


# mines with reported ore mined for a year, but no ore processed for that year
mine_list <- setdiff(ore_mined, ore_proc)

# add "Ore processed" where not reported
sheet_min <- sheet_min %>% 
  union(
    mine_list %>%
      left_join(sheet_min %>% filter(type_mineral == "Ore mined")) %>%
      mutate(type_mineral = "Ore processed")
    )

# mines with reported ore processed, but no ore mined for that year
mine_list <- setdiff(ore_proc, ore_mined)

# add "Ore mined" where not reported
sheet_min <- sheet_min %>% 
  union(
    mine_list %>%
      left_join(sheet_min %>% filter(type_mineral == "Ore processed")) %>%
      mutate(type_mineral = "Ore mined")
  )



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








## Gap filling (values) for sheet_min ---------------------


# save temporary object to compare below which values are estimated
temp_cols <- sheet_min



# use amount_sold where value = NA in sheet_min
sheet_min <- sheet_min %>% 
  mutate(
    value = ifelse(is.na(value) & !is.na(amount_sold), amount_sold, value))

# same for sheet_coal: use amount_sold where value = NA in sheet_min
sheet_coal <- sheet_coal %>% 
  mutate(
    value = ifelse(is.na(value) & !is.na(amount_sold), amount_sold, value))



# join with sheet_com and estimate missing values
  # i.e. join ores/concentrates with commodities
  # and calculate ore based on metal value and grade
  # with/without recovery rate and with yield (depending on whether available)
sheet_min <- sheet_min %>% 
  filter(type_mineral %in% c("Ore processed", "Concentrate")) %>%
  left_join(.,
            sheet_com %>%
              select(mine_fac, min_ore_con, year, mine_processing, value, grade, yield, recovery_rate),
            by = c("mine_fac", "min_ore_con", "year", "mine_processing"),
            suffix = c(".min", ".com")
  ) %>%
  mutate(value.min = ifelse(
    is.na(value.min) & !is.na(grade) & !is.na(recovery_rate),
    value.com / (grade * 0.000001 * recovery_rate),
    value.min
  )) %>%
  mutate(value.min = ifelse(
    is.na(value.min) & !is.na(grade) & is.na(recovery_rate),
    value.com / (grade * 0.000001), #  0.000001 because grade is in ppm
    value.min
  )) %>%
  mutate(value.min = ifelse(
    is.na(value.min) & !is.na(yield),
    value.com / (yield * 0.000001),
    value.min
  )) %>%
  select(-c(value.com, grade, yield, recovery_rate)) %>%
  rename(value = value.min) %>% 
  union(.,
    sheet_min %>%
      filter(!(type_mineral %in% c("Ore processed", "Concentrate")))
  )




## adjust sources in sheet ----

# get all column names for join except for estimated column (i.e. value)
col_names <- names(sheet_min)[!names(sheet_min) %in% c("value")]

# include former values in sheet
# check if current values are different (and therefore estimated)
# add info on estimation to source
sheet_min <- sheet_min %>%
  left_join(
    temp_cols,
    by = all_of(col_names),
    suffix = c("", "_check")
  ) %>%
  mutate(
    source = ifelse(
      !is.na(value) & is.na(value_check),
      paste0(source, " ; ", "Estimate (value)"),
      source
    )) %>%
  select(-value_check)








## Gap filling (values) for sheet_com ---------------------


# save temporary object to compare below which values/grades are estimated
temp_cols <- sheet_com



# use amount_sold where value = NA in sheet_com
sheet_com <- sheet_com %>% 
  mutate(value = ifelse(is.na(value) & !is.na(amount_sold), amount_sold, value))



# join commodities with ores
  # and calculate value based on ore/grade
  # and calculate grades based on ore/value
  # mutate 3x for value: with/without recovery rate and with yield
  # mutate 1x for recovery rate
  # mutate 2x for grade: with/with recovery rate
  # filtering NA for value.min is irrelevant, as it just replaces NA with NA anyway
sheet_com <- sheet_com %>%
  left_join(.,
            sheet_min %>% 
              filter(type_mineral %in% c("Ore processed", "Concentrate")) %>%
              select(mine_fac, min_ore_con, year, mine_processing, value),
            by = c("mine_fac", "min_ore_con", "year", "mine_processing"),
            suffix = c(".com", ".min")
            ) %>%
  mutate(value.com = ifelse(
    is.na(value.com) & !is.na(grade) & !is.na(recovery_rate),
    value.min * grade * 0.000001 * recovery_rate,
    value.com
  )) %>%
  mutate(value.com = ifelse(
    is.na(value.com) & !is.na(grade) & is.na(recovery_rate),
    value.min * grade * 0.000001, #  0.000001 because grade is in ppm
    value.com
  )) %>%
  mutate(value.com = ifelse(
    is.na(value.com) & !is.na(yield),
    value.min * yield * 0.000001,
    value.com
  )) %>% 
  mutate(recovery_rate = ifelse(
    is.na(recovery_rate) & !is.na(value.min) & !is.na(value.com) & !is.na(grade),
    value.min / (value.com * grade * 1000000),
    recovery_rate
  )) %>%
  mutate(grade = ifelse(
    is.na(grade) & !is.na(value.com) & !is.na(recovery_rate),
    value.com / recovery_rate / value.min * 1000000 ,
    grade
  )) %>%
  mutate(grade = ifelse(
    is.na(grade) & !is.na(value.com) & is.na(recovery_rate),
    value.com / value.min * 1000000, #  1000000 because grade is in ppm
    grade
  )) %>%
  select(-value.min) %>%
  rename(value = value.com)



# use metal_payable where value is still NA
sheet_com <- sheet_com %>% 
  mutate(value = ifelse(is.na(value) & !is.na(metal_payable), metal_payable, value))




## adjust sources in sheet ----

# get all column names for join except for estimated column (i.e. value/grade)
col_names <- names(sheet_com)[!names(sheet_com) %in% c("value", "grade")]

# include former values/grades in sheet
# check if current values/grades are different (and therefore estimated)
# add info on estimation to source
sheet_com <- sheet_com %>%
  left_join(
    temp_cols,
    by = all_of(col_names),
    suffix = c("", "_check")
  ) %>%
  mutate(
    source = ifelse(
      !is.na(value) & is.na(value_check),
      paste0(source, " ; ", "Estimate (value)"),
      source
    )) %>%
  mutate(
    source = ifelse(
      !is.na(grade) & is.na(grade_check),
      paste0(source, " ; ", "Estimate (grade)"),
      source
    )) %>%
  select(-value_check)




## adjust source_ids and include again ----

# sources of current sheets
sources <- sheet_min %>%
  distinct(source, source_url) %>%
  union(.,
        sheet_coal %>%
          distinct(source, source_url)) %>% 
  union(.,
        sheet_com %>%
          distinct(source, source_url)) %>% 
  union(.,
        sheet_cap %>%
          distinct(source, source_url)) %>%   
  union(.,
        sheet_res %>%
          distinct(source, source_url)) 
  
  
# get new sources
new_sources <- setdiff(sources, source_ids %>% select(source, source_url))

# get the highest number from current source_ids
last_nr <- source_ids$source_id %>% substr(., 5, 8) %>% as.integer() %>% max()

# provide new sources with ids
if(nrow(new_sources) > 0) {
  new_sources <- new_sources %>%
    mutate(source_id = paste0(
      "det_",
      seq(last_nr + 1, last_nr + nrow(new_sources), by=1)
    )
    )
  
  source_ids <- source_ids %>%
    union(., new_sources)
  
}



# replace sources with respective new IDs in both sheets
sheet_min <- sheet_min %>%
  left_join(., source_ids) %>%
  select(-source, -source_url)

sheet_com <- sheet_com %>%
  left_join(., source_ids) %>%
  select(-source, -source_url)

sheet_coal <- sheet_coal %>%
  left_join(., source_ids) %>%
  select(-source, -source_url)

sheet_cap <- sheet_cap %>%
  left_join(., source_ids) %>%
  select(-source, -source_url)

sheet_res <- sheet_res %>%
  left_join(., source_ids) %>%
  select(-source, -source_url)


# save new source_ids
write_delim(source_ids, "./01_input/02_lists_and_concordance_tables/source_ids.csv",
            delim = ";", na = "")





# include sheets again in list
detailed$minerals_ores_conce <- sheet_min
detailed$commodities <- sheet_com
detailed$coal <- sheet_coal
detailed$capacity <- sheet_cap
detailed$reserves <- sheet_res


# save data
write_rds(detailed, "./03_intermediate/01_detailed_data/gaps_filled.rds")














### check whether double counting was introduced during aggregation or gap filling ----

  ### group twice in order to leave out those which have double entries from the same source

# sheet_min
double_min <- sheet_min %>%
  group_by(mine_fac, type_mineral, min_ore_con, year, mine_processing) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  left_join(.,
            sheet_min %>%
              select(mine_fac, type_mineral, min_ore_con, year, mine_processing, source_id)) %>%
  distinct() %>%
  group_by(mine_fac, type_mineral, min_ore_con, year, mine_processing) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  left_join(., sheet_min) %>%
  arrange(mine_fac, type_mineral, min_ore_con, year, mine_processing)

# sheet_com
double_com <- sheet_com %>%
  group_by(mine_fac, min_ore_con, commodity, year, mine_processing) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  left_join(.,
            sheet_com %>%
              select(mine_fac, min_ore_con, commodity, year, mine_processing, source_id)) %>%
  distinct() %>%
  group_by(mine_fac, min_ore_con, commodity, year, mine_processing) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  left_join(., sheet_com) %>%
  arrange(mine_fac, min_ore_con, commodity, year, mine_processing)

# sheet_coal
double_coal <- sheet_coal %>%
  group_by(mine_fac, type_mineral, min_ore_con, year, mine_processing) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  left_join(.,
            sheet_coal %>%
              select(mine_fac, type_mineral, min_ore_con, year, mine_processing, source_id)) %>%
  distinct() %>%
  group_by(mine_fac, type_mineral, min_ore_con, year, mine_processing) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  filter(n > 1) %>%
  left_join(., sheet_coal) %>%
  arrange(mine_fac, type_mineral, min_ore_con, year, mine_processing)
