###### Checking data in terms of plausibility of values

library(tidyverse)



## read files and define tables --------------
# harmonized and converted data file
detailed <- read_rds("./03_intermediate/01_detailed_data/all_tables_converted.rds")

# list of columns
sheets_columns <- read_delim("./01_input/02_lists_and_concordance_tables/01_detailed_data/sheets_columns.csv", delim = ";")


# sheets
sheet_min <- detailed$minerals_ores_conce
sheet_com <- detailed$commodities



## sheet_com -----------

# define columns by which to aggregate (i.e. excluding those which are to be aggregated)
col_names <- names(sheet_com)[!names(sheet_com) %in% c("sub_site", "type_mining","value", "amount_sold", "metal_payable", "grade", "yield")]

# aggregate
  # two-step approach, because grade and yield first are weighted based on value
    # and if weight is NA, then grade is set to NA as well (for which ifelse function doesn't work)
  # therefore first everything where value (i.e. the weight) is not NA, is aggregated with weighted.mean()
  # and then everything where value is NA, is aggregated just with mean()
  # also, for upper aggregation, grade and yield go first, because they're based on value which is also aggregated afterwards
  # furthermore, no point in also including amount_sold as weight if value is NA, because too few cases: sheet_com %>% filter(is.na(value) & !is.na(grade) & !is.na(amount_sold))

sheet_com <- sheet_com %>%
  filter(!is.na(value)) %>%
  group_by(across(all_of(col_names))) %>%
  summarise(
    grade = if(all(is.na(grade))) NA else weighted.mean(x = grade, w = value, na.rm = TRUE),
    yield = if(all(is.na(yield))) NA else weighted.mean(x = yield, w = value, na.rm = TRUE),
    value = sum(value, na.rm = TRUE),
    amount_sold = if(all(is.na(amount_sold))) NA else sum(amount_sold, na.rm = TRUE),
    metal_payable = if(all(is.na(metal_payable))) NA else sum(metal_payable, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  union(.,
        sheet_com %>%
          select(-sub_site, -type_mining) %>%
          filter(is.na(value))
        ) %>%
  group_by(across(all_of(col_names))) %>%
  summarise(
    grade = if(all(is.na(grade))) NA else mean(grade, na.rm = TRUE),
    yield = if(all(is.na(yield))) NA else mean(yield, na.rm = TRUE),
    value = if(all(is.na(value))) NA else sum(value, na.rm = TRUE),
    amount_sold = if(all(is.na(amount_sold))) NA else sum(amount_sold, na.rm = TRUE),
    metal_payable = if(all(is.na(metal_payable))) NA else sum(metal_payable, na.rm = TRUE)
  ) %>%
  ungroup() 







## sheet_min ---------------

# define columns by which to aggregate (i.e. excluding those which are to be aggregated)
col_names <- names(sheet_min)[!names(sheet_min) %in% c("sub_site", "type_mining", "value", "amount_sold", "all_metals_overall_grade")]

# aggregate
  # similar to sheet_com above
sheet_min <- sheet_min %>%
  filter(!is.na(value)) %>%
  group_by(across(all_of(col_names))) %>%
  summarise(
    all_metals_overall_grade = if(all(is.na(all_metals_overall_grade))) NA else weighted.mean(x = all_metals_overall_grade, w = value, na.rm = TRUE),
    value = if(all(is.na(value))) NA else sum(value, na.rm = TRUE),
    amount_sold = if(all(is.na(amount_sold))) NA else sum(amount_sold, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  union(.,
        sheet_min %>%
          select(-sub_site, -type_mining) %>%
          filter(is.na(value))
  ) %>%
  group_by(across(all_of(col_names))) %>%
  summarise(
    all_metals_overall_grade = if(all(is.na(all_metals_overall_grade))) NA else mean(all_metals_overall_grade, na.rm = TRUE),
    value = if(all(is.na(value))) NA else sum(value, na.rm = TRUE),
    amount_sold = if(all(is.na(amount_sold))) NA else sum(amount_sold, na.rm = TRUE)
  ) %>%
  ungroup() 






# include sheets again in list
detailed$minerals_ores_conce <- sheet_min
detailed$commodities <- sheet_com


# save data
write_rds(detailed, "./03_intermediate/01_detailed_data/aggregated.rds")











