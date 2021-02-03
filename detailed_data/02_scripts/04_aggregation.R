###### Checking data in terms of plausibility of values

library(tidyverse)



## read files
# harmonized and converted data file
detailed <- read_rds("./detailed_data/03_intermediate/all_tables_converted.rds")

# list of columns
sheets_columns <- read_delim("./detailed_data/01_input/02_lists_and_concordance_tables/sheets_columns.csv", delim = ";")


# sheets
sheet_min <- detailed$minerals_ores_conce
sheet_com <- detailed$commodities



# aggregate sheet_com
col_names <- names(sheet_com)[!names(sheet_com) %in% c("sub_site", "type_mining","value", "amount_sold", "metal_payable", "grade", "yield")]

#sheet_com <- 
sheet_com <- sheet_com %>%
  group_by(across(all_of(col_names))) %>%
  summarise(
    value = sum(value),
    amount_sold = sum(amount_sold),
    metal_payable = sum(metal_payable),
    grade = mean(grade),
    yield = mean(yield)
  ) %>%
  ungroup()


# aggregate sheet_min
col_names <- names(sheet_min)[!names(sheet_min) %in% c("sub_site", "type_mining", "value", "amount_sold", "grade")]

sheet_min <- sheet_min %>%
  group_by(across(all_of(col_names))) %>%
  summarise(
    value = sum(value),
    amount_sold = sum(amount_sold),
    grade = mean(grade)
  ) %>%
  ungroup()

# include sheets again in list
detailed$minerals_ores_conce <- sheet_min
detailed$commodities <- sheet_com


# save data
write_rds(detailed, "./detailed_data/03_intermediate/aggregated.rds")











