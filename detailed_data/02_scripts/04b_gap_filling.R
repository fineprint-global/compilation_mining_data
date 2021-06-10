####### Gap filling based on commodity production values, grades, ore production values

library(tidyverse)
library(rlang)

## read files
# harmonized data file
detailed <- read_rds("./detailed_data/03_intermediate/aggregated.rds")

# list of columns
sheets_columns <- read_delim("./detailed_data/01_input/02_lists_and_concordance_tables/sheets_columns.csv", delim = ";")


# sheets
sheet_min <- detailed$minerals_ores_conce
sheet_com <- detailed$commodities





## add duplicates for type_mineral in sheet_min -----------------
  ## e.g. "Ore processed" from "Ore mined" and vice versa (+ same for coal)

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

# Add "Ore mined" where not reported
sheet_min <- sheet_min %>% 
  union(
    mine_list %>%
      left_join(sheet_min %>% filter(type_mineral == "Ore processed")) %>%
      mutate(type_mineral = "Ore mined")
  )


# mine_fac/year with "Coal mined" or "Clean coal"
coal_mined <- sheet_min %>%
  filter(type_mineral == "Coal mined") %>%
  distinct(mine_fac, year)

coal_clean <- sheet_min %>%
  filter(type_mineral == "Clean coal") %>%
  distinct(mine_fac, year)

# mines with reported coal mined for a year, but no Clean coal for that year
mine_list <- setdiff(coal_mined, coal_clean)

# add "Clean coal" where not reported
sheet_min <- sheet_min %>% 
  union(
    mine_list %>%
      left_join(sheet_min %>% filter(type_mineral == "Coal mined")) %>%
      mutate(type_mineral = "Clean coal")
  )

# mines with reported Clean coal, but no coal mined for that year
mine_list <- setdiff(coal_clean, coal_mined)

# add "Coal mined" where not reported
sheet_min <- sheet_min %>% 
  union(
    mine_list %>%
      left_join(sheet_min %>% filter(type_mineral == "Clean coal")) %>%
      mutate(type_mineral = "Coal mined")
  )





## Gap filling (values) for sheet_min ---------------------

# use amount_sold where value = NA in sheet_min
sheet_min <- sheet_min %>% 
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








# ## remove double counting created due to several ore values for each commodity per mine
#   # i.e. use average of ores estimated based on value.com and grade
# 
# # define columns by which to aggregate (i.e. excluding those which are to be aggregated)
# col_names <- names(sheet_min)[!names(sheet_min) %in% c("value", "amount_sold", "all_metals_overall_grade")]
# 
# # aggregate
# # similar to sheet_com above
# sheet_min <- sheet_min %>%
#   filter(!is.na(value)) %>%
#   group_by(across(all_of(col_names))) %>%
#   summarise(
#     all_metals_overall_grade = if(all(is.na(all_metals_overall_grade))) NA else weighted.mean(x = all_metals_overall_grade, w = value, na.rm = TRUE),
#     value = if(all(is.na(value))) NA else mean(value, na.rm = TRUE),
#     amount_sold = if(all(is.na(amount_sold))) NA else mean(amount_sold, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   union(.,
#         sheet_min %>%
#           filter(is.na(value))
#   ) %>%
#   group_by(across(all_of(col_names))) %>%
#   summarise(
#     all_metals_overall_grade = if(all(is.na(all_metals_overall_grade))) NA else mean(all_metals_overall_grade, na.rm = TRUE),
#     value = if(all(is.na(value))) NA else mean(value, na.rm = TRUE),
#     amount_sold = if(all(is.na(amount_sold))) NA else mean(amount_sold, na.rm = TRUE)
#   ) %>%
#   ungroup()








## Gap filling (values) for sheet_com ---------------------


# use amount_sold where value = NA in sheet_com
sheet_com <- sheet_com %>% 
  mutate(value = ifelse(is.na(value) & !is.na(amount_sold), amount_sold, value))



# join commodities with ores
  # and calculate value based on ore/grade
  # and calculate grades based on ore/value
  # mutate 3x for value: with/without recovery rate and with yield
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







# include sheets again in list
detailed$minerals_ores_conce <- sheet_min
detailed$commodities <- sheet_com


# save data
write_rds(detailed, "./detailed_data/03_intermediate/gaps_filled.rds")








# 
# view(sheet_com)
# view(sheet_min)
# 
# sheet_min %>%
#   group_by(type_mineral) %>%
#   summarise(n = n())
# 
# 
# 
# 
# sheet_min %>% filter(type_mineral == "Ore processed" & !is.na(type_mining))
# 
# 
# 
# 
# a <- sheet_min %>%
#   group_by(type_mineral, min_ore_con, type_mining) %>%
#   summarise(n = n())
# 
# view(a)
# 
# sheet_min %>%
#   distinct(type_mineral, min_ore_con, type_mining)
# 
# 
# 
# a1 <- intersect(sheet_min$mine_fac, sheet_com$mine_fac)
# 
# a2 <- sheet_min %>% 
#   filter(mine_fac %in% a1) %>% 
#   filter(type_mineral == "Ore processed") %>% 
#   distinct(mine_fac, min_ore_con, type_mining)
# 
# a3 <- sheet_com %>% 
#   filter(mine_fac %in% a1) %>% 
#   distinct(mine_fac, min_ore_con, type_mining)
# 
# intersect(a2, a3)
# a <- setdiff(a2, a3)
# view(setdiff(a3, a2))
# 
# 
# 
# 
# 
# 
# 
