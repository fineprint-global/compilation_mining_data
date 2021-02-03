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



## Add duplicates for "Ore processed" from "Ore mined" and vice versa
ore_mined <- sheet_min %>%
  filter(type_mineral == "Ore mined") %>%
  distinct(mine_fac, year)

ore_proc <- sheet_min %>%
  filter(type_mineral == "Ore processed") %>%
  distinct(mine_fac, year)

# mines with reported ore mined for a year, but no ore processed for that year
mine_list <- setdiff(ore_mined, ore_proc)

sheet_min <- sheet_min %>% 
  union(
    mine_list %>%
      left_join(sheet_min %>% filter(type_mineral == "Ore mined")) %>%
      mutate(type_mineral = "Ore processed")
    )

# mines with reported ore processed, but no ore mined for that year
mine_list <- setdiff(ore_proc, ore_mined)

sheet_min <- sheet_min %>% 
  union(
    mine_list %>%
      left_join(sheet_min %>% filter(type_mineral == "Ore processed")) %>%
      mutate(type_mineral = "Ore mined")
  )



## Add duplicates for "Clean coal" from "Coal mined" and vice versa

ore_mined <- sheet_min %>%
  filter(type_mineral == "Coal mined") %>%
  distinct(mine_fac, year)

ore_proc <- sheet_min %>%
  filter(type_mineral == "Clean coal") %>%
  distinct(mine_fac, year)

# mines with reported coal mined for a year, but no Clean coal for that year
mine_list <- setdiff(ore_mined, ore_proc)

sheet_min <- sheet_min %>% 
  union(
    mine_list %>%
      left_join(sheet_min %>% filter(type_mineral == "Coal mined")) %>%
      mutate(type_mineral = "Clean coal")
  )

# mines with reported Clean coal, but no coal mined for that year
mine_list <- setdiff(ore_proc, ore_mined)

sheet_min <- sheet_min %>% 
  union(
    mine_list %>%
      left_join(sheet_min %>% filter(type_mineral == "Clean coal")) %>%
      mutate(type_mineral = "Coal mined")
  )



# use amount_sold where value = NA in sheet_com
sheet_com <- sheet_com %>% 
  mutate(value = ifelse(is.na(value) & !is.na(amount_sold), amount_sold, value))



# join commodities with ores
  # and calculate value based on ore/grade
  # and calculate grades based on ore/value
  # with/without recovery rate and with yield
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
  select(-value.min)



# use metal_payable where value is still NA
sheet_com <- sheet_com %>% 
  mutate(value.com = ifelse(is.na(value.com) & !is.na(metal_payable), metal_payable, value.com))





# rename variables
sheet_com <- sheet_com %>%
  rename(value = value.com)



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
