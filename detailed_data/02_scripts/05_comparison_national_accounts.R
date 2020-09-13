####### Assessement of coverage vs. national accounts

library(tidyverse)
library(rlang)
library(scales)

## read files
# harmonized data file
detailed <- read_rds("./detailed_data/03_intermediate/gaps_filled.rds")

load("./detailed_data/01_input/05_other_data/wmd_comb.RData")
load("./detailed_data/01_input/05_other_data/bgs.RData")

# sheets
sheet_min <- detailed$minerals_ores_conce
sheet_com <- detailed$commodities




## commodities -------------------

# include country_ids in sheet_com 
agg_com <- sheet_com %>%
  left_join(.,
            detailed$general %>% 
              select(mine, alphanumiso) %>%
              mutate(alphanumiso = ifelse(is.na(alphanumiso), "Other", alphanumiso))
            )

# aggregate 
agg_com <- agg_com %>%
  group_by(alphanumiso, commodity, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()



# temporary: convert units in WMD table
conv_fac <- read_delim("./detailed_data/01_input/04_factors/conversion_factors.csv", delim = ";")

wmd <- wmd_comb %>%
  left_join(.,
            conv_fac %>% select(unit_id, conv_factor_t),
            ) %>%
  mutate(
    value = value * conv_factor_t,
    unit_id = "t"
    )



# join with national accounts
agg_com <- agg_com %>%
  left_join(.,
            wmd %>% select(alphanumiso, product_id, year, value),
            by = c("alphanumiso", "commodity" = "product_id", "year"),
            suffix = c(".det", ".nat")
            )


# share
agg_com <- agg_com %>%
  mutate(
    share = value.det / value.nat,
    share = round(share, digits = 2),
    share = percent(share, accuracy = 1)
    )


# clean table
agg_com <- agg_com %>%
  filter(
    !(year %in% c(2018, 2019)),
    !is.na(alphanumiso),
    !is.na(commodity),
    !is.na(value.nat),
    )



# spread by country_id and material_id
a <- agg_com %>%
  select(-value.det, -value.nat) %>%
  arrange(desc(year)) %>%
  pivot_wider(., names_from = year, values_from = share) %>%
  arrange(commodity) %>%
  # insert national totals again in order to rank properly (couldn't be kept, because of pivot function)
  left_join(., agg_com %>% filter(year == 2015) %>% select(-value.det, -share, -year)) %>%
  arrange(commodity, desc(value.nat))



# spread by global totals for material_ids
a2 <- agg_com %>%
  group_by(commodity, year) %>%
  summarise(value.det = sum(value.det, na.rm = TRUE), value.nat = sum(value.nat, na.rm = TRUE)) %>%
  mutate(
    share = value.det / value.nat,
    share = round(share, digits = 2),
    share = percent(share, accuracy = 1)
  ) %>%
  select(-value.det, -value.nat) %>%
  arrange(desc(year)) %>%
  pivot_wider(., names_from = year, values_from = share) %>%
  arrange(commodity)





## minerals and ores -------------------

# include country_ids in sheet_com 
agg_min <- sheet_min %>%
  left_join(.,
            detailed$general %>% 
              select(mine, alphanumiso) %>%
              mutate(alphanumiso = ifelse(is.na(alphanumiso), "Other", alphanumiso))
  )

# aggregate 
agg_min <- agg_min %>%
  filter(type_mineral %in% c("Ore processed", "Concentrate")) %>%
  filter(!is.na(min_ore_con)) %>%
  group_by(alphanumiso, type_mineral, min_ore_con, year) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()



# temporary: convert units in BGS table
conv_fac <- read_delim("./detailed_data/01_input/04_factors/conversion_factors.csv", delim = ";")

bgs <- bgs %>%
  left_join(.,
            conv_fac %>% select(unit_id, conv_factor_t),
  ) %>%
  mutate(
    value = value * conv_factor_t,
    unit_id = "t"
  )



# join with national accounts
agg_min <- agg_min %>%
  left_join(.,
            bgs %>% select(alphanumiso, product_id, year, value),
            by = c("alphanumiso" = "alphanumiso", "min_ore_con" = "product_id", "year" = "year"),
            suffix = c(".det", ".nat")
  )


# share
agg_min <- agg_min %>%
  mutate(
    share = value.det / value.nat,
    share = round(share, digits = 2),
    share = percent(share, accuracy = 1)
  )


# clean table
agg_min <- agg_min %>%
  filter(
    !(year %in% c(2018, 2019)),
    !is.na(alphanumiso),
    !is.na(min_ore_con),
    !is.na(value.nat),
  )



# spread by country_id and material_id
a3 <- agg_min %>%
  select(-value.det, -value.nat) %>%
  arrange(desc(year)) %>%
  pivot_wider(., names_from = year, values_from = share) %>%
  arrange(min_ore_con) %>%
  # insert national totals again in order to rank properly (couldn't be kept, because of pivot function)
  left_join(., agg_min %>% filter(year == 2015) %>% select(-value.det, -share, -year)) %>%
  arrange(min_ore_con, desc(value.nat))



# spread by global totals for material_ids
a4 <- agg_min %>%
  group_by(min_ore_con, year) %>%
  summarise(value.det = sum(value.det, na.rm = TRUE), value.nat = sum(value.nat, na.rm = TRUE)) %>%
  mutate(
    share = value.det / value.nat,
    share = round(share, digits = 2),
    share = percent(share, accuracy = 1)
  ) %>%
  select(-value.det, -value.nat) %>%
  arrange(desc(year)) %>%
  pivot_wider(., names_from = year, values_from = share) %>%
  arrange(min_ore_con)









##### Other stuff


detailed$general %>%
  group_by(mine) %>%
  summarise(n = n()) %>%
  filter(n > 1)


b <- agg_com %>% filter(alphanumiso == "CHL152")

view(wmd %>% distinct(product_id))
