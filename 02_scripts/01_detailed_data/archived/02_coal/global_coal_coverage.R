library(tidyverse)
library(sf)

coal_production <- read_csv("./04_output/01_detailed_data/07_other/coal_production.csv") %>% tibble()

nat_acc_coal <- read_csv(file = "./04_output/02_country_specific/nat_acc_coal.csv") %>% tibble()
general <- st_read("./04_output/01_detailed_data/07_other/general_coal.gpkg") %>% st_drop_geometry() %>% tibble()

coal_production
nat_acc_coal
general

# include country_ids in coal_production
coal_production <- coal_production %>%
  left_join(.,
            general %>%
              select(mine_fac, alphanumiso),
            by = "mine_fac")


# aggregate, and filter for type_mineral == "Clean coal" only
coal_production <- coal_production %>%
  group_by(alphanumiso, year) %>%
  summarise(value = sum(value, na.rm = TRUE))


# join with national accounts
coal_production <- coal_production %>%
  full_join(.,
            nat_acc,
            by = c("alphanumiso", "year"),
            suffix = c(".det", ".nat")
  )


# share
coal_production <- coal_production %>%
  mutate(
    share = value.det / value.nat,
    share = round(share, digits = 2)
  )



# clean table
coal_production <- coal_production %>%
  filter(
    !is.na(alphanumiso),
    !is.na(value.nat)
  )

# coverage per country per year
coal_cov_per_country <- coal_production %>%
  select(-value.det, -value.nat) %>%
  arrange(desc(year)) %>%
  pivot_wider(., names_from = year, values_from = share) %>%
  # insert national totals again in order to rank properly (couldn't be kept, because of pivot function)
  left_join(., coal_production %>% filter(year == 2018) %>% select(-value.det, -share, -year)) %>%
  arrange(desc(value.nat))


# global coverage per year
coal_cov_global <- coal_production %>%
  group_by(year) %>%
  summarise(value.det = sum(value.det, na.rm = TRUE), value.nat = sum(value.nat, na.rm = TRUE)) %>%
  mutate(
    share = value.det / value.nat,
    share = round(share, digits = 2),
  ) %>%
  arrange(desc(year))

