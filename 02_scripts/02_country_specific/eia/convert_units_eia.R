#### Conversion of units (EIA)


library(tidyverse)

# clear R environment
rm(list = ls())


### read files

# data
eia <- read_rds("./03_intermediate/02_country_specific/eia/eia_harmonized.rds")

# conversion factors
conv_fac <- read_delim("./01_input/04_factors/conversion_factors.csv", delim = ";")



# get production table
sheet_coal <- eia$sheet_coal



# include factors
  # simple, because only for short tons
sheet_coal <- sheet_coal %>%
  left_join(., conv_fac %>%
              select(unit_id, conv_factor_t)
            )


# convert
sheet_coal <- sheet_coal %>%
  mutate(
    value = value * conv_factor_t,
    unit_id = "t"
    ) %>%
  select(-conv_factor_t)



# save
eia$sheet_coal <- sheet_coal

write_rds(eia, file = "./03_intermediate/02_country_specific/eia/eia_converted.rds")

