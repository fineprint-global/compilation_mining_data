#### Conversion of units (Peru MINEM)


library(tidyverse)

# clear R environment
rm(list = ls())


### read files

# data
peru_minem <- read_rds("./03_intermediate/02_country_specific/peru/peru_harmonized.rds")

# conversion factors
conv_fac <- read_delim("./01_input/04_factors/conversion_factors.csv", delim = ";")





# do in loop for all tables

for (i in c("sheet_min", "sheet_coal", "sheet_com", "processing")) {
  
  sheet <- peru_minem[[i]]
  
  # include factors
    # simple, because only mass units
  sheet <- sheet %>%
    left_join(., conv_fac %>%
                       select(unit_id, conv_factor_t)
              )
  
  # convert
  sheet <- sheet %>%
    mutate(
      value = value * conv_factor_t,
      unit_id = "t"
    ) %>%
    select(-conv_factor_t)
  
  peru_minem[[i]] <- sheet
  
  }



# save
write_rds(peru_minem, file = "./03_intermediate/02_country_specific/peru/peru_converted.rds")

