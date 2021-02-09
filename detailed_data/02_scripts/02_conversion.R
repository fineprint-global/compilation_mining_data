####### Convert all absolute values to tonnes and all grades to ppm
  ####### Plus conversion by production and reserves shares 


library(tidyverse)
library(rlang)


## read files
# harmonized data file
detailed <- read_rds("./detailed_data/03_intermediate/all_tables_harmonized.rds")

# conversion factors
conv_fac <- read_delim("./detailed_data/01_input/04_factors/conversion_factors.csv", delim = ";")

# list of columns
sheets_columns <- read_delim("./detailed_data/01_input/02_lists_and_concordance_tables/sheets_columns.csv", delim = ";")


## select all relevant columns for conversion and replacement
sel_col <- sheets_columns %>% 
  filter(!is.na(conv_unit_col)) %>%
  filter(sheet != "water_input")


## convert all relevant columns in a loop
  ## (including two nested loops, one for value columns based on grades, second for grade columns)
for (i in sel_col %>% distinct(sheet) %>% pull()) {
  
  sheet <- detailed[[i]]
  
  # filter for only the relevant columns in this sheet
  sel_col_filtered <- sel_col %>% filter(sheet == i)
  
  
  
  # first nested loop for each variable in the current sheet, converting it based on unit
    # converting and replacing units is done in two nested loops because some columns share the same unit column in sheet "commodities"
    # (other possibility would have been to add a unit column for each, but this option has been discarded for now)
  for (j in sel_col_filtered$variable) {
  
  unit_col <- sel_col_filtered %>% filter(variable == j) %>% pull(conv_unit_col)

  # convert values
    # table join and conversion don't include a differentiation by material_id (for volumes and energy) for now
      # at the moment not necessary to include that, but at a later stage when conversion factors actually differ
  sheet <- sheet %>%
    left_join(.,
              conv_fac %>% select(-material_id), 
              by=setNames(nm=unit_col, "unit_id")
    ) %>%
    mutate(
      !!j := ifelse(
        unit_cat == "mass",
        !!parse_expr(j) * conv_factor_t,
        !!parse_expr(j) * conv_factor_ppm
      )
    )
  
  # check if there are volumes or energy units included
  vol_en_units <- conv_fac %>% filter(unit_cat %in% c("volume", "energy")) %>% distinct(unit_id) %>% pull()
  
  print("energy units included:")
  print(sheet %>% filter(eval(parse(text = j)) %in% vol_en_units))
  
  # check if there are entries which do not have either conversion factor
  print("missing conversion factor:")
  print(sheet %>% filter(!is.na(eval(parse(text = j))) & is.na(conv_factor_t) & is.na(conv_factor_ppm)))
  
  # deselect joined columns to enable join without problem in next loop
  sheet <- sheet %>% select(-unit_cat, -conv_factor_t, -conv_factor_ppm)
  
  }
  
 
 
  # second nested loop for each unit variable in the current sheet, replacing unit to either "t" or "ppm"
  for (k in sel_col_filtered %>% distinct(conv_unit_col) %>% pull()) {
    
    sheet <- sheet %>%
      left_join(.,
                conv_fac %>% select(unit_cat, unit_id), 
                by=setNames(nm = k, "unit_id")
      ) %>%
      mutate(
        !!k := ifelse(
          unit_cat == "mass",
          "t",
          "ppm"
        )
      ) %>% 
      select(-unit_cat)
    
    
  }
  
  detailed[[i]] <- sheet
  
}




## conversion by shares, based on production or reserves share

# select all relevant columns for conversion
sel_col <- sheets_columns %>% 
  filter(!is.na(conv_by_share)) %>%
  filter(sheet != "water_input")



# convert all relevant columns in loops again
for (i in sel_col %>% distinct(sheet) %>% pull()) {
  
  sheet <- detailed[[i]]
  
  # filter for only the relevant columns in this sheet
  sel_col_filtered <- sel_col %>% filter(sheet == i)
  
  # loop for each variable in sheet
  for (j in sel_col_filtered$variable) {
    
    share_col <- sel_col_filtered %>% filter(variable == j) %>% pull(conv_by_share)
    
    # convert values
    sheet <- sheet %>%
      mutate(!!j := eval(parse(text = j)) / eval(parse(text = share_col)))
    
  }
  
  # change share column to 1 after converting
  sheet <- sheet %>%
    mutate(!!share_col := ifelse(!is.na(eval(parse(text = share_col))), 1, NA))
  
  
  
  sheet <- detailed[[i]]
  
}



#### save converted tables
write_rds(detailed, "./detailed_data/03_intermediate/all_tables_converted.rds")
