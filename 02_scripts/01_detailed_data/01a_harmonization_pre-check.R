library(tidyverse)
library(readxl)

# clear R environment
rm(list = ls())


## read input tables

# list of supposed column order
sheets_columns <- read_delim("./01_input/02_lists_and_concordance_tables/01_detailed_data/sheets_columns.csv", delim = ";")

# path to Excel data file
path <- "./01_input/01_data/01_detailed_data/detailed_data_mining.xlsx"



### Check of consistency between columns in Excel file and supposed order of columns (i.e. sheets_columns.csv)
  ### Check for number of columns
  ### Check of new columns
  ### Check of order of columns
  ### In case of inconsistency, please adjust sheets_columns.csv or Excel file

# Note: Harmonization is based on structure of file sheets_columns.csv, in terms of column types, allocation of IDs, checks to be undertaken
  # Types have to be assigned, because the guessing of types when reading the file does not work properly (and single assignment of types by column name is not provided by readxl package)
    # If the number and order of columns is not the same, the wrong types will be provided,
    # resulting in a large magnitude of errors

detailed <- path %>%
  excel_sheets() %>%
  set_names() %>% 
  map(~ read_excel(path = path, sheet = .x, na = c("", "-", "NA")))

options(nwarnings = 200)
print(paste("Number of warnings: ", length(warnings())))

if(exists("column_check")) rm(column_check)

column_check <- tibble(sheet = NA, variable = NA) %>% filter(!is.na(sheet))

for (i in names(detailed)) {
  
  a <- tibble(
    sheet = i,
    variable = names(detailed[[i]])
  )
  
  column_check <- column_check %>%
    rbind(., a)
  
}



# table with number of rows in Excel and column list
print("Number of rows in Excel and column list: ")

tibble(
  variable_number_Excel = nrow(column_check),
  variable_number_list = nrow(sheets_columns)
  )



# Show if there are new columns
  # If yes, either adjust Excel file or sheets_columns.csv accordingly
print("New columns: ")

tibble(
  setdiff(
    column_check,
    sheets_columns %>% distinct(sheet, variable))
  )



# Show columns which are in wrong order (if there are any)
print("Columns in wrong order: ")

tibble(
  bind_cols(
    column_check %>% rename(data_sheet = sheet, data_variable = variable), 
    sheets_columns%>% rename(list_sheet = sheet, list_variable = variable)
  )) %>% 
    filter(data_sheet != list_sheet | data_variable != list_variable)


