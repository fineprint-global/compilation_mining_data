### Retrieval of EIA data using API v2
  
  # Via API of EIA (please note: key needed)
  # API documentation: https://www.eia.gov/opendata/commands.php



library(jsonlite)
library(tidyverse)


# clear R environment
rm(list = ls())


# set environment variables
# i.e. API key
readRenviron("./02_scripts/02_country_specific/eia/.Renviron")





### data retrieval ----

## define years for download in loop (because max. 5000 rows per request)

# year range 2000 until current year
year_min <- 2000

year_max <- substr(Sys.time(),1,4) %>%
  as.integer()


## loop

for (i in year_min:year_max) {

  # API request
    # splitted into respective parts (base url, api key, frequency, additional variables, sorting, year)
  json_list <- fromJSON(
    paste0(
      "https://api.eia.gov/v2/coal/mine-production/data/?",
      "api_key=", Sys.getenv("eia_api_key"),
      "&frequency=annual",
      "&data[]=latitude&data[]=longitude&data[]=operating-company&data[]=production",
      "&sort[0][column]=period&sort[0][direction]=desc",
      "&start=", i, "&end=", i
    ))
  
  # select data from request
  data <- json_list$response$data
  
  # turn into tibble
  data <- data %>%
    as.data.frame(., stringsAsFactors = FALSE) %>% 
    as_tibble()
  
  # union tibbles
  if(exists("all_data")) {
    all_data <- all_data %>% bind_rows(., data)
  } else {all_data <- data}
  
  }



# save rds
write_rds(all_data, file = paste0("./03_intermediate/02_country_specific/eia/eia_v2_data_", substr(Sys.time(),1,10),".rds"))



