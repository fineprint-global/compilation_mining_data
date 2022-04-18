### Retrieval of EIA data
  
  # Via API of EIA (please note: key needed)
  # API documentation: https://www.eia.gov/opendata/commands.php


  # Please note: Data has to be retrieved for >3900 mines/facilities and their respective mining types
  # Therefore retrieval takes >30min



library(jsonlite)
library(httr)
library(tidyverse)


# clear R environment
rm(list = ls())


# set environment variables
  # i.e. API key
readRenviron("./02_scripts/02_country_specific/eia/.Renviron")





### data retrieval ----


# IDs for the materials which are to be retrieved from EIA
cat_id <- "717256"  # Coal - Mine level data



# remove EIA tibble if in R environment
if(exists("eia_coal_general")) rm(eia_coal_general)
if(exists("eia_coal_values")) rm(eia_coal_values)


# retrieve child categories for given category from API
  # i.e. in this case for all availables states
url <- paste0("https://api.eia.gov/category/?api_key=", Sys.getenv("eia_api_key"), "&category_id=", cat_id)

category_ids_1 <- fromJSON(url)

category_ids_1 <- category_ids_1$category$childcategories


#i <- "911428"

# retrieve child categories for each state category
  # i.e. in this case for all available mines
for (i in category_ids_1$category_id) {
  
  url <- paste0("https://api.eia.gov/category/?api_key=eed697fbab825de21d1f1c08c56e63cf&category_id=", i)
  
  category_ids_2 <- fromJSON(url)
  
  category_ids_2 <- category_ids_2$category$childcategories
  
  
  #j <- "769799"
  
  # retrieve data for the respective cat_id
    # i.e. data for each mine
  for (j in category_ids_2$category_id) {
    
    json_list <- fromJSON(paste0("https://api.eia.gov/category/?api_key=eed697fbab825de21d1f1c08c56e63cf&category_id=", j))
    
    series_ids <- json_list$category$childseries$series_id %>%
      grep("PRODUCTION", ., value = TRUE)
    
    
    # retrieve, combine and include data (only if production data is available)
    if(is_empty(series_ids)) {next}
    
    #if(series_ids %in% (eia_coal_1_series_ids %>% select(series_id) %>% pull())) {next}
    
      #k <- "COAL.MINE.PRODUCTION.3303792-BIT-SUR.A"
      
      # do that for each production-related series_id of a mine (i.e. if a mine has different mining types)
      for(k in series_ids) {
        
        
        # columns with general info (series_id, mine name incl. coal type and mining type, reporting frequency, unit)
        json_list_2 <- fromJSON(paste0("https://api.eia.gov/series/?api_key=eed697fbab825de21d1f1c08c56e63cf&series_id=", k))
          
        general <- json_list_2[["series"]][grepl("series_id|name|units|f|copyright|source|latlon|geography", 
                                                 names(json_list_2[["series"]]))] %>%
          as_tibble()
        
        
        # separate name column into several (because containing several infos)
        general <- general %>%
          separate(
            col = name,
            into = c("type_production", "mine_fac", "type_coal", "type_mining", "frequency"),
            sep = ":",
            fill = "right"
          )
        
        
        # columns with data
          # including the series_id (so it can be matched with general info)
        values <- json_list_2$series$data %>% 
          as.data.frame(., stringsAsFactors = FALSE) %>% 
          as_tibble() %>%
          rename(year = X1, value = X2) %>%
          add_column(., series_id = !!k, .before = "year")
        

        # union both retrieved tibbles from loop into a combined one for each (general and values)
        if(exists("eia_coal_general")) {
          eia_coal_general <- eia_coal_general %>% bind_rows(., general)
        } else {eia_coal_general <- general}
        
        if(exists("eia_coal_values")) {
          eia_coal_values <- eia_coal_values %>% bind_rows(., values)
        } else {eia_coal_values <- values}
        
        print(Sys.time())
        print(k)
    
      }
    }
  }




# save rds
write_rds(eia_coal_general, file = paste0("./03_intermediate/02_country-specific/eia/eia_general_raw_", substr(Sys.time(),1,10),".rds"))

write_rds(eia_coal_values, file = paste0("./03_intermediate/02_country-specific/eia/eia_values_raw_", substr(Sys.time(),1,10),".rds"))






