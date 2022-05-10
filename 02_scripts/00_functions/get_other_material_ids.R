# This functions are used in the script coverage_coordinates.Rmd
# This function takes as input a vector of material_ids, and returns all OTHER material_ids of the material_ids_df with column material_id 

other_mat_ids <- function(material_ids_input, material_ids_df){
  other_material_ids <- material_ids_df %>% 
    filter(!(material_id %in% material_ids_input)) %>% 
    select(material_id) %>% 
    pull() %>%
    paste(., collapse = "|")
  
}


# get all material ids belonging to a certain categories
aggregate_mat_ids <- function(agg_category, agg_category_input, material_ids_df){
  aggregate_material_ids <- material_ids_df %>% 
    filter(UQ(sym(agg_category)) == agg_category_input) %>% 
    select(material_id) %>% 
    pull()
  
}

# get all material_id_agg belonging to a certain category
aggregate_mat_ids_agg <- function(agg_category, agg_category_input, material_ids_df){
  aggregate_material_ids <- material_ids_df %>% 
    filter(UQ(sym(agg_category)) == agg_category_input) %>% 
    select(material_id_agg) %>% 
    pull() %>% 
    unique() %>% 
    na.omit()
    
}

