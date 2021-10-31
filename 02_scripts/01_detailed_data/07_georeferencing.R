# DESCRIPTION:
# assign points, multipoints or shapefiles to each mine in sheet_general
# mines which have no sub_sites, and have exact coordinates, will have the type point (exact) assigned
# mines which have sub_sites with coordinates will have the type multipoint (exact) assigned
# mines which have no coordinates and no sub_sites will have a shapefile of the finest possible granularity assigned
# regions will have a shapefile of the finest possible granularity assigned
# such a shapefile can be, for example, the shapefile of a district, a shapefile of several districts combined, or the shapefile of a state

library(xlsx)
library(stringr)
library(tidyverse)
library(sf)
library(sp)



# read files and load data -----------------------------------------------------

# harmonized data file (detailed data)
detailed <- read_rds("./03_intermediate/01_detailed_data/gaps_filled.rds")
general <- detailed$general
sub_sites <- detailed$sub_sites

# Geodata from GADM

# load function on which this script depends
source("./02_scripts/00_functions/split_columns.R")



# bind the sheets general and subsites together --------------------------------

#first, create a column "sub_site" which is empty for the sheet general
general <- add_column(general, sub_site = NA, .after = "mine_fac")

#then, bind the sheet general and sheet_sub_sites "beneath" each other
general <- bind_rows(general, sub_sites)

# the only column that has a different naming in the two sheets but refers to the same is:
# mining_facility_types (in sheet_general) and mine_types (in sheet_sub_sites).
# therefore, they are put together into one column, named mining_facility_types
general$mining_facility_types <- coalesce(general$mining_facility_types, general$mine_types)
general <- select(general, -mine_types)

# now as described above, the column mine_fac and sub_site together act as the primary key.
# to simplify, a new column mine_id is created, which simply is the two strings bound together. 
# this leads to the (aggregated) mine always having its original mine_fac string as mine_id, while sub_sites have 
# their mine_id as a composite of mine_fac and sub_site. The columns mine_fac and sub_site will be kept in order to easily
# identify which parent mine_fac a sub_site belongs to.
general$mine_id <- paste(general$mine_fac, general$sub_site, sep = " ")
general <- relocate(general, mine_id, .before = "mine_fac")
general$mine_id <- gsub(pattern = " NA", replacement = "", general$mine_id) #cleaning

# If there are NA values in the latitude or longitute, replace them with 0 and add a column to keep information
general$coord_is_na <- ifelse(is.na(general$longitude), TRUE, FALSE)
index <- general$coord_is_na == TRUE
general[index, "longitude"] <- 0
general[index, "latitude"] <- 0

# convert their coordinates into sf objects
general.sf <- st_as_sf(x = general, 
                               coords = c("longitude", "latitude"),
                               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")



### georeferencing of mines and processing--------------------------------------

# further divide mines and processing into mines_general and mines_sub_sites
# the reason being that mines_general can be multipoints, while mines_sub_sites cannot
mines_general <- filter(general.sf, is.na(sub_site))
mines_sub_sites <- filter(general.sf, !is.na(sub_site))

## mines_sub_sites ----------
# create the output dataframe and add mines_sub_sites (Think about this again if I want to add mines_sub_sites to GENERAL output)
general_output <- mines_sub_sites

## mines_general ----------
# mines that do have sub_sites with coordinates
mines_general_w_multipoints_list <- filter(mines_sub_sites, coord_is_na == FALSE)$mine_fac %>% unique()
mines_general_w_multipoints <- mines_general %>%
  filter(mine_fac %in% mines_general_w_multipoints_list)

# mines that do not have sub_sites with coordinates, but have coordinates in sheet general
mines_general_wo_multipoints <- mines_general %>%
  filter(!(mine_fac %in% mines_general_w_multipoints_list) & coord_is_na == FALSE)

# mines that do not have sub_sites with coordinate and do not have coordinates in sheet general
mines_general_wo_coords <- mines_general %>%
  filter(!(mine_fac %in% mines_general_w_multipoints_list) & coord_is_na == TRUE)

# this has to return TRUE
nrow(mines_general_wo_multipoints) + nrow(mines_general_w_multipoints) +
  nrow(mines_general_wo_coords) == nrow(mines_general)

# mines that do not have sub_sites with coordinates can just be added to general_output
general_output <- rbind(general_output, mines_general_wo_multipoints)

# for those mines_general that have sub_sites with coordinates (mines_general_w_multipoints), create multipoints with all sub_sites
for(mine in mines_general_w_multipoints_list){
  
  #select a particular mine with all its sub_sites that have coordinates
  df <- filter(mines_processing, mine_fac == mine & coord_is_na == F)
  
  #combine all the points of a particular mine and its sub_sites to a multipoint
  multipoint <- df$geometry %>%
    st_difference() %>% #removes potential duplicate points
    st_combine() #combines the points to multipoint
  
  #replace the original point of the (general) mine with the multipoint
  mines_general_w_multipoints[mines_general_w_multipoints$mine_id == mine,]$geometry <- multipoint
  
}

# add mines with updated multipoints (mines_general_w_multipoints) to general_output
general_output <- rbind(general_output, mines_general_w_multipoints)



### georeferencing of regions and mines that don't have coordinates ------------
mwoc <- as.data.frame(select(mines_general_wo_coords, c(2,5:12)))
mwoc <- select(mwoc, -geometry)


first_split <- split_columns(mwoc, ";") #split by ";" 

second_split <- split_columns(first_split, ",") #split by "," 

#create dataframe of unique regions in the detailed data set
detailed_regions <- second_split %>% 
  mutate(across(where(is.character), str_trim)) %>%
  mutate(., GID_0 = substr(alphanumiso, 1,3), .after = location_municipality) %>% 
  select(-1) %>% 
  distinct() %>% 
  arrange(country)


# add missing regions to the WU_GADM_concordance table file, which links regions to geometries from the GADM package
if (file.exists("./01_input/02_lists_and_concordance_tables/01_detailed_data/WU_GADM_concordance.xlsx")){
  
  regions_geometries_concordance <- read.xlsx2("./01_input/02_lists_and_concordance_tables/01_detailed_data/WU_GADM_concordance.xlsx", 1) %>% as.data.frame()
  
  missing_rows <- anti_join(detailed_regions, regions_geometries_concordance, 
            by = c("country", "state", "region", "province", "district", "sector", "location_municipality"))
  
  if (nrow(missing_rows) > 0) {
    
    #add GID cols
    GID <- data.frame(matrix(ncol = 4, nrow = nrow(missing_rows)))
    colnames(GID) <- c("GID_1", "GID_2", "GID_3", "GID_4")
    missing_rows <- cbind(missing_rows, GID)
    regions_geometries_concordance <- rbind(regions_geometries_concordance, missing_rows) %>% arrange(., country, state, district, location_municipality)
    
    write.xlsx2(regions_geometries_concordance, "./01_input/02_lists_and_concordance_tables/01_detailed_data/WU_GADM_concordance.xlsx", row.names = FALSE, showNA = FALSE)
    print(c((nrow(missing_rows)), " Rows added"))
    
    } else {print("No new regions added")}

  } else {
    
  # create columns where the GID will be
  GID <- data.frame(matrix(ncol = 4, nrow = nrow(detailed_regions)))
  colnames(GID) <- c( "GID_1", "GID_2", "GID_3", "GID_4")
  
  #cbind them together
  regions_geometries_concordance <- cbind(detailed_regions, GID) %>% arrange(., country, state, district, location_municipality)

  write.xlsx2(regions_geometries_concordance, "./01_input/02_lists_and_concordance_tables/01_detailed_data/WU_GADM_concordance.xlsx", row.names = FALSE, showNA = FALSE)
  print("New file created")
}

# join the polygons to mines and regions without coordinates via the concordance table


# bind general_output and mines_with_polygons together


# this has to return TRUE 
nrow(general) == nrow(general_output)

# write to intermediate folder a copy of the sheet general with the geometries included
