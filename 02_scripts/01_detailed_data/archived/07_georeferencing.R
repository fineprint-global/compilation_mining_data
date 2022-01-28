# DESCRIPTION:
# assign points, multipoints or shapefiles to each mine in sheet_general
# mines which have no sub_sites, and have exact coordinates, will have the type point (exact) assigned
# mines which have sub_sites with coordinates will have the type multipoint (exact) assigned
# mines which have no coordinates and no sub_sites will have a shapefile of the finest possible granularity assigned
# regions will have a shapefile of the finest possible granularity assigned
# such a shapefile can be, for example, the shapefile of a district, a shapefile of several districts combined, or the shapefile of a state

library(readxl)
library(tidyverse)
library(sf)
library(sp)
library(RSQLite)

# clear R environment
rm(list = ls())



################################################################################
# read files and load data
################################################################################

# read harmonized data file (detailed data)
detailed <- read_rds("./03_intermediate/01_detailed_data/gaps_filled.rds")
general <- detailed$general
sub_sites <- detailed$sub_sites

# Import Geodata from GADM

source("./02_scripts/05_other/get_GADM_data.R")

file <- "./01_input/06_geodata/gadm36_levels.gpkg"
st_layers(file)
data <- src_sqlite(file)

# load function on which this script depends
source("./02_scripts/00_functions/split_columns.R")





################################################################################
# bind the sheets general and subsites together and create mine_id
################################################################################

#first, create a column "sub_site" which is empty for the sheet general
general <- add_column(general, sub_site = NA, .after = "mine_other_names")


#then, bind the sheet general and sheet_sub_sites below each other
general <- bind_rows(general, sub_sites)


# the only column that has a different naming in the two sheets but refers to the same is:
# mining_facility_types (in sheet_general) and mine_types (in sheet_sub_sites).
# therefore, they are put together into one column, named mining_facility_types
general$mining_facility_types <- coalesce(general$mining_facility_types, general$mine_types)
general <- select(general, -mine_types)


#create new column mine_id
general <- general %>% 
  arrange(mine_fac) %>% 
  group_by(mine_fac) %>% 
    mutate(mine_id = str_c("WUD", str_pad(dplyr::cur_group_id(), width = 5, pad = "0"))) %>%
  group_by(mine_id) %>% 
    mutate(mine_id = str_c(mine_id, ".", str_pad(row_number()-1, width = 2, pad = "0", "left"))) %>% 
  relocate(mine_id, .before = mine_fac)


# If there are NA values in the latitude or longitute, add a column to keep information
general$coord_is_na <- ifelse(is.na(general$longitude), TRUE, FALSE)
general_w_coords <- general %>% filter(., coord_is_na == FALSE)
general_wo_coords <- general %>% filter(., coord_is_na == TRUE)


# convert their coordinates into sf objects
general_w_coords.sf <- st_as_sf(x = general_w_coords, 
                               coords = c("longitude", "latitude"),
                               crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

general_wo_coords.sf <- st_as_sf(x = select(general_wo_coords, -latitude, -longitude), 
                                 geometry = st_as_sfc("POINT EMPTY"), 
                                 crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

general.sf <- rbind(general_w_coords.sf, general_wo_coords.sf)


# divide  mines and processing into mines_general and mines_sub_sites again
# the reason is that mines_general can be multipoints, while mines_sub_sites cannot
mines_general <- filter(general.sf, is.na(sub_site))
mines_sub_sites <- filter(general.sf, !is.na(sub_site))





################################################################################
# georeferencing of mines that have coordinates in sheet_general or sheet_sub_sites
################################################################################

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
general_output <- mines_general_wo_multipoints

# for those mines_general that have sub_sites with coordinates (mines_general_w_multipoints), create multipoints with all sub_sites
for(mine in mines_general_w_multipoints_list){
  
  #select a particular mine with all its sub_sites that have coordinates
  df <- filter(general.sf, mine_fac == mine & coord_is_na == FALSE)
  
  #combine all the points of a particular mine and its sub_sites to a multipoint
  multipoint <- df$geometry %>%
    st_difference() %>% #removes potential duplicate points
    st_combine() #combines the points to multipoint
  
  #replace the original point of the (general) mine with the multipoint
  mines_general_w_multipoints[mines_general_w_multipoints$mine_fac == mine & is.na(mines_general_w_multipoints$sub_site),]$geometry <- multipoint
  
  # set the column coord_is_na to FALSE, as now this mine has coordinates
  mines_general_w_multipoints[mines_general_w_multipoints$mine_fac == mine & is.na(mines_general_w_multipoints$sub_site),]$coord_is_na <- FALSE
}


# add mines with updated multipoints (mines_general_w_multipoints) to general_output
general_output <- bind_rows(general_output, mines_general_w_multipoints)





################################################################################
# georeferencing of mines and regions that do not 
# have coordinates in sheet_general or sheet_sub_sites
################################################################################

mwoc <- mines_general_wo_coords %>% 
  select(mine_fac, country, alphanumiso, state, region, province, district, sector, location_municipality) %>% 
  as.data.frame()

mwoc <- select(mwoc, -geometry)
mwoc %>% names()

# split the regions into separate rows according to logic of the split_columns function
first_split <- split_columns(mwoc, ";") #split by ";" 
second_split <- split_columns(first_split, ",") #split by "," 


#cleaning
second_split <- second_split %>% 
  mutate(across(where(is.character), str_trim)) %>%
  mutate_all(na_if,"") %>% #replace "" with NA values
  as_tibble()
  

#create dataframe of unique regions in the detailed data set
detailed_regions <- second_split %>% 
  mutate(., GID_0 = substr(alphanumiso, 1,3), .after = location_municipality) %>% 
  select(-1) %>% 
  distinct() %>% 
  arrange(country)




# add missing regions to the WU_GADM_concordance table file, which links regions to geometries from the GADM package
if (file.exists("./01_input/02_lists_and_concordance_tables/01_detailed_data/WU_GADM_concordance.csv")){
  
  WU_GADM_concordance <- read_delim("./01_input/02_lists_and_concordance_tables/01_detailed_data/WU_GADM_concordance.csv", delim = ";" ) 
  
  missing_rows <- anti_join(detailed_regions, WU_GADM_concordance, 
            by = c("country", "state", "region", "province", "district", "sector", "location_municipality"))
  
  if (nrow(missing_rows) > 0) {
    
    #add GID cols
    GID <- data.frame(matrix(ncol = 4, nrow = nrow(missing_rows)))
    colnames(GID) <- c("GID_1", "GID_2", "GID_3", "GID_4")
    
    missing_rows <- cbind(missing_rows, GID)
    WU_GADM_concordance <- rbind(WU_GADM_concordance, missing_rows) %>% arrange(., country, state, district, location_municipality)
    
    write_delim(WU_GADM_concordance, "./01_input/02_lists_and_concordance_tables/01_detailed_data/WU_GADM_concordance.csv", delim = ";")
    print(c((nrow(missing_rows)), "Rows added"))
    
    } else {print("No new regions added, file did not change")}

  } else {
    
  # create columns where the GID will be
  GID <- data.frame(matrix(ncol = 4, nrow = nrow(detailed_regions)))
  colnames(GID) <- c( "GID_1", "GID_2", "GID_3", "GID_4")
  
  #cbind them together
  WU_GADM_concordance <- cbind(detailed_regions, GID) %>% arrange(., country, state, district, location_municipality)

  write_delim(WU_GADM_concordance, "./01_input/02_lists_and_concordance_tables/01_detailed_data/WU_GADM_concordance.csv", delim = ";")
  print("New file created")
  
}


# join the polygons to mines via the concordance table
WU_GADM_concordance <- read_delim("./01_input/02_lists_and_concordance_tables/01_detailed_data/WU_GADM_concordance.csv", delim = ";" )


mines_wo_georeference <- left_join(second_split, WU_GADM_concordance, 
                                   by = c("country", "alphanumiso", "state", "region", "province", 
                                          "district", "sector", "location_municipality"))


#this should return TRUE
nrow(filter(mines_wo_georeference, is.na(GID_0))) == 0


# insert additonal row when level is level 4 to circumvent lvl + 1 throwing error in the loop below
mines_wo_georeference <- mines_wo_georeference %>% mutate(GID_5 = NA)


# construct output df 
mines_wo_georeference_output <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(mines_wo_georeference_output) <- c("mine_fac", "geom", "GADM_level")


# loop over levels and join the geometries from GADM to the mines without coordinates
for(lvl in 0:4){
  
  # load in correct level of the GADM dataset
  curr_GADM <- st_read(file, layer = paste0("level",lvl))
  
  # set current GID
  curr_GID <- paste0("GID_",as.character(lvl))
  
  # set next GID
  next_GID <- paste0("GID_",as.character(lvl + 1))
  
  # filter mines where a polygon of the current level should be assigned
  curr_mines <- mines_wo_georeference[as.vector(!is.na(mines_wo_georeference[ , 10 + lvl ])), ]
  curr_mines <- curr_mines[as.vector(is.na(curr_mines[ , 11 + lvl ])), ]
  
  # join
  curr_mines <- left_join(curr_mines, curr_GADM)

  # cleaning
  curr_mines <- curr_mines %>% select(., mine_fac, geom, GID_0, GID_1, GID_2, GID_3, GID_4)
  
  #adding the level of the geometry
  GADM_level <- rep(lvl, nrow(curr_mines))
  curr_mines <- cbind(curr_mines, GADM_level)

  # append to output dataframe
  mines_wo_georeference_output <- rbind(mines_wo_georeference_output, curr_mines)
  
  print(c("level ", lvl, " joined"))
  
}


# aggregate polygons per mine
mines_wo_georeference_union <- mines_wo_georeference_output %>% 
  group_by(mine_fac) %>%
  summarize(geometry = st_union(geom), 
            GADM_level = min(GADM_level),
            GID_0 = paste(unique(GID_0), collapse = " ; "),
            GID_1 = paste(unique(GID_1), collapse = " ; "),
            GID_2 = paste(unique(GID_2), collapse = " ; "),
            GID_3 = paste(unique(GID_3), collapse = " ; "),
            GID_4 = paste(unique(GID_4), collapse = " ; "))


# keep record of whether polygons were aggregated in the previous step (by st_union)
# and if so, how many polygons there were originally
nr_features <- mines_wo_georeference_output %>% count(mine_fac)
nr_features <- nr_features %>% rename(., n_features = n)
mines_wo_georeference_union <- mines_wo_georeference_union %>% left_join(., nr_features, by = "mine_fac")


# this has to return TRUE
nrow(mines_general_wo_coords) == nrow(mines_wo_georeference_union)


# bind general_output and mines_with_polygons together
mines_wo_georeference_final <- left_join(st_drop_geometry(mines_general_wo_coords), 
                                         mines_wo_georeference_union, by = "mine_fac") %>% st_as_sf()


#set coord_is_na to FALSE, as these mines just got georeferenced
mines_wo_georeference_final$coord_is_na <- FALSE


# add to general output
general_output <- bind_rows(general_output, mines_wo_georeference_final)



# cleaning and sorting
general_output <- general_output %>% 
  select(-sub_site, -sub_site_other_name, -coord_is_na) %>% 
  arrange(mine_id)


# CHECK: this has to return TRUE 
nrow(filter(general, is.na(sub_site))) == nrow(general_output)
nrow(general_output) + nrow(mines_sub_sites) == nrow(general)

################################################################################
# export final data
################################################################################

# export sheet_general as geopackage with geometries
st_write(general_output, "./03_intermediate/01_detailed_data/sheet_general/general.gpkg", append = FALSE)

# export sheet_sub_sites as geopackage with coordinates as POINT
st_write(mines_sub_sites %>% 
           relocate(sub_site_other_name, .after = sub_site) %>%
           select(-coord_is_na) %>%
           arrange(mine_id),
         "./03_intermediate/01_detailed_data/sheet_general/sub_sites.gpkg", append = FALSE)





# checking and visualization of data

#should return only POINT, MULTIPOINT, POLYGON, MULTIPOLYGON
# st_geometry_type(general_output$geometry) %>% unique()
# 
# 
# 
# st_geometry_type(general_output$geometry) %>% print(n=1500)
# 
# pl <- general_output %>% filter(st_geometry_type(geometry) == "POINT")
# plot(pl$geometry)
# 
# 
# a <- st_read("./03_intermediate/01_detailed_data/general_georeferenced.gpkg")
# head(a)
# st_geometry_type(a$geom) %>% unique()

