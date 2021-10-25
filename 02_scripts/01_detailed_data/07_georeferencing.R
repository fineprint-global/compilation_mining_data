# assign points, multipoints or shapefiles to each mine in sheet_general
# mines which have no sub_sites, and have exact coordinates, will have the type point (exact) assigned
# mines which have sub_sites with coordinates will have the type multipoint (exact) assigned
# mines which have no coordinates and no sub_sites will have a shapefile of the finest possible granularity assigned
# regions will have a shapefile of the finest possible granularity assigned
# such a shapefile can be, for example, the shapefile of a district, a shapefile of several districts combined, or the shapefile of a state

library(xlsx)
library(tidyverse)
library(sf)
library(sp)

# read files and load data

# download global shapefiles for different sub-national levels from the GADM geopackage

if (!file.exists("./01_input/01_data/01_detailed_data/GADM/gadm36_levels.gpkg")){
  if (!file.exists("./01_input/01_data/01_detailed_data/GADM/gadm36_levels_gpkg.zip")){
    timeout <- getOption('timeout')
    options(timeout=600) # 10 min
    download.file("https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_levels_gpkg.zip", destfile = "./01_input/01_data/01_detailed_data/GADM/gadm36_levels_gpkg.zip")
    options(timeout=timeout)
  }
  unzip("./01_input/01_data/01_detailed_data/GADM/gadm36_levels_gpkg.zip", files = "gadm36_levels.gpkg", exdir = "./01_input/01_data/01_detailed_data/GADM")
}

# read in the downloaded GADM file
file <- "./01_input/01_data/01_detailed_data/GADM/gadm36_levels.gpkg/gadm36_levels.gpkg"

# Explore layers
sf::st_layers(file)

# harmonized data file (detailed data)
detailed <- read_rds("./03_intermediate/01_detailed_data/gaps_filled.rds")
general <- detailed$general
sub_sites <- detailed$sub_sites

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

# filter out regions
regions <- filter(general.sf, mine_or_processing == "Region")

# filter out mines and processing (everything except regions and companies)
mines_processing <- filter(general.sf, mine_or_processing != "Region|Company")

### georeferencing of mines and processing--------------------------------------

# further divide mines and processing into mines_general and mines_sub_sites
# the reason being that mines_general can be multipoints, while mines_sub_sites cannot
mines_general <- filter(mines_processing, is.na(sub_site))
mines_sub_sites <- filter(mines_processing, !is.na(sub_site))

## mines_sub_sites ----------
# create the output data.frame and add mines_sub_sites
general_output <- mines_sub_sites

## mines_general ----------
# mines that do have sub_sites with coordinates
mines_general_w_multipoints_list <- filter(mines_sub_sites, coord_is_na == F)$mine_fac %>% unique()
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

# this has to return TRUE
nrow(mines_processing) == nrow(general_output) + nrow(mines_general_wo_coords)


### georeferencing of regions and mines that don't have coordinates ------------

# create output list of mines that have production (in sheet_min or sheet_com) and are not georeferenced
write.xlsx(data.frame(regions_mines), file="./04_output/01_detailed_data/07_other/mines_regions_wo_coordinates", row.names=FALSE, showNA=FALSE)

# create a column with strings that specify in which polygons the mine or region is
# each of the strings should match one polygon
regions_mines <- rbind(regions, mines_general_wo_coords) %>%
  select(c("mine_fac", "country", "state", "region", "province", "district", "sector", "location_municipality"))




# this has to return TRUE
nrow(general) == nrow(general_output)
