# install.packages("sf")
# install.packages(c("rgeos","rgdal"))
# install.packages(c("rnaturalearth", "rnaturalearthdata"))
# install.packages("spData")
# install.packages("ggplot2")


library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)

#define some things for the libraries
theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)


### read files

## harmonized data file
detailed <- read_rds("./detailed_data/03_intermediate/gaps_filled.rds")

# sheets from detailed data
general <- detailed$general
sub_sites <- detailed$sub_sites


##############################################

# get coordinates of all mines and processing facilities
mines <- select(general, c(mine_fac, longitude, latitude, commodities_products, mine_or_processing))


# also integrate the case where the mine in sheet general does not have coordinates, but the sub_sites do
sub_sites <- select(sub_sites, mine_fac, sub_site, longitude, latitude)


colnames(sub_sites) <- c("mine_fac", "sub_site","longitude_sub", "latitude_sub")
sub_sites <- sub_sites[!is.na(sub_sites$longitude_sub),]
mines <- left_join(mines, sub_sites, by = "mine_fac")
sub_sites$longitude_sub
mines$longitude
mines$longitude_sub

#if sheet_gen does not have coordinates, fill up from sheet_sub_sites
mines %>% mutate(longitude = coalesce(longitude, longitude_sub))
mines %>% mutate(latitude = coalesce(latitude, latitude_sub))

mines %>%
  mutate(longitude = 
           longitude %>% 
           is.na %>%
           ifelse(longitude_sub, longitude))

### this does not work yet!!! ###########################


#delete possible double entered mines7sub_sites


# come up with the commodities the mines produce, based on the column commodities_products specified in sheet general
mines$commodities_distinct <- ifelse(grepl(c("coal|lignite"), mines$commodities_products, ignore.case = TRUE), "Coal",
                                     ifelse(grepl(c("iron"), mines$commodities_products, ignore.case = TRUE), "Iron",
                                     ifelse(grepl(c("bauxite"), mines$commodities_products, ignore.case = TRUE), "Bauxite",
                                     ifelse(grepl(c("Gold.*Silver|Gold.*Copper|Copper.*Silver|PGM.*Nickel|PGM.*Copper|Nickel.*Copper"), mines$commodities_products, ignore.case = TRUE), "Polymetallic",
                                     ifelse(grepl(c("gold"), mines$commodities_products, ignore.case = TRUE), "Gold",
                                     ifelse(grepl(c("Copper"), mines$commodities_products, ignore.case = TRUE), "Copper",
                                     ifelse(grepl(c("PGM|Platinum|Palladium"), mines$commodities_products, ignore.case = TRUE), "PGM",
                                     "Other")))))))


# determine if it is a mine or processing facility, based on the column mine_or_processing in sheet general
mines$mine_or_processing_distinct <-  ifelse(grepl("Mine", mines$mine_or_processing, ignore.case = TRUE), "Mine", "Processing")
processing_fac <- filter(mines, mine_or_processing_distinct == "Processing")
mines <- filter(mines, mine_or_processing_distinct == "Mine")

# plot the coordinates

#create sf objects out of the coordinates tibbles, for each category of commodity
coal_sf <- st_as_sf(filter(mines, commodities_distinct == "Coal"), coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
iron_sf <- st_as_sf(filter(mines, commodities_distinct == "Iron"), coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
bauxite_sf <- st_as_sf(filter(mines, commodities_distinct == "Bauxite"), coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
gold_sc <- st_as_sf(filter(mines, commodities_distinct == "Gold"), coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
copper_sf <- st_as_sf(filter(mines, commodities_distinct == "Copper"), coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
pgm_sf <- st_as_sf(filter(mines, commodities_distinct == "PGM"), coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
poly_sf <- st_as_sf(filter(mines, commodities_distinct == "Polymetallic"), coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
other_sf <- st_as_sf(filter(mines, commodities_distinct == "Other"), coords = c("longitude", "latitude"), crs = 4326, agr = "constant")
processing_fac <- st_as_sf(processing_fac, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")



ggplot(data = world) +
  geom_sf() +
  geom_sf(data = coal_sf, size = 3, shape = 24, fill = "black") +
  geom_sf(data = iron_sf, size = 3, shape = 24, fill = "blue") +
  geom_sf(data = bauxite_sf, size = 3, shape = 24, fill = "red3") +
  geom_sf(data = gold_sc, size = 3, shape = 24, fill = "gold") +
  geom_sf(data = copper_sf, size = 3, shape = 24, fill = "darkorange") +
  geom_sf(data = pgm_sf, size = 3, shape = 24, fill = "cyan") +
  geom_sf(data = poly_sf, size = 3, shape = 24, fill = "darkgrey") +
  geom_sf(data = other_sf, size = 3, shape = 24, fill = "white") +
  geom_sf(data = processing_fac, size = 2, shape = 23, fill = "cornflowerblue") +
  coord_sf(expand = FALSE)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = coal_sf, size = 3, shape = 24, fill = "black") +   coord_sf(expand = FALSE)
