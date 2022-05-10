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

theme_set(theme_bw())
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

### read files
## harmonized data file
detailed <- read_rds("./03_intermediate/01_detailed_data/gaps_filled.rds")

# sheets from detailed data
general <- detailed$general
sub_sites <- detailed$sub_sites
coords_before <- sum(!is.na(general$longitude))

### create combined descriptive sheet from sheet_general and sheet_sub_sites
#first, create a column "sub_site" which is empty for the sheet general
general <- add_column(general, sub_site = NA, .after = "mine_fac")

#then, bind the sheet general and sheet_sub_sites "beneath" each other
general <- bind_rows(general, sub_sites)

# the only column that has a different naming in the two sheets but refers to the same is:
# mining_facility_types (in sheet_general) and mine_types (in sheet_sub_sites).
# therefore, they are put together into one column, named mining_facility_types
general$mining_facility_types <- coalesce(general$mining_facility_types, general$mine_types)
general <- select(general, -mine_types)
mines <- general[!is.na(general$longitude),]

#check how many coordinates got added by the addition of sub_sites
coords_after <- nrow(mines)
coords_after - coords_before


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


### create plots

#plot for the whole world with all commodities
pdf(file = "./04_output/01_detailed_data/05_coverage/03_spatial/coverage_coordinates.pdf",   
    width = 17, 
    height = 9)

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

dev.off()

# plot for the whole world with only coal 
pdf(file = "./04_output/01_detailed_data/05_coverage/03_spatial/coverage_coordinates_coal.pdf",   
    width = 17, 
    height = 9) 

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = coal_sf, size = 2, shape = 24, fill = "black") +
  coord_sf(expand = FALSE)

dev.off()


# plot for only one country with all commodities (enter coordinates for the resp. country)

# ggplot(data = world) +
#   geom_sf() +
#   geom_sf(data = coal_sf, size = 3, shape = 24, fill = "black") +
#   geom_sf(data = iron_sf, size = 3, shape = 24, fill = "blue") +
#   geom_sf(data = bauxite_sf, size = 3, shape = 24, fill = "red3") +
#   geom_sf(data = gold_sc, size = 3, shape = 24, fill = "gold") +
#   geom_sf(data = copper_sf, size = 3, shape = 24, fill = "darkorange") +
#   geom_sf(data = pgm_sf, size = 3, shape = 24, fill = "cyan") +
#   geom_sf(data = poly_sf, size = 3, shape = 24, fill = "darkgrey") +
#   geom_sf(data = other_sf, size = 3, shape = 24, fill = "white") +
#   geom_sf(data = processing_fac, size = 2, shape = 23, fill = "cornflowerblue") +
#   coord_sf(xlim = c(-129, -90), ylim = c(0, 50), expand = FALSE)

# known coordinates
# Indonesia: xlim = c(90, 145), ylim = c(-15, 5)



