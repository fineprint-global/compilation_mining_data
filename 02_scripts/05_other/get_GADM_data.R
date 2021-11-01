library(xlsx)
library(sf)
library(sp)
library(RSQLite)

########################################################################################
# download global shapefiles for different sub-national levels from the GADM geopackage
########################################################################################

if (!file.exists("./01_input/06_geodata/gadm36_levels.gpkg")){
  
  if (!file.exists("./01_input/06_geodata/gadm36_levels_gpkg.zip")){
    
    timeout <- getOption('timeout')
    options(timeout=600) # 10 min
    download.file("https://biogeo.ucdavis.edu/data/gadm3.6/gadm36_levels_gpkg.zip", destfile = "./01_input/06_geodata/gadm36_levels_gpkg.zip")
    options(timeout=timeout)
    
  }
  unzip("./01_input/06_geodata/gadm36_levels_gpkg.zip", files = "gadm36_levels.gpkg", exdir = "./01_input/06_geodata")
}

########################################################################################
# create excel file that stores all GADM levels descriptions in one table
########################################################################################


if (!file.exists("./01_input/06_geodata/GADM_levels_combined.csv")){
  
  # read in the downloaded GADM file
  file <- "./01_input/06_geodata/gadm36_levels.gpkg"

  # Import GeoPackage
  data <- src_sqlite(file)
  
  # create Excel file that stores all levels of the GADM dataset, which will 
  # be used to link the regions to the shapefiles of the GADM dataset via a concordance table
  gadm_combined <- as.data.frame(tbl(data, paste0("level",5)))
  gadm_combined <- output[,colnames(output) != "geom"]
  
  for( l in seq(4,0,-1)){
    
    print(l)
    
    # Create a table for specific layer (0 = ISO region codes)
    df <- as.data.frame( tbl(data, paste0("level",l) ) )
    
    # Drop geo info cause otherwise not possible to write data frame to xlsx
    df <- df[,colnames(df) != "geom"]
    
    # bind it to the output df
    gadm_combined <- bind_rows(gadm_combined, df)
    
  }
  
  gadm_combined <- gadm_combined %>% 
    select(fid, NAME_0, GID_0, NAME_1, GID_1, ENGTYPE_1, NAME_2, GID_2, ENGTYPE_2, NAME_3, GID_3, ENGTYPE_3, NAME_4, GID_4, ENGTYPE_4) %>%
    distinct() %>%
    arrange(NAME_0, NAME_1, NAME_2, NAME_3, NAME_4)
  
  write.csv(gadm_combined, "./01_input/06_geodata/GADM_levels_combined.csv")

}
