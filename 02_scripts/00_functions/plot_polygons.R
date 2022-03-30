library(mapview)

#function for plotting the mines not intersecting with GADM regions
plot_GADM_polygons <- function(df_row, buffer_input) {
  
  print(df_row)
  
  # read in the GADM level
  gadm <- st_read(file, layer = paste0("level", df_row$max_gadm_level), quiet = TRUE)
  
  # filter only for country for efficiency
  country <- gadm %>% 
    filter(GID_0 == df_row$GID_0)
  
  # create buffer around the mine_polygon
  buffer <- st_buffer(df_row$geometry, buffer_input)
  intersection_indices <- st_intersects(country, buffer, sparse = F)
  nearest <- st_nearest_feature(df_row, country)
  ROI <- country[intersection_indices,]
  
  # plot with mapview
  mapview(ROI) + mapview(df_row, burst = TRUE)
  
  # plot with ggplot (uncomment if desired)
  # ggplot() +
  # geom_sf(data = ROI) +
  # geom_sf(data = mine_polygon) +
  # geom_sf(data = country[nearest,], color = "red") +
  # coord_sf()
  
}
