### Download data from server



library(tidyverse)
library(httr)
library(DBI)
library(sf)
library(fineprintutils)


# clear R environment
rm(list = ls())

# set environment variables
readRenviron("./snl/.Renviron")




## load SNL data from Geoserver
# i.e. mines and general info
fineprint_geoserver <- "http://fineprint.wu.ac.at:8080/geoserver/"

snl_wfs_request <- "snl-2018/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=snl-2018:snl_metals&outputFormat=application/json"

snl_mines_wfs <- stringr::str_glue(fineprint_geoserver, snl_wfs_request)

snl_mines <- httr::GET(url = snl_mines_wfs, httr::authenticate(Sys.getenv("GS_USER"), Sys.getenv("GS_PASS"))) %>%
  sf::st_read(quiet = TRUE) %>%
  sf::st_transform(crs = sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


# geometry to column to make object smaller
snl_mines <- dplyr::bind_cols(snl_mines, data.frame(st_coordinates(snl_mines))) %>%
  sf::st_set_geometry(NULL)






## download data from CKAN

# Connect to CKAN using API key.
ck_setup()


# download production data (ore production and commodity production)
fineprintutils::ck_get_datasets() %>%
  dplyr::filter(
    name.organization == "snl", 
    name == "snl_l1_v1"
  ) %>%
  fineprintutils::ck_download(destpath = "./snl/01_input/01_data/production/")


# download ore grades
fineprintutils::ck_get_datasets() %>%
  dplyr::filter(
    name.organization == "snl", 
    name == "snl_l1_v1_millhead_grade"
  ) %>%
  fineprintutils::ck_download(destpath = "./snl/01_input/01_data/ore_grades/")


# download recovery rates
fineprintutils::ck_get_datasets() %>%
  dplyr::filter(
    name.organization == "snl", 
    name == "snl_l1_v1_recovery_rate"
  ) %>%
  fineprintutils::ck_download(destpath = "./snl/01_input/01_data/recovery_rates/")


