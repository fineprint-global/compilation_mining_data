
##### Process detailed data from online research 
  
  ##### Harmonize
  ##### Convert
  ##### Check
  ##### Aggregate (sub_sites, type_mining)
  ##### Fill gaps
  ##### Create overview for coverage
  ##### Compile estimation factors for IRP database
  ##### Produce final data output
  ##### Additional scripts
  


# Check if the necessary packages are installed and if not install them, then load them
req_packages <- c("tidyverse", "readxl", "scales", "knitr", "kableExtra", "viridis", "rlang", "DT", "patchwork", "sf", "ggplot2", "RSQLite", "rnaturalearthdata")
req_packages <- req_packages[!req_packages %in% installed.packages()]
lapply(req_packages, install.packages)


# load packages (other packages are loaded in sub-scripts)
library(tidyverse)


#### Load all scripts ---------

getwd()

## Check columns 
  ## i.e. in Excel file before harmonization (i.e. number of columns, names of columns, order of columns)
source("./02_scripts/01_detailed_data/01a_harmonization_pre-check.R", print.eval = TRUE)


## Harmonization
 ## with results on non-fitting variables (html output is saved in `./04_output/01_detailed_data/01_harmonization/`)
wd <- getwd()
rmarkdown::render("./02_scripts/01_detailed_data/01b_harmonization.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./04_output/01_detailed_data/01_harmonization/",
                  intermediates_dir = "./04_output/01_detailed_data/01_harmonization/",
                  output_file = paste0("harmonization_", substr(Sys.time(), 1, 10),".html")
                  )



## Conversion
  ## of all absolute values to tonnes and all grades to ppm + conversion by production and reserves shares 
  ## (html output is saved in `./04_output/01_detailed_data/02_conversion/`)
wd <- getwd()
rmarkdown::render("./02_scripts/01_detailed_data/02_conversion.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./04_output/01_detailed_data/02_conversion/",
                  intermediates_dir = "./04_output/01_detailed_data/02_conversion/",
                  output_file = paste0("conversion_", substr(Sys.time(), 1, 10),".html")
)



## Intermediate data checks
wd <- getwd()
rmarkdown::render("./02_scripts/01_detailed_data/03_intermediate_check.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./04_output/01_detailed_data/03_intermediate_check/",
                  intermediates_dir = "./04_output/01_detailed_data/03_intermediate_check/",
                  output_file = paste0("intermediate_check_", substr(Sys.time(), 1, 10),".html")
)



## Aggregation
source("./02_scripts/01_detailed_data/04a_aggregation.R")



## Gap filling
source("./02_scripts/01_detailed_data/04b_gap_filling.R")


## Georeferencing of all mines
  ## (runs likely more than 1h on average computer)
  ## (10min if GADM data has not been downloaded yet + >60min for assigning GADM regions)
wd <- getwd()
rmarkdown::render("./02_scripts/01_detailed_data/07_georeferencing.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./04_output/01_detailed_data/07_other",
                  intermediates_dir = "./04_output/01_detailed_data/07_other",
                  output_file = "georeferencing.html"
)


## Add primary commodity to table general
source("./02_scripts/01_detailed_data/07a_add_primary_commodity.R")


## Produce final data output
wd <- getwd()
rmarkdown::render("./02_scripts/01_detailed_data/08_compile_final_data.Rmd",
                  knit_root_dir = wd, 
                  output_dir = "./04_output/01_detailed_data/08_final_data/markdown", 
                  intermediates_dir = "./04_output/01_detailed_data/08_final_data/markdown",
                  output_file = paste0("final_data_", substr(Sys.time(), 1, 10),".html")
)


## Calculate Coverages based on final data output
## this includes coverage of all commodities and spatial coverage (coordinates)
## html output is saved in `./04_output/01_detailed_data/05_coverage/01_general`)
wd <- getwd()
rmarkdown::render("./02_scripts/01_detailed_data/09_coverage_final_data.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./04_output/01_detailed_data/05_coverage/01_general",
                  intermediates_dir = "./04_output/01_detailed_data/05_coverage/01_general",
                  output_file = paste0("coverage_final_data_", substr(Sys.time(), 1, 10),".html")
)

# create pdf output with coverage of final data per country, material and year
wd <- getwd()
rmarkdown::render("./02_scripts/01_detailed_data/coverage_other/coverage_table.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./04_output/01_detailed_data/05_coverage/01_general",
                  intermediates_dir = "./04_output/01_detailed_data/05_coverage/01_general",
                  output_file = "coverage_table.pdf"
)


#### Additional scripts -----------

 
# ## Comparison of data coverage against national accounts with respective HTML output for COAL
# wd <- getwd()
# rmarkdown::render("./02_scripts/01_detailed_data/coverage_other/02_coal/coverage_coal_aggregated.Rmd",
#                   knit_root_dir = wd, 
#                   output_dir = "./04_output/01_detailed_data/05_coverage/02_coal/", 
#                   intermediates_dir = "./04_output/01_detailed_data/05_coverage/02_coal/",
#                   output_file = paste0("coverage_coal_", substr(Sys.time(), 1, 10),".html")
# )
# 
# ## Coverage for TOTAL MATERIAL
# wd <- getwd()
# rmarkdown::render("./02_scripts/01_detailed_data/coverage_other/04_total_material/coverage_total_material.Rmd",
#                   knit_root_dir = wd, 
#                   output_dir = "./04_output/01_detailed_data/05_coverage/04_total_material/", 
#                   intermediates_dir = "./04_output/01_detailed_data/05_coverage/04_total_material/",
#                   output_file = paste0("coverage_total_material", substr(Sys.time(), 1, 10),".html")
# )
# 
# 
# ## Comparison of data coverage against national accounts for ONLY ONE COUNTRY
# #Brazil
# wd <- getwd()
# rmarkdown::render("./02_scripts/01_detailed_data/coverage_other/06_by_country/coverage_brazil.Rmd",
#                   knit_root_dir = wd, 
#                   output_dir = "./04_output/01_detailed_data/05_coverage/", 
#                   intermediates_dir = "./04_output/01_detailed_data/05_coverage/",
#                   output_file = paste0("coverage_brazil", substr(Sys.time(), 1, 10),".html")
# )
# 
# 
# ## Plotting the spatial coverage
# source("./02_scripts/01_detailed_data/coverage_other/03_spatial/coverage_coordinates.R")
# 
# 
# ## producing coal tables only
# source("./02_scripts/01_detailed_data/other/get_coal_tables_only.R")
#
#
# Produce final data coverage (i.e. coverage calculated based on the final data set)


