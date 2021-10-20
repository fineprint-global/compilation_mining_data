
##### Process detailed data from online research 
  
  ##### Harmonize
  ##### Convert
  ##### Check
  ##### Aggregate (sub_sites, type_mining)
  ##### Fill gaps
  ##### Create overview for coverage
  ##### Compile estimation factors for IRP database
  



# Check if the necessary packages are installed and if not install them, then load them
req_packages <- c("tidyverse", "readxl", "scales", "knitr", "kableExtra", "viridis", "rlang", "DT")
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



## Comparison of data coverage against national accounts with respective HTML output 
  ## (html output is saved in `./04_output/01_detailed_data/coverage/`)
wd <- getwd()
rmarkdown::render("./02_scripts/01_detailed_data/05_coverage.Rmd",
                  knit_root_dir = wd, 
                  output_dir = "./04_output/01_detailed_data/05_coverage/01_general/", 
                  intermediates_dir = "./04_output/01_detailed_data/05_coverage/01_general/",
                  output_file = paste0("coverage_", substr(Sys.time(), 1, 10),".html")
                  )



## Comparison of data coverage against national accounts with respective HTML output for COAL
## (html output is saved in `./04_output/01_detailed_data/coverage/coverage_coal`)
wd <- getwd()
rmarkdown::render("./02_scripts/01_detailed_data/coverage_other/02_coal/coverage_coal_aggregated.Rmd",
                  knit_root_dir = wd, 
                  output_dir = "./04_output/01_detailed_data/05_coverage/02_coal/", 
                  intermediates_dir = "./04_output/01_detailed_data/05_coverage/02_coal/",
                  output_file = paste0("coverage_coal_", substr(Sys.time(), 1, 10),".html")
)


