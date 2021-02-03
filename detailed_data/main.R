##### Process detailed data from online research into harmonized format #####



# Check if the necessary packages are installed and if not install them, then load them
req_packages <- c("tidyverse", "readxl", "scales", "knitr", "kableExtra", "viridis", "rlang")
req_packages <- req_packages[!req_packages %in% installed.packages()]
lapply(req_packages, install.packages)

# load packages (other packages are loaded in sub-scripts)
library(tidyverse)



# Load scripts

## Check columns in Excel file before harmonization (i.e. number of columns, names of columns, order of columns)
source("./detailed_data/02_scripts/01_harmonization_pre-check.R", print.eval = TRUE)

## Harmonization with results on non-fitting variables (html output is saved in `./02_scripts/comparison/`)
wd <- getwd()
rmarkdown::render("./detailed_data/02_scripts/01_harmonization.Rmd",
                  knit_root_dir = wd,
                  output_dir = "./detailed_data/04_output/01_harmonization/",
                  intermediates_dir = "./detailed_data/04_output/01_harmonization/",
                  output_file = paste0("harmonization_", substr(Sys.time(), 1, 10),".html")
                  )



## Conversion of all absolute values to tonnes and all grades to ppm + conversion by production and reserves shares 
source("./detailed_data/02_scripts/02_conversion.R")



## Data checks nr. 1
source("./detailed_data/02_scripts/03_data_check.R")



## Gap filling
source("./detailed_data/02_scripts/04_gap_filling.R")



## Comparison of data coverage against national accounts with respective HTML output (html output is saved in `./detailed_data/04_output/coverage/`)
wd <- getwd()
rmarkdown::render("./detailed_data/02_scripts/05_coverage.Rmd",
                  knit_root_dir = wd, 
                  output_dir = "./detailed_data/04_output/coverage/", 
                  intermediates_dir = "./detailed_data/04_output/coverage/",
                  output_file = paste0("coverage_", substr(Sys.time(), 1, 10),".html")
                  )

# rmarkdown::render("./detailed_data/02_scripts/05_coverage_brazil.Rmd",
#                   knit_root_dir = wd, 
#                   output_dir = "./detailed_data/04_output/coverage/", 
#                   intermediates_dir = "./detailed_data/04_output/coverage/",
#                   output_file = paste0("coverage_brazil_", substr(Sys.time(), 1, 10),".html")
#                   )



## Compilation of estimation factors
  ## With respective HTML output for checking of time series and outliers (html output is saved in `./detailed_data/04_output/est_fac/`)
wd <- getwd()
rmarkdown::render("./detailed_data/02_scripts/06_compile_estimation_factors.Rmd",
                  knit_root_dir = wd, 
                  output_dir = "./detailed_data/04_output/est_fac/", 
                  intermediates_dir = "./detailed_data/04_output/est_fac/",
                  output_file = paste0("overview_estimation_factors_", substr(Sys.time(), 1, 10),".html")
)


