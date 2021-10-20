
# ---------------------------
# Other main scripts
# ---------------------------




## Compilation of estimation factors
  ## With respective HTML output for checking of time series and outliers (html output is saved in `./detailed_data/04_output/est_fac/`)
wd <- getwd()
rmarkdown::render("./02_scripts/01_detailed_data/06_compile_estimation_factors.Rmd",
                  knit_root_dir = wd, 
                  output_dir = "./04_output/01_detailed_data/06_est_fac/", 
                  intermediates_dir = "./04_output/01_detailed_data/06_est_fac/",
                  output_file = paste0("overview_estimation_factors_", substr(Sys.time(), 1, 10),".html")
)




## Overview of coverage for total material and overburden
  ## (html output is saved in `./detailed_data/04_output/coverage/`)
wd <- getwd()
rmarkdown::render("./02_scripts/01_detailed_data/coverage_other/04_total_material/coverage_total_material.Rmd",
                  knit_root_dir = wd, 
                  output_dir = "./04_output/01_detailed_data/05_coverage/04_total_material/", 
                  intermediates_dir = "./04_output/01_detailed_data/05_coverage/04_total_material/",
                  output_file = paste0("coverage_total_material_", substr(Sys.time(), 1, 10),".html")
)




# Country-specific coverage overview
# wd <- getwd()
# rmarkdown::render("./02_scripts/01_detailed_data/coverage_other/06_by_country/coverage_brazil.Rmd",
#                   knit_root_dir = wd, 
#                   output_dir = "./04_output/01_detailed_data/coverage/06_by_country/", 
#                   intermediates_dir = "./04_output/01_detailed_data/coverage/06_by_country/",
#                   output_file = paste0("coverage_brazil_", substr(Sys.time(), 1, 10),".html")
#                   )







