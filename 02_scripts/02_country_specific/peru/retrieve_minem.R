###### Retrieval of MINEM data
  # Download of Excel files via scraping html code



# Load packages
library(tidyverse)
library(rvest)
  # if rvest doesn't work, try with XML and httr

# clear R environment
rm(list = ls())



### files

file_list <- read_delim(
  "C:/Users/mlieber/Other_docs/Github/compilation_mining_data/01_input/03_log_files/02_country_specific/peru/file_list_2022-04-16.csv",
  delim = ";"
  )

# temporary list for file_list
file_list_former <- file_list



### data retrieval ----

# url 2011-2020
url_1 <- "http://www.minem.gob.pe/_estadistica.php?idSector=1&idEstadistica=12501"

# url 2001-2010
url_2 <- "http://www.minem.gob.pe/_estadistica.php?idSector=1&idEstadistica=6908"

urls <- c(url_1, url_2)



## loop for each url
for (j in urls) {
  
  
  ## get links for all files
  
  # check encoding, in case of error
  # and adjust, if necessary
  html_encoding_guess(j)
  
  # read page content
  html <- read_html(j, encoding="ISO-8859-1")
  
  # get all excel links from webpage
  links <- c(
    html_nodes(html, xpath=".//a[contains(@href, '.xls')]") %>% 
      html_attr("href"),
    html_nodes(html, xpath=".//a[contains(@href, '.XLS')]") %>% 
      html_attr("href")
  )
  
  

  ## download all files
  for (i in links) {
    
    # adjust link
    if (grepl("http://www.minem.gob.pe", i)) {
      link <- i
    } else {
      link <- i %>%
        sprintf("http://www.minem.gob.pe%s", .)
      }
    
    
    # isolate year
    if (grepl("http://www.minem.gob.pe", i)) {
      year <- i %>%
        substr(., 76, 79)
    } else {
      year <- i %>%
        substr(., 53, 56)
    }
   
    
    # file name
      # based on last "/" in link
    file_name <- i %>%
      substr(., tail(unlist(gregexpr('/', i)), n=1) + 1, nchar(i))
    
    file_name <- paste0(year, "_", file_name)
    
    # file destination
    dest <- paste0(
      "./03_intermediate/02_country_specific/peru/raw/",
      file_name
    )
    
    
    # if file does not exist in file_list yet
    if(!(file_name %in% file_list$file)) {
      
      # download and save file
      # method "curl" is used, because otherwise issues with special characters
      download.file(
        link,
        destfile = dest,
        method = "curl",
        mode = "wb"
      )
      
      # append list of all downloaded files
      file_list <- file_list %>%
        union(., tibble(file = file_name))
      
    }
    
  }
  
}





## save file list
file_list <- file_list %>%
  arrange(file)

write_delim(
  file_list,
  paste0(
    "./01_input/03_log_files/02_country_specific/peru/file_list_",
    substr(Sys.time(),1,10),
    ".csv"
    ),
  delim = ";"
  )


## newly download files
newly_downloaded <- setdiff(
  file_list,
  file_list_former
  )

write_delim(
  newly_downloaded,
  paste0(
    "./01_input/03_log_files/02_country_specific/peru/newly_downloaded_files_",
    substr(Sys.time(),1,10),
    ".csv"
  ),
  delim = ";"
)


