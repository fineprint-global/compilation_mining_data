###### Format, clean and combine files



# Load packages
library(tidyverse)
library(readxl)

# clear R environment
rm(list = ls())



### files

# data
files <- list.files(
  path = "./03_intermediate/02_country_specific/peru/raw",
  #pattern = ,
  )

# lists
com_list <- read_delim(
  "./01_input/03_log_files/02_country_specific/peru/commodity_list.csv",
  delim = ";",
  locale = locale(encoding = "ISO-8859-1")
    )



# filter only for commodity files (i.e. no "concentrados" with info on grades, and no "metales" with aggregated info)

filter <- paste0(com_list$minem_file_name, collapse = "|")

files_f <- grep(filter, files, value = TRUE)





### loops for all files



## filter for single-commodity files and 2006-2019

files_f_2 <- grep("CARBON|NO-METALICOS", files_f, value = TRUE, invert = TRUE)

filter_y <- paste0(2006:2019, collapse = "|")

files_f_2 <- grep(filter_y, files_f_2, value = TRUE)

# include "2005_ORO.xls" (because same format)
files_f_2 <- c(files_f_2, "2005_ORO.xls")



## check first commodity name for each file ----
 # i.e. if it is in the same row for each single commodity file (i.e. not "CARBON" and "NO-METALICOS")
 # for 2006 to 2019

for (i in files_f_2) {
  
  data <- read_excel(
    path = paste0("./03_intermediate/02_country_specific/peru/raw/", i),
    skip = 1
  )
  
  ## get commodity name
  data <- data %>%
    mutate(LEY = gsub("%", "", LEY))
  
  com_name <- data[3, ]$LEY
  
  overview <- tibble(commodity = com_name, file = i)
  
  if(exists("overview_all")) {
    overview_all <- overview_all %>% bind_rows(., overview)
  } else {overview_all <- overview}
  
}

overview_all %>%
  filter(is.na(commodity))



## single-commodity files from 2006 to 2019 ----
  ## (because different format before 2006)

if(exists("data_all")) rm(data_all)

for (i in files_f_2) {
  
  data <- read_excel(
    path = paste0("./03_intermediate/02_country_specific/peru/raw/", i),
    skip = 1
  )
  
  # clean commodity name
  data <- data %>%
    mutate(LEY = gsub("%", "", LEY))
  
  
  # filter for only data rows (i.e. no footnotes)
  data <- data %>%
    filter(!is.na(UNIDAD))
  
  
  # filter for relevant columns
  data <- data %>%
    select("LEY", "ETAPA", "TITULAR", "UNIDAD", "REGION", "PROVINCIA", "DISTRITO", "TOTAL GENERAL")
  
  
  # get year and insert year
  year <- i %>% substr(., 1, 4) %>% as.integer
  
  data <- data %>%
    mutate(year = !!year)
  
  
  # get unit
  unit <- read_excel(
    path = paste0("./03_intermediate/02_country_specific/peru/raw/", i)
  )
  
  unit <- names(unit)[1]
  
  unit <- substr(unit, unlist(gregexpr('\\(', unit)) + 1, unlist(gregexpr('\\)', unit)) - 1)
  
  # insert unit
  data <- data %>%
    mutate(unit = !!unit)
  
  
  # combine data
  if(exists("data_all")) {
    data_all <- data_all %>% bind_rows(., data)
  } else {data_all <- data}
  
  }






## coal and minerals ----
  ## from 2004 to 2019, because only starting in the former year and consistent throughtout all years

files_f_3 <- grep("CARBON|NO-METALICOS", files_f, value = TRUE)

filter_y <- paste0(2004:2019, collapse = "|")

files_f_3 <- grep(filter_y, files_f_3, value = TRUE)


if(exists("data_all_2")) rm(data_all_2)

for (i in files_f_3) {
  
  data <- read_excel(
    path = paste0("./03_intermediate/02_country_specific/peru/raw/", i),
    skip = 1
  )
  
  
  # filter for only data rows (i.e. no footnotes)
  data <- data %>%
    filter(!is.na(UNIDAD))
  
  
  # special case "2005_NO-METALICOS.XLS"
  if(i == "2005_NO-METALICOS.XLS"){
    data <- data %>%
      rename(
        TITULAR = EMPRESA,
        REGIÓN = DEPARTAMENTO,
        ACUMULADO = `CANTIDAD TM EXTRAIDO`
      )
  }
  
  
  # filter for relevant columns
  data <- data %>%
    select("PRODUCTO", "TITULAR", "UNIDAD", "REGIÓN", "PROVINCIA", "DISTRITO", starts_with("ACUM"))
  
  
  # get year and insert year
  year <- i %>% substr(., 1, 4) %>% as.integer
  
  data <- data %>%
    mutate(year = !!year)
  
  
  # get unit
  unit <- read_excel(
    path = paste0("./03_intermediate/02_country_specific/peru/raw/", i)
  )
  
  unit <- names(unit)[1]
  
  unit <- substr(unit, tail(unlist(gregexpr('\\(', unit)) + 1, n=1), tail(unlist(gregexpr('\\)', unit)) - 1, n=1))
  
  # insert unit
  data <- data %>%
    mutate(unit = !!unit)
  
  # special case "2005_NO-METALICOS.XLS"
  if(i == "2005_NO-METALICOS.XLS"){
    data <- data %>%
      mutate(unit = "T.M.")
  }
  
  
  # combine data
  if(exists("data_all_2")) {
    data_all_2 <- data_all_2 %>% bind_rows(., data)
  } else {data_all_2 <- data}
  
}







## for single-commodity files from 2002 to 2005 ----
  ## (because different format after 2005 and before 2002)

files_f_4 <- grep("CARBON|NO-METALICOS", files_f, value = TRUE, invert = TRUE)

filter_y <- paste0(2002:2005, collapse = "|")

files_f_4 <- grep(filter_y, files_f_4, value = TRUE)

# exclude "2005_ORO.xls" (because different format)
files_f_4 <- grep("2005_ORO.xls", files_f_4, value = TRUE, invert = TRUE)


if(exists("data_all_3")) rm(data_all_3)

for (i in files_f_4) {
  
  data <- read_excel(
    path = paste0("./03_intermediate/02_country_specific/peru/raw/", i),
    skip = 3)
  
  # special cases for rows when reading file
  if(i %in% c(
    "2003_CADMIO.XLS",
    "2003_ESTAÑO.XLS",
    "2003_HIERRO.XLS",
    "2003_MOLIBDENO.XLS"
    )){
    data <- read_excel(
      path = paste0("./03_intermediate/02_country_specific/peru/raw/", i),
      skip = 5
    )
  }
  
  if(grepl("2004", i)){
    data <- read_excel(
      path = paste0("./03_intermediate/02_country_specific/peru/raw/", i),
      skip = 5
    )
  }
  
  if(grepl("2005", i)){
    data <- read_excel(
      path = paste0("./03_intermediate/02_country_specific/peru/raw/", i),
      skip = 6
    )
  }
  
  
  # rename
  data <- data %>%
    rename(
      ETAPA = ...1,
      TITULAR = ...2,
      UNIDAD = `UNIDAD MINERA`,
    )
  
  # special cases for "TOTAL"
  if(grepl("2002", i)){
    data <- data %>%
      rename(`TOTAL GENERAL` = `T O T A L`)
    } else {
      data <- data %>%
        rename(`TOTAL GENERAL` = `ENE-DIC`)
    }
  
  
  # fill empty cells
  data <- data %>%
    tidyr::fill(., ETAPA) %>%
    tidyr::fill(., TITULAR)
  
  
  # filter for only data rows (i.e. no footnotes)
  data <- data %>%
    filter(!is.na(UNIDAD))
  
  
  # insert commodity
  data <- data %>%
    mutate(
      PRODUCTO = i %>% gsub(".XLS|.xls", "", .) %>% substr(., 6, nchar(i))
    )
  
  
  # filter for relevant columns
  data <- data %>%
    select("PRODUCTO", "ETAPA", "TITULAR", "UNIDAD", "REGION", "PROVINCIA", "DISTRITO", "TOTAL GENERAL")
  
  
  # get year and insert year
  year <- i %>% substr(., 1, 4) %>% as.integer
  
  data <- data %>%
    mutate(year = !!year)
  
  
  # get unit
  unit <- read_excel(
    path = paste0("./03_intermediate/02_country_specific/peru/raw/", i)
  )
  
  unit <- names(unit)[1]
  
  unit <- substr(unit, tail(unlist(gregexpr('\\(', unit)) + 1, n=1), tail(unlist(gregexpr('\\)', unit)) - 1, n=1))
  
  # insert unit
  data <- data %>%
    mutate(unit = !!unit)
  
  
  # combine data
  if(exists("data_all_3")) {
    data_all_3 <- data_all_3 %>% bind_rows(., data)
  } else {data_all_3 <- data}
  
}







## for single-commodity files for 2001 ----
  ## (because different format than others)

files_f_5 <- grep("CARBON|NO-METALICOS", files_f, value = TRUE, invert = TRUE)

filter_y <- 2001

files_f_5 <- grep(filter_y, files_f_5, value = TRUE)

i <- "2001_COBRE.XLS"

if(exists("data_all_4")) rm(data_all_4)

for (i in files_f_5) {
  
  data <- read_excel(
    path = paste0("./03_intermediate/02_country_specific/peru/raw/", i),
    skip = 4)
  
  
  # rename
  data <- data %>%
    rename(
      ETAPA = ...1,
      TITULAR = `EMPRESA MINERA`,
      UNIDAD = `UNIDAD MINERA`,
      REGION = DEPARTAMENTO,
      `TOTAL GENERAL` = TOTAL...45
    )
  
  
  # remove "... MINERIA" entries from ETAPA (otherwise won't be filled correctly)
  data <- data %>%
    mutate(ETAPA = ifelse(
      grepl("MINERIA", ETAPA),
      NA,
      ETAPA
    ))
  
  
  # fill empty cells
  data <- data %>%
    tidyr::fill(., ETAPA) %>%
    tidyr::fill(., TITULAR)
  
  
  # filter for only data rows (i.e. no footnotes)
  data <- data %>%
    filter(!is.na(UNIDAD))
  
  
  # insert commodity
  data <- data %>%
    mutate(
      PRODUCTO = i %>% gsub(".XLS|.xls", "", .) %>% substr(., 6, nchar(i))
    )
  
  
  # filter for relevant columns
  data <- data %>%
    select("PRODUCTO", "ETAPA", "TITULAR", "UNIDAD", "REGION", "PROVINCIA", "DISTRITO", "TOTAL GENERAL")
  
  
  # get year and insert year
  year <- i %>% substr(., 1, 4) %>% as.integer
  
  data <- data %>%
    mutate(year = !!year)
  
  
  # get unit
  unit <- read_excel(
    path = paste0("./03_intermediate/02_country_specific/peru/raw/", i)
  )
  
  unit <- names(unit)[1]
  
  unit <- substr(unit, tail(unlist(gregexpr('\\(', unit)) + 1, n=1), tail(unlist(gregexpr('\\)', unit)) - 1, n=1))
  
  # insert unit
  data <- data %>%
    mutate(unit = !!unit)
  
  
  # combine data
  if(exists("data_all_4")) {
    data_all_4 <- data_all_4 %>% bind_rows(., data)
  } else {data_all_4 <- data}
  
}








### format all data tables ----

## format data_all ----

# rename
data_all <- data_all %>%
  rename(PRODUCTO = LEY)


# check and remove footnotes from mine names (i.e. UNIDAD)
data_all %>%
  filter(grepl(")", UNIDAD))

data_all <- data_all %>%
  mutate(
    UNIDAD = gsub("  a)|  b)|  c)|  d)|  e)|  f)|  g)|  h)|  i)|  j)| j)|  k)|  l)|  m)", "", UNIDAD)
  )



## format data_all_2 ----

# merge value columns for data_all_2
  # and remove all former value columns
data_all_2 <- data_all_2 %>%
  mutate(`TOTAL GENERAL` = rowSums(across(starts_with("ACUM")), na.rm = TRUE)) %>%
  select(-starts_with("ACUM"))


# insert 'ETAPA'
data_all_2 <- data_all_2 %>%
  mutate(ETAPA = NA) %>%
  mutate(across(where(is.logical), as.character))


# rename
data_all_2 <- data_all_2 %>%
  rename(REGION = REGIÓN)



### combine ----

data_comb <- data_all %>%
  union(., data_all_2) %>%
  union(., data_all_3) %>%
  union(., data_all_4)



### save rds ----
min_y <- data_comb %>% select(year) %>% min()
max_y <- data_comb %>% select(year) %>% max()

write_rds(
  data_comb,
  file = paste0("./03_intermediate/02_country_specific/peru/peru_", min_y, "-", max_y, "_formatted_", substr(Sys.time(),1,10),".rds")
  )






