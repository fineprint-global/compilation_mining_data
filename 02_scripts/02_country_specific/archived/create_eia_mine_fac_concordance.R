###### create concordance table for mine names/ids



library(tidyverse)

# clear R environment
rm(list = ls())




### read files

## data

# get the latest files (general info and data)
source("./02_scripts/00_functions/lastfile.R")

lastfile <- my_lastfile("./03_intermediate/02_country_specific/eia/", "general_raw")

eia <- read_rds(lastfile)

detailed <- read_rds("./03_intermediate/01_detailed_data/gaps_filled.rds")

lastfile <- my_lastfile("./03_intermediate/02_country_specific/eia/", "values")

eia_values <- read_rds(lastfile)





### clean eia ----

# clean mine_fac
eia <- eia %>%
  mutate(mine_fac = gsub("\\(.*)", "", mine_fac)) %>%
  mutate(mine_fac = str_trim(mine_fac, side = c("both")))

# remove leading and trailing white spaces from various variables
eia <- eia %>%
  mutate(type_production = str_trim(type_production, side = c("both"))) %>%
  mutate(type_coal = str_trim(type_coal, side = c("both"))) %>%
  mutate(type_mining = str_trim(type_mining, side = c("both"))) %>%
  mutate(frequency = str_trim(frequency, side = c("both")))

# remove obsolete columns
eia <- eia %>%
  select(-type_production, -frequency, -f, -copyright)




### mine_fac ----

# eia
mine_fac_eia <- eia %>%
  distinct(mine_fac)

# detailed filtered for USA
mine_fac_det <- detailed$general %>%
  filter(alphanumiso == "USA840") %>%
  distinct(mine_fac, mine_other_names, alphanumiso)



## get those which are completely equal
equal <- intersect(
  mine_fac_det %>%
    select(mine_fac),
  mine_fac_eia
  )


## remove equal names from mine lists
mine_fac_eia_fil <- mine_fac_eia %>%
  filter(!mine_fac %in% equal$mine_fac)

mine_fac_det_fil <- mine_fac_det %>%
  filter(!mine_fac %in% equal$mine_fac) %>%
  select(-alphanumiso)


## table for all_matches
all_matches <- equal %>%
  mutate(match = mine_fac) %>%
  mutate(n = 1)


# get all close matches for mine_fac
for (i in mine_fac_det_fil$mine_fac) {
  
  matches <- agrep(
    i,
    mine_fac_eia_fil$mine_fac,
    value = TRUE
  )
  
  # table with indication whether there is more than 1 match
  matches <- tibble(mine_fac = i, match = matches, n = length(matches))
  
  
  if(exists("all_matches")) {
    all_matches <- all_matches %>% bind_rows(., matches)
  } else {all_matches <- matches}
  
}





### mine_other_names ----

## table for all_matches_2
all_matches_2 <- all_matches %>%
  filter(mine_fac == "Give me an empty tibble! :-) ") %>%
  rename(mine_other_names = mine_fac)


name_list <- mine_fac_det_fil %>%
  select(mine_other_names) %>%
  filter(!is.na(mine_other_names)) %>%
  pull()


# get all close matches for "mine_other_names"
for (i in name_list) {
  
  matches <- agrep(
    i,
    mine_fac_eia_fil$mine_fac,
    value = TRUE
  )
  
  # table with indication whether there is more than 1 match
  matches <- tibble(mine_other_names = i, match = matches, n = length(matches))
  
  
  if(exists("all_matches_2")) {
    all_matches_2 <- all_matches_2 %>% bind_rows(., matches)
  } else {all_matches_2 <- matches}
  
}


# include mine_fac for all_matches_2
all_matches_2 <- all_matches_2 %>%
  left_join(., mine_fac_det_fil)


# combine matches for mine_fac and for mine_other_names
combined <- all_matches %>%
  left_join(., mine_fac_det_fil) %>%
  full_join(.,
            all_matches_2,
            by = c("mine_fac", "mine_other_names"),
            suffix = c("", ".other_names")
            ) %>%
  arrange(mine_fac)
  





### sub-sites ----

# distinct sub-sites filtered for USA
sub_sites <- mine_fac_det_fil %>%
  select(-mine_other_names) %>%
  left_join(.,
            detailed$sub_sites %>%
              distinct(mine_fac, sub_site)
            ) %>%
  distinct(mine_fac, sub_site) %>%
  filter(!is.na(sub_site))


## table for all_matches_3
all_matches_3 <- all_matches %>%
  filter(mine_fac == "Give me an empty tibble! :-) ") %>%
  rename(sub_site = mine_fac)


name_list <- sub_sites %>%
  select(sub_site) %>%
  filter(!is.na(sub_site)) %>%
  pull()


# get all close matches for "sub_sites"
for (i in name_list) {
  
  matches <- agrep(
    i,
    mine_fac_eia_fil$mine_fac,
    value = TRUE
  )
  
  # table with indication whether there is more than 1 match
  matches <- tibble(sub_site = i, match = matches, n = length(matches))
  
  
  if(exists("all_matches_3")) {
    all_matches_3 <- all_matches_3 %>% bind_rows(., matches)
  } else {all_matches_3 <- matches}
  
}


# include mine_fac for all_matches_3
all_matches_3 <- all_matches_3 %>%
  left_join(., sub_sites)


# combine matches for mine_fac/mine_other_names and for sub-sites
combined <- combined %>%
  full_join(.,
            all_matches_3,
            by = c("mine_fac"),
            suffix = c("", ".sub_site")
            ) %>%
  arrange(mine_fac)



### save table
write_delim(
  combined,
  "./01_input/02_lists_and_concordance_tables/02_country_specific/eia_mine_fac_ids.csv",
  delim = ";",
  na = ""
  )















## checking data ----

# combine eia tables
eia_all <- eia_values %>%
  left_join(., eia)

view(detailed$general)

view(detailed$sub_sites)


# ##### checking different approaches for partially matching strings ----
# 
# agrep(
#   "Vindex",
#   mine_fac_eia$mine_fac,
#   value = TRUE,
#   ignore.case = FALSE
# )
# 
# agrep(
#   "Eagle",
#   mine_fac_det$mine_fac,
#   value = TRUE,
#   ignore.case = TRUE
# )
# 
# 
# 
# a3 <- fuzzyjoin::stringdist_inner_join(
#   mine_fac_det,
#   mine_fac_eia,
#   by = "mine_fac",
#   method = "lv"
#   )
# 
# 
# nrow(
#   merge(
#     mine_fac_det,
#     mine_fac_eia,
#     x.by="mine_fac",
#     y.by="mine_fac")
#   )
# 
# 
# a2 <- mine_fac_det$mine_fac[!is.na(pmatch(mine_fac_det$mine_fac, mine_fac_eia$mine_fac))] %>% 
#   as_tibble()
# 
# 
