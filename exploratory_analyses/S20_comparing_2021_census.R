# Comparing migration in 2021 with 2011

# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

#load packages
pacman::p_load(tidyverse, 
               gtsummary,
               janitor,
               rio, 
               ggplot2, 
               stringr, 
               here, 
               aws.s3,
               readr,
               Hmisc)



# Data load ---------------------------------------------------------------

    # data were downloaded from https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/articles/demographyandmigrationdatacontent/2022-11-02
    # saved in R workbench rather than S3 buckets because of access issues on the day of analysis
dta2021 <- import(here::here("2021_census_data", "UR_ltla_migrant_ind.xlsx"))

# Calculate percent inmigration among usual residents
    # NOTE: we are not using midyear population (as with rest of analysis) because we do not have information on outmigrants
    # and therefore cannot calculate 2020 usual residents

dta2021 <- dta2021 %>%
  clean_names() %>%
  select(-migrant_indicator_5_categories_code, -lower_tier_local_authorities_code) 

#reshape to wide
  dta2021 <- stats::reshape(dta2021, idvar = "lower_tier_local_authorities_label", timevar = "migrant_indicator_5_categories_label", direction = "wide")
  dta2021 <- dta2021 %>%
    clean_names()
  names(dta2021)[2:6]<-c('n_na', 'n_samead', 'n_temp' , 'n_inmig_uk','n_inmig_for')
  
    
# calculate % inmigration
  dta2021 <- dta2021 %>%
    mutate(p_inmig_2021 = (n_inmig_uk + n_inmig_for) / (n_na + n_samead + n_temp + n_inmig_uk + n_inmig_for) *100)

  
# Import 2011 data -------------------------------------------------------------
  dta2011 <- s3read_using(FUN = fread,
                      object = 'age_net_migration_LA.csv',
                      bucket = buck_clean)

  # replace name for a few local authorities so they merge correctly
  dta2011 <- dta2011 %>%
    mutate(geography = replace(geography, geography == "Kingston upon Hull, City of", "Kingston upon Hull")) %>%
    mutate(geography = replace(geography, geography == "Herefordshire, County of", "Herefordshire")) %>%
    mutate(geography = replace(geography, geography == "Bristol, City of", "Bristol")) %>%
    mutate(geography = replace(geography, geography == "Rhondda Cynon Taff", "Rhondda Cynon Taf"))

  # merge local authorities which merged between 2011 and 2021
    # NOTE: this dataset was made by looking at which LAs did not merge to the 2021 file, then manually searching the internet for which LAs had merged to form these
  lookup <- import(here::here("2021_census_data", "2021_2011_LA_lookup.xlsx"))
  dta2011 <- left_join(dta2011, lookup, by = c("geography" = "geography_2011"))
  dta2011 <- dta2011 %>%
    mutate(geography_2021 = case_when(
      is.na(geography_2021) ~ geography, 
      TRUE ~ geography_2021))
  
  dta2011 <- dta2011 %>%
    group_by(geography_2021) %>%
    dplyr::mutate(under_34_inmig_m = sum(under_34_inmig), 
           x35_to_64_inmig_m = sum(x35_to_64_inmig),
           x65plus_inmig_m = sum(x65plus_inmig),
           under_34_usualres11_m = sum(under_34_usualres11),
           x35_to_64_usualres11_m = sum(x35_to_64_usualres11), 
           x65plus_usualres11_m = sum(x65plus_usualres11))
  
  # calculate % inmigration
  dta2011 <- dta2011 %>%
    mutate(p_inmig_2011 = (under_34_inmig_m + x35_to_64_inmig_m + x65plus_inmig_m) / (under_34_usualres11_m + x35_to_64_usualres11_m + x65plus_usualres11_m) *100)
  
  
  # keep only name and p_inmig
  dta2011 <- dta2011 %>%
    select(geography_2021, p_inmig_2011)
  
  
  
  
  
# Merge two datasets -----------------------------------------------------------
  dta2021 <- left_join(dta2021, dta2011, by = c("lower_tier_local_authorities_label" = "geography_2021"))
  dta2021 <- dta2021 %>% 
    distinct() 

  
#Scatter plot
  plot(dta2021$p_inmig_2011, dta2021$p_inmig_2021, 
       xlim= c(0, 25),
       ylim = c(0,25))

  # add reference line
  abline(coef=c(0,1))

  # correlation
  dtacorr <- dta2021 %>%
    select(p_inmig_2011, p_inmig_2021)
  corr <- rcorr(as.matrix(dtacorr), type="spearman")
  corr  
  
  
  
  # Identify LAs furthest from 2011 value
  dta2021 <- dta2021 %>%
    mutate(diff=(p_inmig_2021-p_inmig_2011))

    