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
               Hmisc, 
               nomisr)



# Data load ---------------------------------------------------------------

# the following github page was useful for understanding the nomisr package: https://github.com/ropensci/nomisr

# find dataset id
search <- nomis_search(name = "*Migrant*")
tibble::glimpse(search)
View(search)
  # dataset id for migrant indicator is NM_2039_1


# find geography code for all local authority districts
geography <- nomis_get_metadata("NM_2039_1", "geography", "TYPE")
tail(geography)
  # geography code is TYPE154

# import data
dta2021 <- nomis_get_data( id = "NM_2039_1", time = "latest",
                        geography = "TYPE154") 

# Calculate percent inmigration among usual residents
    # NOTE: we are not using midyear population (as with rest of analysis) because we do not have information on outmigrants
    # and therefore cannot calculate 2020 usual residents

dta2021 <- dta2021 %>%
  clean_names() %>%
  select(geography_name, c2021_migind_4_name, obs_value) 

#reshape to wide
  dta2021 <- dta2021 %>%
    pivot_wider(names_from = "c2021_migind_4_name", values_from = "obs_value")
  dta2021 <- dta2021 %>%
    clean_names()
  names(dta2021)[2:6]<-c('usual_res', 'n_samead', 'n_temp' , 'n_inmig_uk','n_inmig_for')
  
    
# calculate % inmigration
  dta2021 <- dta2021 %>%
    mutate(p_inmig_2021 = (n_inmig_uk + n_inmig_for) / usual_res *100)


  
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
    # saved on our github page https://github.com/HFAnalyticsLab/mobility_scoping/blob/main/2021_census_data/2021_2011_LA_lookup.xlsx
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
    mutate(usualres_2011 = under_34_usualres11_m + x35_to_64_usualres11_m + x65plus_usualres11_m,
      p_inmig_2011 = (under_34_inmig_m + x35_to_64_inmig_m + x65plus_inmig_m) / (usualres_2011) *100)
  
  
  # keep only name and p_inmig
  dta2011 <- dta2011 %>%
    select(geography_2021, p_inmig_2011, usualres_2011)
  
  
  
  
  
# Merge two datasets -----------------------------------------------------------
  dta2021 <- left_join(dta2021, dta2011, by = c("geography_name" = "geography_2021"))
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

  
  
  # Check correlation between usual residents
  
  #Scatter plot
  plot(dta2021$usualres_2011, dta2021$usual_res,
       xlim= c(0, 1200000),
       ylim = c(0,1200000))
  
  # add reference line
  abline(coef=c(0,1))
  
  # correlation
  dtacorr <- dta2021 %>%
    select(usualres_2011, usual_res)
  corr <- rcorr(as.matrix(dtacorr), type="spearman")
  corr  
  