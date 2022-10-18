## Calculating net migration at upper-tier local authority level

# Housekeeping
# clear R environment
rm(list = ls())

# run script with bucket names
source("0_file_pathways.R") 

#load packages
pacman::p_load(dplyr,
               sf,
               XML,
               tmap,
               viridis, 
               wesanderson,
               aws.s3,
               tidyverse,
               rio,
               janitor,
               ggmap)


# import data
netmig_dta <- s3read_using(import, 
                       object = 'net_migration_LAD.csv', 
                       bucket = buck_clean) 

dim(netmig_dta)      #16 variables, 360 observations

# import lookup table
    # Data source: https://geoportal.statistics.gov.uk/datasets/lower-tier-local-authority-to-upper-tier-local-authority-december-2016-lookup-in-england-and-wales/explore
lookup <- s3read_using(import, 
                       object = 'LTLA_UTLA_lookup_Dec2016.csv', 
                       bucket = buck_data) # File to open 

# merge data
netmig_dta <- left_join(netmig_dta, lookup, by = c("geography_code" = "LTLA16CD"))

tabyl(netmig_dta$UTLA16NM)
  # only 2 missing, these are Irish LAs that should have been deleted

netmig_dta <- netmig_dta %>%
  subset(str_detect(geography_code, '^E') | str_detect(geography_code, '^W'))

tabyl(netmig_dta$UTLA16NM)
  # no missing


# calculate numbers in each category by UTLA
netmig_dta <- netmig_dta %>%
  group_by(UTLA16CD) %>%
  mutate(n_samead_lad = sum(n_samead),
         n_usualres_lad11 = sum(n_usualres11),
         n_movedwithin_lad = sum(n_movedwithin),
         n_inmiguk_lad = sum(n_inmiguk),
         n_inmigfor_lad = sum(n_inmigfor),
         n_outmig_lad = sum(n_outmig), 
         n_usualres_lad10 = sum(n_usualres10),
         midyrpop_lad = sum(midyrpop),
         n_inmig_lad = sum(n_inmig))

tabyl(netmig_dta$n_samead_lad)


#calculate net migration in UTLA
netmig_dta <- netmig_dta %>%
  mutate(netmigration_lad = (n_inmig_lad - n_outmig_lad)/midyrpop_lad *100)


# keep only UTLA columns
netmig_dta <- netmig_dta %>%
  dplyr::select('UTLA16CD', 'UTLA16NM', 'netmigration_lad', 'midyrpop_lad', 'n_inmig_lad', 'n_outmig_lad')

dim(netmig_dta)

unique(netmig_dta$UTLA16CD)

# keep unique rows
netmig_dta <- netmig_dta %>%
  dplyr::distinct()

dim(netmig_dta)
  # this grouped e.g. inner and outer London, not sure if what we want

# save data
s3write_using(netmig_dta # What R object we are saving
              , FUN = write_csv # Which R function we are using to save
              , object = 'net_migration_utla.csv' # Name of the file to save to (include file type)
              , bucket = buck_clean) # Bucket name defined above



