# CDRC by LA compare with net migration in 2010-11  --------------------------------------------------------------

# Housekeeping
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

#Accessed from: https://data.cdrc.ac.uk/dataset/cdrc-residential-mobility-index
cdrc_lad<-s3read_using(read_csv # Which function are we using to read
                       , object = 'CDRC_residential_mobility_index_LAD.csv' # File to open
                       , bucket = buck_data) # Bucket name defined above

#have churn of every year from 1997-2019 compared to 2020
summary(cdrc_lad)


#drop 2020 (no data) and only include England and Wales code
cdrc_lad_clean<-cdrc_lad %>% 
  dplyr::select(area,chn2019, chn2011, chn2010, chn2015) %>% 
  dplyr::filter(., grepl("E|W", area))


summary(cdrc_lad_clean)


#summary of data

cdrc_plot<-cdrc_lad_clean %>% 
  dplyr::select(chn2019:chn2010) %>% 
  pivot_longer(everything(), names_to="year", values_to="RMIx2020")


t<-boxplot(RMIx2020~year,data=cdrc_plot, main="Net Migration by Year",
           xlab="Year", ylab="Net Migration")



# Migration data ----------------------------------------------------------

#data were downloaded from https://www.nomisweb.co.uk/census/2011/ukmig008 
    # download -> local authority: district/unitary (prior to April 2015)
eng_dta <- s3read_using(import # Which function are we using to read
                        , object = 'la_migration.csv' # File to open
                        , bucket = buck_data) # Bucket name defined above

eng_dta<-eng_dta %>% 
  clean_names()


# naming unneeded colums x, y, z
names(eng_dta)[4:10]<-c('n_usualres11', 'n_samead', 'x' , 'n_movedwithin','n_inmiguk','n_inmigfor','n_outmig')

# usual residents one year before census
eng_dta <- eng_dta %>%
  mutate(n_usualres10 = n_samead + n_outmig + n_movedwithin)

# usual residents - midyear
eng_dta <- eng_dta %>% 
  mutate(midyrpop = (n_usualres10 + n_usualres11)/2)

# total inmigrants
eng_dta <- eng_dta %>%
  mutate(n_inmig = n_inmiguk + n_inmigfor)


#drop columns don't need
eng_dta <- eng_dta %>% 
  dplyr::select(1:3, midyrpop, !contains("migration")) 

head(eng_dta)

class(eng_dta$n_samead)

#calculate net migration

eng_dta<-eng_dta %>% 
  mutate(netmigration=(n_inmig-n_outmig)/(midyrpop)*100)


#only keep england and wales 
eng_dta<-eng_dta %>% 
  dplyr::filter(., grepl("E|W", geography_code))

#summary of net migration

t<-boxplot(eng_dta$netmigration,data=eng_dta, main="Net Migration",
           xlab="", ylab="Net Migration")

#save net_migration data

#save data

eng_dta<-eng_dta %>% 
  mutate(netmigration_lab=case_when(netmigration < -1 ~ "Negative (greater than 1% of people leaving)",
                                    netmigration > 1 ~ "Positive (greater than 1% of people moving in)", 
                                    TRUE ~ "stable migration (~1% of population moving in and out)"))


s3write_using(eng_dta # What R object we are saving
              , FUN = write.csv # Which R function we are using to save
              , object = 'net_migration_LAD.csv' # Name of the file to save to (include file type)
              , bucket = buck_clean) # Bucket name defined above




# Merging CDRC with census data -------------------------------------------



cdrc_eng<-eng_dta %>% 
  dplyr::select(1:3, netmigration) %>% 
  full_join(cdrc_lad_clean, by=c("geography_code"="area"))
# %>% 
#   dplyr::filter(., !grepl("EE|WW", geography_code))

#Correlation for net migration and RMI index

cdrc_eng_clean<-cdrc_eng %>% 
  drop_na() %>% 
  dplyr::select(-c("date", "geography", "geography_code"))

corr <- rcorr(as.matrix(cdrc_eng_clean), type="spearman")
corr

#net migration in 2010-11 is strongly correlated with change in RMI index from 2010 and 2011 compared to 2020 
#change in RM  in 2019 is assoc with change in RM in 2011 and 2010 but weakly associated with net migration 



# validating against new census data --------------------------------------
#Data downloaded from: https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/bulletins/populationandhouseholdestimatesenglandandwales/census2021
 #> Figure 3: Population change between 2011 and 2021, local authorities in England and Wales
new_census <- s3read_using(import # Which function are we using to read
                           , object = 'pop_change.csv' # File to open
                           , bucket = buck_clean) # Bucket name defined above

new_census<-new_census %>% 
  janitor::row_to_names(.,3,remove_rows_above = TRUE) %>% 
  clean_names() %>% 
  dplyr::select(la_code:percentage_change) %>% 
  dplyr::filter(., grepl("E|W", la_code)) %>% 
  mutate_all(gsub, pattern=",",replacement="") %>% 
  mutate(percentage_change=as.numeric(percentage_change),
         usual_resident_population_2011=as.numeric(usual_resident_population_2011),
         usual_resident_population_2021 =as.numeric(usual_resident_population_2021))


old_census<-eng_dta %>% 
  dplyr::select(geography_code, n_usualres11, netmigration)

census<-new_census %>% 
  full_join(old_census, by=c("la_code"="geography_code")) %>% 
  dplyr::filter(., !grepl("EE|WW", la_code)) %>% 
  mutate(percent_change_check=((usual_resident_population_2021-n_usualres11)
                               /((n_usualres11+usual_resident_population_2021)/2)*100))

#proportion that didn't match
summary(census)

#25 areas don't have net migration and about 8 areas are not in the new census 

#correlations for those without NA

census_corr<-census %>% 
  drop_na() %>% 
  select(contains(c("change", "migration")))

corr <- rcorr(as.matrix(census_corr), type="spearman")
corr


#corelating new census with cdrc



census_cdrc_corr<-census %>% 
  full_join(cdrc_lad_clean, by=c("la_code"="area"))


summary(census_cdrc_corr)

#20 didn't merge 

census_cdrc_corr<-census_cdrc_corr %>% 
  select(contains(c("change", "migration", "chn"))) %>% 
  drop_na()


corr <- rcorr(as.matrix(census_cdrc_corr), type="spearman")
corr





# Validating against 2011 cdrc data ---------------------------------------

#Requested from https://data.cdrc.ac.uk/dataset/cdrc-residential-mobility-index 
cdrc_lad_2011<-s3read_using(read_csv # Which function are we using to read
                       , object = 'CDRC_LAD_2011.csv' # File to open
                       , bucket = buck_data) # Bucket name defined above


#have churn of every year from 1997-2019 compared to 2011
summary(cdrc_lad_2011)


cdrc_lad_clean<-cdrc_lad_2011 %>% 
  filter(Year %in% c("2010", "2020")) %>% 
  dplyr::select(LAD20name,Year, RMI) %>% 
  mutate(Year=paste0("chn",Year)) %>% 
  mutate(RMI=1-as.numeric(RMI))
  
#summary of data

t<-boxplot(RMI~Year,data=cdrc_lad_clean, main="RMI by Year",
           xlab="Year", ylab="RMI")

t 

cdrc_lad_clean %>% 
  select(-LAD20name) %>% 
  tbl_summary(, by=Year, digits=everything()~3) #default is median (25%,75%) 



cdrc_eng<-eng_dta %>% 
  dplyr::select(1:3, netmigration) %>% 
  full_join(cdrc_lad_clean, by=c("geography"="LAD20name")) %>% 
 dplyr::filter(., !grepl("EE|WW", geography_code))

#Correlation for net migration and RMI index

cdrc_eng_clean<-cdrc_eng %>% 
  drop_na() %>% 
  dplyr::select(-c("date", "geography", "geography_code"))

corr <- rcorr(as.matrix(cdrc_eng_clean), type="spearman")
corr









